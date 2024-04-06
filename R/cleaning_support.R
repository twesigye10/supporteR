#' Cleaning support
#' Uses packages like kobold and butteR
#'
#' @param input_df_raw_data Specify the data frame for the tool data
#' @param input_df_survey Specify the data frame for the survey sheet
#' @param input_df_choices Specify the data frame for the choices sheet
#' @param input_df_cleaning_log Specify the data frame for the cleaning/checking log
#' @param input_vars_to_remove_from_data Specify a vector of columns to blank. This is especially to handle PII columns in the dataset.
#' @param input_geopoint_col Specify column name for the geopoint
#'
#' @return Data frame of cleaned data
#' @export
#'
#' @examples
#'
cleaning_support <- function(input_df_raw_data,
                             input_df_survey,
                             input_df_choices,
                             input_df_cleaning_log,
                             input_vars_to_remove_from_data = c("deviceid",
                                                                 "audit",
                                                                 "audit_URL",
                                                                 "instance_name",
                                                                 "complainant_name",
                                                                 "complainant_id",
                                                                 "respondent_telephone",
                                                                 "name_pers_recording"),
                             input_geopoint_col = "geopoint") {

  # find all new choices to add to choices sheet

  # gather choice options based on unique choices list
  df_grouped_choices<- input_df_choices %>%
    group_by(list_name) %>%
    summarise(choice_options = paste(name, collapse = " : "))

  # get new name and choice pairs to add to the choices sheet
  new_vars <- input_df_cleaning_log %>%
    filter(type %in% c("change_response", "add_option")) %>%
    left_join(input_df_survey, by = "name") %>%
    filter(str_detect(string = type.y, pattern = "select_one|select one|select_multiple|select multiple")) %>%
    separate(col = type.y, into = c("select_type", "list_name"), sep =" ", remove = TRUE, extra = "drop") %>%
    left_join(df_grouped_choices, by = "list_name") %>%
    filter(!str_detect(string = choice_options, pattern = value ) ) %>%
    dplyr::rename(choice = value ) %>%
    select(name, choice) %>%
    distinct() %>% # to make sure there are no duplicates
    arrange(name)

  # create kobold object

  kbo <- kobold::kobold(survey = input_df_survey,
                        choices = input_df_choices,
                        data = input_df_raw_data,
                        cleaning = input_df_cleaning_log)

  # modified choices for the survey tool
  df_choises_modified <- butteR:::xlsform_add_choices(kobold = kbo, new_choices = new_vars)

  # special treat for variables for select_multiple, we need to add the columns to the data itself
  df_survey_sm <- input_df_survey %>%
    mutate(q_type = case_when(str_detect(string = type, pattern = "select_multiple|select multiple") ~ "sm",
                              str_detect(string = type, pattern = "select_one|select one") ~ "so",
                              TRUE ~ type)) %>%
    select(name, q_type)

  # construct new columns for select multiple
  new_vars_sm <- new_vars %>%
    left_join(df_survey_sm, by = "name") %>%
    filter(q_type == "sm") %>%
    mutate(new_cols = paste0(name,"/",choice))

  # add new columns to the raw data
  df_raw_data_modified <- input_df_raw_data %>%
    butteR:::mutate_batch(nm = new_vars_sm$new_cols, value = F )

  # make some cleanup
  kbo_modified <- kobold::kobold(survey = input_df_survey %>% filter(name %in% colnames(df_raw_data_modified)),
                                 choices = df_choises_modified,
                                 data = df_raw_data_modified,
                                 cleaning = input_df_cleaning_log)
  kbo_cleaned <- kobold::kobold_cleaner(kbo_modified)

  # handling Personally Identifiable Information(PII)
  # update columns to remove
  latitude_col <- paste0("_", input_geopoint_col, "_latitude")
  longitude_col <- paste0("_", input_geopoint_col, "_longitude")
  altitude_col <- paste0("_", input_geopoint_col, "_altitude")
  precision_col <- paste0("_", input_geopoint_col, "_precision")

  vars_to_remove_from_data <- c(input_vars_to_remove_from_data,
                                input_geopoint_col, latitude_col, longitude_col,
                                altitude_col, precision_col)
  df_handle_pii <- kbo_cleaned$data %>%
    mutate(across(any_of(vars_to_remove_from_data), .fns = ~na_if(., .)))

  # handling added responses after starting data collection and added responses in the cleaning process

  sm_colnames <-  df_handle_pii %>%
    select(contains("/")) %>%
    colnames() %>%
    str_replace_all(pattern = "/.+", replacement = "") %>%
    unique()

  df_handle_sm_data <- df_handle_pii

  for (cur_sm_col in sm_colnames) {
    df_updated_data <- df_handle_sm_data %>%
      mutate(
        across(contains(paste0(cur_sm_col, "/")), .fns = ~ifelse(!is.na(!!sym(cur_sm_col)) & is.na(.) , FALSE, .)),
        across(contains(paste0(cur_sm_col, "/")), .fns = ~ifelse(is.na(!!sym(cur_sm_col)), NA, .))
      )
    df_handle_sm_data <- df_updated_data
  }

  df_final_cleaned_data <- df_handle_sm_data
}


#' Add new select multiple choices to the data
#' This function adds new choices to be added on sm questions after data collection
#'
#' @param input_df_tool_data Specify the data frame for the raw data
#' @param input_df_filled_cl Specify the data frame for the filled cleaning log
#' @param input_df_survey Specify the data frame for the survey sheet
#' @param input_df_choices Specify the data frame for the choices sheet
#' @param input_sm_seperator The seperator for select multiple
#'
#' @return An updated data frame of the data with added columns for new choices
#' @export
#'
#' @examples
cts_add_new_sm_choices_to_data <- function(input_df_tool_data, input_df_filled_cl, input_df_survey, input_df_choices, input_sm_seperator = "/") {
  # gather choice options based on unique choices list
  df_grouped_choices<- input_df_choices %>%
    group_by(list_name) %>%
    summarise(choice_options = paste(name, collapse = " : "))
  # regexes
  sm_question_regex <- paste0("\\w+\\",input_sm_seperator,"+\\w+")
  sm_int_choice_regex <- paste0("\\w+\\",input_sm_seperator)
  sm_int_question_regex <- paste0("\\",input_sm_seperator,"+\\w+")

  # get new name and choice pairs to add to the choices sheet
  new_vars_sm <- input_df_filled_cl %>%
    filter(str_detect(string = question, pattern = sm_question_regex)) %>%
    filter(!str_detect(string = question, pattern = "other$"), change_type %in% c("change_response")) %>%
    mutate(int.new_value = str_replace_all(string = question, pattern = sm_int_choice_regex, replacement = ""),
           int.question = str_replace_all(string = question, pattern = sm_int_question_regex, replacement = "")) %>%
    left_join(input_df_survey, by = c("int.question" = "name")) %>%
    filter(str_detect(string = type, pattern = "select_one|select one|select_multiple|select multiple")) %>%
    separate(col = type, into = c("select_type", "list_name"), sep =" ", remove = TRUE, extra = "drop") %>%
    left_join(df_grouped_choices, by = "list_name") %>%
    filter(!str_detect(string = choice_options, pattern = int.new_value)) %>%
    select(question) %>%
    group_by(question) %>%
    summarise(n = n())

  # handle when a question had not been answered
  df_add_columns_to_data <- input_df_tool_data %>%
    butteR:::mutate_batch(nm = new_vars_sm$question, value = NA_character_ ) # %>%

  # parent questions for select multiple
  col_changes_parent_vars_sm <- new_vars_sm %>%
    mutate(question = str_replace_all(string = question, pattern = paste0(input_sm_seperator,".+"), replacement = "")) %>%
    pull(question) %>%
    unique()

  df_handle_sm_data <- df_add_columns_to_data

  for (cur_sm_col in col_changes_parent_vars_sm) {
    df_updated_data <- df_handle_sm_data %>%
      mutate(
        across(contains(paste0(cur_sm_col, input_sm_seperator)), .fns = ~ifelse(!is.na(!!sym(cur_sm_col)) & is.na(.) , 0, .)),
        across(contains(paste0(cur_sm_col, input_sm_seperator)), .fns = ~ifelse(is.na(!!sym(cur_sm_col)), NA_integer_, .))
      )
    df_handle_sm_data <- df_updated_data
  }

  df_data_with_added_cols <- df_handle_sm_data
}


#' Update select multiple parent columns
#' This function updates select multiple parent columns trying to keep the original selection order
#'
#' @param input_df_cleaning_step_data The output data frame from the cleaning step
#' @param input_sm_seperator The seperator for select multiple
#' @param input_uuid_col Specify the uuid column
#' @param input_enumerator_id_col Specify the enumerator id column
#' @param input_point_id_col Specify the point id column
#' @param input_collected_date_col Specify the column for the date of data collection
#' @param input_location_col Specify the location description column
#'
#' @return A list that contains an updated data and the extra log to add to the original log used for cleaning
#' @export
#'
#' @examples
cts_update_sm_parent_cols <- function(input_df_cleaning_step_data,
                                      input_sm_seperator = "/",
                                      input_uuid_col = "uuid",
                                      input_enumerator_id_col = "enumerator_id",
                                      input_point_id_col,
                                      input_collected_date_col,
                                      input_location_col) {

  # check existance of sm columns
  if(!str_detect(string = paste(colnames(input_df_cleaning_step_data), collapse = " "), pattern = paste0("\\",input_sm_seperator))){
    stop("check that there are select multiple columns and the sm seperator ")
  }

  # parent column names
  sm_parent_cols <- input_df_cleaning_step_data %>%
    select(contains("/")) %>%
    colnames() %>%
    str_replace_all(pattern = "’", replacement = "") %>%
    str_replace_all(pattern = "\\/+\\w+", replacement = "") %>%
    unique()

  # update the sm parent columns using changes made during cleaning ---------

  # initialise data to be updated
  df_handle_parent_qn_data <- input_df_cleaning_step_data

  for (cur_parent_sm_col in sm_parent_cols) {
    # test
    print(cur_parent_sm_col)

    df_updated_parent_qn_data <- df_handle_parent_qn_data %>%
      mutate(across(.cols = starts_with(paste0(cur_parent_sm_col, "/")),
                    .fns = ~ifelse(.x %in% c(1, "1") & !str_detect(string = !!sym(cur_parent_sm_col), pattern = paste0("\\s?\\b", str_replace_all(string = cur_column(), pattern = paste0(cur_parent_sm_col, "/"), replacement = ""), "\\b\\s?",recycle0 = TRUE)),
                                   str_replace_all(string = cur_column(), pattern = paste0(cur_parent_sm_col, "/"), replacement = ""),
                                   NA_character_),
                    .names = "check.extra.{.col}"),
             across(.cols = starts_with(paste0(cur_parent_sm_col, "/")),
                    .fns = ~ifelse(.x %in% c(0, "0") & str_detect(string = !!sym(cur_parent_sm_col), pattern = paste0("\\s?\\b", str_replace_all(string = cur_column(), pattern = paste0(cur_parent_sm_col, "/"), replacement = ""), "\\b\\s?",recycle0 = TRUE)),
                                   str_replace_all(string = cur_column(), pattern = paste0(cur_parent_sm_col, "/"), replacement = ""),
                                   NA_character_),
                    .names = "check.removed.{.col}")
      ) %>%
      unite(!!paste0("check.extra.", cur_parent_sm_col), starts_with(glue::glue("check.extra.{cur_parent_sm_col}/")), remove = FALSE, na.rm = TRUE, sep = " ") %>%
      unite(!!paste0("check.removed.", cur_parent_sm_col), starts_with(glue::glue("check.removed.{cur_parent_sm_col}/")), remove = FALSE, na.rm = TRUE, sep = " ") %>%
      mutate(!!paste0("check.old.", cur_parent_sm_col) := !!sym(cur_parent_sm_col),
             !!paste0("check.reginit.", cur_parent_sm_col) := ifelse(!(is.na(!!sym(paste0("check.removed.", cur_parent_sm_col)))|!!sym(paste0("check.removed.", cur_parent_sm_col)) %in% c("NA", "")), gsub(pattern = " ", replacement = paste0("\\b|\\b",recycle0 = TRUE), fixed = TRUE, x = !!sym(paste0("check.removed.", cur_parent_sm_col))), NA_character_),
             !!paste0("check.regstart.", cur_parent_sm_col) := ifelse(!(is.na(!!sym(paste0("check.removed.", cur_parent_sm_col)))|!!sym(paste0("check.removed.", cur_parent_sm_col)) %in% c("NA", "")), "\\b", NA_character_),
             !!paste0("check.regend.", cur_parent_sm_col) := ifelse(!(is.na(!!sym(paste0("check.removed.", cur_parent_sm_col)))|!!sym(paste0("check.removed.", cur_parent_sm_col)) %in% c("NA", "")), "\\b", NA_character_),
      ) %>%
      unite(!!paste0("check.reg.", cur_parent_sm_col), c(!!paste0("check.regstart.", cur_parent_sm_col), !!paste0("check.reginit.", cur_parent_sm_col), !!paste0("check.regend.", cur_parent_sm_col)), remove = FALSE, na.rm = TRUE, sep = "") %>%
      mutate(!!paste0("check.remaining.", cur_parent_sm_col) := ifelse(!(is.na(!!sym(paste0("check.removed.", cur_parent_sm_col))) | !!sym(paste0("check.removed.", cur_parent_sm_col)) %in% c("NA", "")), str_replace_all(string = !!sym(cur_parent_sm_col), pattern = regex(!!sym(paste0("check.reg.", cur_parent_sm_col)), comments = TRUE), replacement = ""), !!sym(cur_parent_sm_col)),
             !!paste0("check.remaining.", cur_parent_sm_col) := ifelse(str_detect(string = !!sym(paste0("check.remaining.", cur_parent_sm_col)), pattern = "\\s{2,}"), str_replace_all(string = !!sym(paste0("check.remaining.", cur_parent_sm_col)), pattern = regex("\\s{2,}"), replacement = " "), !!sym(paste0("check.remaining.", cur_parent_sm_col)))
      ) %>%
      unite(!!paste0("check.final.", cur_parent_sm_col), c(!!paste0("check.remaining.", cur_parent_sm_col), !!paste0("check.extra.", cur_parent_sm_col)), remove = FALSE, na.rm = TRUE, sep = " ") %>%
      mutate(!!cur_parent_sm_col := str_trim(!!sym(paste0("check.final.", cur_parent_sm_col))))

    df_handle_parent_qn_data <- df_updated_parent_qn_data
  }

  # extract updated data
  df_updated_parent_cols <- df_handle_parent_qn_data

  # generate extra log ------------------------------------------------------

  df_log_parent_sm_cols_changes <- purrr::map_dfr(.x = sm_parent_cols,
                                                  .f = ~ {df_updated_parent_cols %>%
                                                      dplyr::filter(!!sym(paste0("check.old.",.x)) != !!sym(.x)) %>%
                                                      dplyr::mutate(i.check.uuid = `_uuid`,
                                                                    i.check.enumerator_id = enumerator_id,
                                                                    i.check.point_number = point_number,
                                                                    i.check.today = today,
                                                                    !!paste0("i.check.", input_location_col) := !!sym(input_location_col),
                                                                    i.check.change_type = "change_response",
                                                                    i.check.question = .x,
                                                                    i.check.old_value = as.character(!!sym(paste0("check.old.",.x))),
                                                                    i.check.new_value = as.character(!!sym(.x)),
                                                                    i.check.issue = "changed parent sm column",
                                                                    i.check.description = "Parent column changed to match children columns",
                                                                    i.check.other_text = "",
                                                                    i.check.comment = "",
                                                                    i.check.reviewed = "1",
                                                                    i.check.so_sm_choices = "") %>%
                                                      dplyr::select(starts_with("i.check."))}) %>%
    supporteR::batch_select_rename()

  updated_sm_parent_data <- list("updated_sm_parents" = df_updated_parent_cols %>%
                                   select(-matches("^check.")),
                                 "extra_log_sm_parents" = df_log_parent_sm_cols_changes)

}
