#' Extract other specify data
#'
#' @param input_tool_data Specify the data frame for the tool data
#' @param input_point_id_col Specify the column for the point_id
#' @param input_location_col Specify the column for location description in the dataset
#' @param input_survey Specify the data frame for the survey sheet
#' @param input_choices Specify the data frame for the choices sheet
#'
#' @return
#' @export
#'
#' @examples
#' extract_other_specify_data(input_repeat_data = df_tool_data_protection_risky_places,
#'                            input_point_id_col = point_number,
#'                            input_location_col = district_name,
#'                            input_survey = df_survey,
#'                            input_choices = df_choices)
#'
extract_other_specify_data <- function(input_tool_data, input_point_id_col = "point_number", input_location_col = "district_name", input_survey, input_choices) {

  # add and rename some columns
  df_data <- input_tool_data %>%
    rename(uuid = `_uuid`) %>%
    mutate(start_date = as_date(start),
           enumerator_id = as.character(enumerator_id))

  # get questions with other
  others_colnames <-  df_data %>%
    select(ends_with("_other"), -contains("/")) %>%
    colnames()

  # data.frame for holding _other response data
  df_other_response_data <- purrr::map_dfr(.x = others_colnames,
                                    .f = ~{
                                      df_data %>%
                                        select(-contains("/")) %>%
                                        select(uuid, start_date, enumerator_id, sym(input_location_col) , sym(input_point_id_col), other_text = as.character(.x), current_value = str_replace_all(string = .x, pattern = "_other$", replacement = "")) %>%
                                        filter(!is.na(other_text), !other_text %in% c(" ", "NA")) %>%
                                        mutate(other_name = .x,
                                               int.my_current_val_extract = ifelse(str_detect(current_value, "\\bother\\b"), str_extract_all(string = current_value, pattern = "\\bother\\b|\\w+_other\\b"), current_value),
                                               value = "",
                                               parent_qn = str_replace_all(string = .x, pattern = "_other$", replacement = ""),
                                               across(.cols = !contains("date"), .fns = as.character))
                                    })

  # arrange the data
  df_data_arranged <- df_other_response_data %>%
    arrange(start_date, uuid)

  # get choices to add to the _other responses extracted
  df_grouped_choices <- input_choices %>%
    group_by(list_name) %>%
    summarise(choice_options = paste(name, collapse = " : ")) %>%
    arrange(list_name)

  # extract parent question and join survey for extracting list_name
  df_data_parent_qns <- df_data_arranged %>%
    left_join(input_survey %>% select(name, type), by = c("parent_qn"="name")) %>%
    separate(col = type, into = c("select_type", "list_name"), sep =" ", remove = TRUE, extra = "drop" ) %>%
    rename(name = parent_qn)

  # join other responses with choice options based on list_name
  df_join_other_response_with_choices <- df_data_parent_qns %>%
    left_join(df_grouped_choices, by = "list_name") %>%
    mutate(issue_id = "other_checks",
           issue = "",
           checked_by = "",
           checked_date = as_date(today()),
           comment = "",
           reviewed = "",
           adjust_log = ""
    ) %>%
    filter(str_detect(string = current_value, pattern = "other\\b|\\w+_other\\b"))

  # care for select_one and select_multiple (change_response, add_option, remove_option)
  output <- list()
  # select_one checks
  output$select_one <- df_join_other_response_with_choices %>%
    filter(str_detect(select_type, c("select_one|select one"))) %>%
    mutate(type = "change_response")

  # select_multiple checks
  select_mu_data <- df_join_other_response_with_choices %>%
    filter(str_detect(select_type, c("select_multiple|select multiple")))

  select_mu_add_option <- select_mu_data %>%
    mutate(type = "add_option")
  select_mu_remove_option <- select_mu_data %>%
    mutate(type = "remove_option",
           value = as.character(int.my_current_val_extract))

  output$select_multiple <- bind_rows(select_mu_add_option, select_mu_remove_option) %>%
    arrange(uuid, start_date, enumerator_id, name)

  # merge other checks
  merged_other_checks <- bind_rows(output) %>%
    mutate(so_sm_choices = choice_options) %>%
    select(uuid,
           start_date,
           enumerator_id,
           sym(input_location_col),
           sym(input_point_id_col),
           type,
           name,
           current_value,
           value,
           issue_id,
           issue,
           other_text,
           checked_by,
           checked_date,
           comment,
           reviewed,
           adjust_log,
           so_sm_choices)
}


#' Extract other specify data for repeats
#'
#' @param input_repeat_data Specify the data frame for the main dataset joined with repeat. Keep the order Main dataset then join repeat
#' @param input_point_id_col Specify the column for the point_id
#' @param input_location_col Specify the column for location description in the dataset
#' @param input_survey Specify the data frame for the survey sheet
#' @param input_choices Specify the data frame for the choices sheet
#' @param input_sheet_name Specify the sheet name as in the tool
#' @param input_repeat_cols Specify the columns in the repeat that have other specify
#'
#' @return A data frame of other specify for the repeats
#' @export
#'
#' @examples
#' extract_other_specify_data_repeats(input_repeat_data = df_tool_data_protection_risky_places,
#'                                    input_survey = df_survey,
#'                                    input_point_id_col = point_number,
#'                                    input_location_col = district_name,
#'                                    input_choices = df_choices,
#'                                    input_sheet_name = "protection_risky_places",
#'                                    input_repeat_cols = c("places_where_children_are_mostly_at_risk"))
#'
extract_other_specify_data_repeats <- function(input_repeat_data, input_point_id_col = "point_number", input_location_col = "district_name", input_survey, input_choices, input_sheet_name, input_repeat_cols) {

  # add and rename some columns
  df_data <- input_repeat_data %>%
    rename(uuid = `_uuid`) %>%
    filter(!is.na(start)) %>%
    mutate(start_date = as_date(start),
           enumerator_id = as.character(enumerator_id))


  # get questions with other
  others_colnames <-  df_data %>%
    select(starts_with(input_repeat_cols)) %>%
    select(ends_with("_other"), -contains("/")) %>%
    colnames()

  # data.frame for holding _other response data
  df_other_response_data <- purrr::map_dfr(.x = others_colnames,
                                          .f = ~{
                                            df_data %>%
                                              select(-contains("/")) %>%
                                              select(uuid, start_date, enumerator_id, sym(input_location_col) , sym(input_point_id_col), other_text = as.character(.x),
                                                     current_value = str_replace_all(string = .x, pattern = "_other$", replacement = ""),
                                                     , index = `_index.y`) %>%
                                              filter(!is.na(other_text), !other_text %in% c(" ", "NA")) %>%
                                              mutate(other_name = .x,
                                                     int.my_current_val_extract = ifelse(str_detect(current_value, "\\bother\\b"), str_extract_all(string = current_value, pattern = "\\bother\\b|\\w+_other\\b"), current_value),
                                                     value = "",
                                                     parent_qn = str_replace_all(string = .x, pattern = "_other$", replacement = ""),
                                                     across(.cols = !contains("date"), .fns = as.character))
                                          })


  # arrange the data
  df_data_arranged <- df_other_response_data %>%
    arrange(start_date, uuid)

  # get choices to add to the _other responses extracted
  df_grouped_choices <- input_choices %>%
    group_by(list_name) %>%
    summarise(choice_options = paste(name, collapse = " : ")) %>%
    arrange(list_name)

  # extract parent question and join survey for extracting list_name
  df_data_parent_qns <- df_data_arranged %>%
    left_join(input_survey %>% select(name, type), by = c("parent_qn"="name")) %>%
    separate(col = type, into = c("select_type", "list_name"), sep =" ", remove = TRUE, extra = "drop" ) %>%
    rename(name = parent_qn)

  # join other responses with choice options based on list_name
  df_join_other_response_with_choices <- df_data_parent_qns %>%
    left_join(df_grouped_choices, by = "list_name") %>%
    mutate(issue_id = "other_checks",
           issue = "",
           sheet = input_sheet_name,
           checked_by = "",
           checked_date = as_date(today()),
           comment = "",
           reviewed = "",
           adjust_log = ""
    ) %>%
    filter(str_detect(string = current_value, pattern = "other\\b|\\w+_other\\b"))

  # care for select_one and select_multiple (change_response, add_option, remove_option)
  output <- list()
  # select_one checks
  output$select_one <- df_join_other_response_with_choices %>%
    filter(str_detect(select_type, c("select_one|select one"))) %>%
    mutate(type = "change_response")

  # select_multiple checks
  select_mu_data <- df_join_other_response_with_choices %>%
    filter(str_detect(select_type, c("select_multiple|select multiple")))

  select_mu_add_option <- select_mu_data %>%
    mutate(type = "add_option")
  select_mu_remove_option <- select_mu_data %>%
    mutate(type = "remove_option",
           value = as.character(int.my_current_val_extract))

  output$select_multiple <- bind_rows(select_mu_add_option, select_mu_remove_option) %>%
    arrange(uuid, start_date, enumerator_id, name)

  # merge other checks
  merged_other_checks <- bind_rows(output) %>%
    mutate(uuid_cl = "",
           so_sm_choices = choice_options) %>%
    select(sheet,
           uuid,
           start_date,
           enumerator_id,
           sym(input_location_col),
           sym(input_point_id_col),
           type,
           name,
           current_value,
           value,
           index,
           issue_id,
           issue,
           other_text,
           checked_by,
           checked_date,
           comment,
           reviewed,
           adjust_log,
           so_sm_choices)
}
