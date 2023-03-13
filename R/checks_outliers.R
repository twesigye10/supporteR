
#' Check for outliers on a column using percentiles
#'
#' @param input_tool_data Specify the data frame for the tool data
#' @param input_enumerator_id_col Specify the enumerator id column
#' @param input_location_col Specify the location description column
#' @param input_point_id_col Specify the point id column
#' @param input_column    Specify the column where to detect outliers
#' @param input_lower_limit Lower limit value of the lower percentile
#' @param input_upper_limit Upper limit value of the upper percentile
#'
#' @return The resulting data frame of data out of range
#' @export
#'
#' @examples
#'
check_outliers_on_column <- function(input_tool_data,
                                     input_enumerator_id_col = "enumerator_id",
                                     input_location_col,
                                     input_point_id_col,
                                     input_column,
                                     input_lower_limit,
                                     input_upper_limit) {
  input_tool_data %>%
    filter(!!sym(input_column) < input_lower_limit | !!sym(input_column) > input_upper_limit) %>%
    mutate(i.check.uuid = `_uuid`,
           i.check.start_date = as_date(start),
           !!paste0("i.check.", input_enumerator_id_col) := as.character(!!sym(input_enumerator_id_col)),
           !!paste0("i.check.", input_location_col) := !!sym(input_location_col),
           !!paste0("i.check.", input_point_id_col) := !!sym(input_point_id_col),
           i.check.type = "change_response",
           i.check.name = input_column,
           i.check.current_value = as.character(!!sym({{input_column}})),
           i.check.value = "NA",
           i.check.issue_id = "logic_c_outlier",
           i.check.issue = paste(input_column,": ",!!sym({{input_column}}), "seems to be an outlier, needs confirmation"),
           i.check.other_text = "",
           i.check.checked_by = "",
           i.check.checked_date = as_date(today()),
           i.check.comment = "",
           i.check.reviewed = "",
           i.check.adjust_log = "",
           i.check.so_sm_choices = "") %>%
    ungroup() %>%
    dplyr::select(starts_with("i.check"))%>%
    rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))
}


#' Check for outliers on a column in a repeat loop using percentiles
#'
#' @param input_tool_data Specify the data frame for the repeat loop data joined with main dataset
#' @param input_enumerator_id_col Specify the enumerator id column
#' @param input_location_col Specify the location description column
#' @param input_point_id_col Specify the point id column
#' @param input_column Specify the column where to detect outliers
#' @param input_lower_limit Lower limit value of the lower percentile
#' @param input_upper_limit Upper limit value of the upper percentile
#' @param input_sheet_name Specify the sheet name as defined in the tool
#'
#' @return The resulting data frame of data out of range
#' @export
#'
#' @examples
#'
check_outliers_on_column_repeats <- function(input_tool_data,
                                             input_enumerator_id_col = "enumerator_id",
                                             input_location_col,
                                             input_point_id_col,
                                             input_column,
                                             input_lower_limit,
                                             input_upper_limit,
                                             input_sheet_name) {
  input_tool_data %>%
    filter(!!sym(input_column) < input_lower_limit | !!sym(input_column) > input_upper_limit) %>%
    mutate(i.check.sheet = input_sheet_name,
           i.check.uuid = `_uuid`,
           i.check.start_date = as_date(start),
           !!paste0("i.check.", input_enumerator_id_col) := as.character(!!sym(input_enumerator_id_col)),
           !!paste0("i.check.", input_location_col) := !!sym(input_location_col),
           !!paste0("i.check.", input_point_id_col) := !!sym(input_point_id_col),
           i.check.type = "change_response",
           i.check.name = input_column,
           i.check.current_value = as.character(!!sym({{input_column}})),
           i.check.value = "NA",
           i.check.index = `_index.y`,
           i.check.issue_id = "logic_c_outlier",
           i.check.issue = paste(input_column,": ",!!sym({{input_column}}), "seems to be an outlier, needs confirmation"),
           i.check.other_text = "",
           i.check.checked_by = "",
           i.check.checked_date = as_date(today()),
           i.check.comment = "",
           i.check.reviewed = "",
           i.check.adjust_log = "",
           i.check.so_sm_choices = "") %>%
    ungroup() %>%
    dplyr::select(starts_with("i.check"))%>%
    rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))
}


#' Check for outliers using cleaninginspectoR package and format the log
#'
#' @param input_tool_data Specify the data frame for the tool data
#' @param input_enumerator_id_col Specify the enumerator id column
#' @param input_location_col Specify the location description column
#' @param input_point_id_col Specify the point id column
#'
#' @return The resulting data frame of data out of range
#' @export
#'
#' @examples
#'
check_outliers_cleaninginspector <- function(input_tool_data,
                                             input_enumerator_id_col = "enumerator_id",
                                             input_location_col,
                                             input_point_id_col) {

  escape_columns <- c("start", "end",	"today", "deviceid",
                      "geopoint",	"_geopoint_latitude",	"_geopoint_longitude",
                      "_geopoint_altitude", "_geopoint_precision", "_id")

  input_tool_data %>%
    select(-any_of(escape_columns)) %>%
    cleaninginspectoR::find_outliers() %>%
    rename(int.index = index, int.value = value, int.variable = variable,
           int.issue_type = issue_type) %>%
    left_join(input_tool_data %>% mutate(int.row_number = row_number()),  by = c("int.index" = "int.row_number")) %>%
    mutate(i.check.uuid = `_uuid`,
           i.check.start_date = as_date(start),
           !!paste0("i.check.", input_enumerator_id_col) := as.character(!!sym(input_enumerator_id_col)),
           !!paste0("i.check.", input_location_col) := !!sym(input_location_col),
           !!paste0("i.check.", input_point_id_col) := !!sym(input_point_id_col),
           i.check.type = "change_response",
           i.check.name = int.variable,
           i.check.current_value = as.character(int.value),
           i.check.value = "NA",
           i.check.issue_id = "logic_c_outlier",
           i.check.issue = paste(int.variable,": ",int.value, "seems to be a ", int.issue_type, ", needs confirmation"),
           i.check.other_text = "",
           i.check.checked_by = "",
           i.check.checked_date = as_date(today()),
           i.check.comment = "",
           i.check.reviewed = "",
           i.check.adjust_log = "",
           i.check.so_sm_choices = "") %>%
    ungroup() %>%
    dplyr::select(starts_with("i.check"))%>%
    rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))
}


#' Check for outliers with repeats using cleaninginspectoR package and format the log
#'
#' @param input_tool_data Specify the data frame for the repeat loop data joined with main dataset
#' @param input_enumerator_id_col Specify the enumerator id column
#' @param input_location_col Specify the location description column
#' @param input_point_id_col Specify the point id column
#' @param input_sheet_name Specify the sheet name as defined in the tool
#' @param input_repeat_cols Specify the column names in the repeat loop to be checked
#'
#' @return The resulting data frame of data out of range
#' @export
#'
#' @examples
#'
check_outliers_cleaninginspector_repeats <- function(input_tool_data,
                                                     input_enumerator_id_col = "enumerator_id",
                                                     input_location_col,
                                                     input_point_id_col,
                                                     input_sheet_name,
                                                     input_repeat_cols) {

  escape_columns <- c("start", "end",	"today", "deviceid",
                      "geopoint",	"_geopoint_latitude",	"_geopoint_longitude",
                      "_geopoint_altitude", "_geopoint_precision", "_id", "_parent_index", "_submission__id")

  input_tool_data %>%
    select(any_of(input_repeat_cols)) %>%
    cleaninginspectoR::find_outliers() %>%
    rename(int.index = index, int.value = value, int.variable = variable,
           int.issue_type = issue_type) %>%
    left_join(input_tool_data %>% mutate(int.row_number = row_number()),  by = c("int.index" = "int.row_number")) %>%
    mutate(i.check.sheet = input_sheet_name,
           i.check.uuid = `_uuid`,
           i.check.start_date = as_date(start),
           !!paste0("i.check.", input_enumerator_id_col) := as.character(!!sym(input_enumerator_id_col)),
           !!paste0("i.check.", input_location_col) := !!sym(input_location_col),
           !!paste0("i.check.", input_point_id_col) := !!sym(input_point_id_col),
           i.check.type = "change_response",
           i.check.name = int.variable,
           i.check.current_value = as.character(int.value),
           i.check.value = "NA",
           i.check.index = `_index.y`,
           i.check.issue_id = "logic_c_outlier",
           i.check.issue = paste(int.variable,": ",int.value, "seems to be a ", int.issue_type, ", needs confirmation"),
           i.check.other_text = "",
           i.check.checked_by = "",
           i.check.checked_date = as_date(today()),
           i.check.comment = "",
           i.check.reviewed = "",
           i.check.adjust_log = "",
           i.check.so_sm_choices = "") %>%
    ungroup() %>%
    dplyr::select(starts_with("i.check"))%>%
    rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))
}

