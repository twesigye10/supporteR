
#' Check for outliers on a column using percentiles
#'
#' @param input_tool_data Specify the data frame for the tool data
#' @param input_column    Specify the column where to detect outliers
#' @param input_lower_limit Lower limit value of the lower percentile
#' @param input_upper_limit Upper limit value of the upper percentile
#'
#' @return The resulting data frame of surveys out of range
#' @export
#'
#' @examples
#'
check_outliers_on_column <- function(input_tool_data, input_column, input_lower_limit, input_upper_limit) {
  input_tool_data %>%
    filter(!!sym(input_column) < input_lower_limit | !!sym(input_column) > input_upper_limit) %>%
    mutate(i.check.type = "change_response",
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
           i.check.uuid_cl = "",
           i.check.so_sm_choices = "") %>%
    ungroup() %>%
    dplyr::select(starts_with("i.check"))%>%
    rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))
}


#' Check for outliers on a column in a repeat loop using percentiles
#'
#' @param input_tool_data Specify the data frame for the tool data
#' @param input_column Specify the column where to detect outliers
#' @param input_lower_limit Lower limit value of the lower percentile
#' @param input_upper_limit Upper limit value of the upper percentile
#' @param input_sheet_name Specify the sheet name as defined in the tool
#'
#' @return The resulting data frame of surveys out of range
#' @export
#'
#' @examples
#'
check_outliers_on_column_repeats <- function(input_tool_data, input_column, input_lower_limit, input_upper_limit, input_sheet_name) {
  input_tool_data %>%
    filter(!!sym(input_column) < input_lower_limit | !!sym(input_column) > input_upper_limit) %>%
    mutate(i.check.sheet = input_sheet_name,
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
           i.check.uuid_cl = "",
           i.check.so_sm_choices = "") %>%
    ungroup() %>%
    dplyr::select(starts_with("i.check"))%>%
    rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))
}
