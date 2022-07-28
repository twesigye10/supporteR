
#' Check survey time against expected minimum time and maximum time of the survey
#'
#' @param input_tool_data Specify the data frame for the tool data
#' @param input_min_time Specify the minimum expected time for the survey
#' @param input_max_time Specify the maximum expected time for the survey
#'
#' @return The resulting data frame of surveys outside of the minimum and maximum times
#' @export
#'
#' @examples
#'
check_survey_time <- function(input_tool_data, input_min_time, input_max_time) {
  input_tool_data %>%
    mutate(i.check.uuid = `_uuid`,
           i.check.start_date = as_date(start),
           i.check.enumerator_id = as.character(enumerator_id),
           i.check.district_name = district_name,
           i.check.point_number = point_number,
           start = as_datetime(start),
           end = as_datetime(end)) %>%
    mutate(int.survey_time_interval = lubridate::time_length(end - start, unit = "min"),
           int.survey_time_interval = ceiling(int.survey_time_interval),
           i.check.type = "remove_survey",
           i.check.name = "point_number",
           i.check.current_value = "",
           i.check.value = "",
           i.check.issue_id = case_when(
             int.survey_time_interval < input_min_time ~ "less_survey_time",
             int.survey_time_interval > input_max_time ~ "more_survey_time",
             TRUE ~ "normal_survey_time" ),
           i.check.issue = glue("{int.survey_time_interval} min taken to do the survey"),
           i.check.other_text = "",
           i.check.checked_by = "",
           i.check.checked_date = as_date(today()),
           i.check.comment = "",
           i.check.reviewed = "",
           i.check.adjust_log = "",
           i.check.so_sm_choices = "") %>%
    filter(i.check.issue_id %in% c("less_survey_time", "more_survey_time")) %>%
    dplyr::select(starts_with("i.check")) %>%
    rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))
}


#' Check interval between surveys by the same enumerator
#'
#' @param input_tool_data Specify the data frame for the tool data
#' @param input_min_time Specify the minimum expected time for an enumerator to move from one survey location to another
#'
#' @return The resulting data frame of surveys not meeting this requirement
#' @export
#'
#' @examples
#'
check_time_interval_btn_surveys <- function(input_tool_data, input_min_time) {
  input_tool_data %>%
    mutate(i.check.uuid = `_uuid`,
           i.check.start_date = as_date(start),
           i.check.enumerator_id = as.character(enumerator_id),
           i.check.district_name = district_name,
           i.check.point_number = point_number,
           start = as_datetime(start),
           end = as_datetime(end)) %>%
    group_by(i.check.start_date, i.check.enumerator_id) %>%
    filter(n()>1) %>%
    arrange(start, .by_group = TRUE) %>%
    mutate(int.time_between_survey = lubridate::time_length(start - lag(end, default = first(start)), unit = "min"),
           int.time_between_survey = ceiling(int.time_between_survey)) %>%
    filter(int.time_between_survey != 0 & int.time_between_survey < input_min_time) %>%
    mutate(i.check.type = "remove_survey",
           i.check.name = "point_number",
           i.check.current_value = "",
           i.check.value = "",
           i.check.issue_id = "less_time_btn_surveys",
           i.check.issue = glue("{int.time_between_survey} min taken between surveys"),
           i.check.other_text = "",
           i.check.checked_by = "",
           i.check.checked_date = as_date(today()),
           i.check.comment = "",
           i.check.reviewed = "",
           i.check.adjust_log = "",
           i.check.so_sm_choices = "") %>%
    dplyr::select(starts_with("i.check")) %>%
    rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))
}
