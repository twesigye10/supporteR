
#' Check survey time against expected minimum time and maximum time of the survey
#'
#' @param input_tool_data Specify the data frame for the tool data
#' @param input_enumerator_id_col Specify the enumerator id column
#' @param input_location_col Specify the location description column
#' @param input_min_time Specify the minimum expected time for the survey
#' @param input_max_time Specify the maximum expected time for the survey
#'
#' @return The resulting data frame of surveys outside of the minimum and maximum times
#' @export
#'
#' @examples
#'
check_survey_time <- function(input_tool_data,
                              input_enumerator_id_col = "enumerator_id",
                              input_location_col,
                              input_min_time,
                              input_max_time) {
  input_tool_data %>%
    mutate(i.check.uuid = `_uuid`,
           i.check.start_date = as_date(start),
           !!paste0("i.check.", input_enumerator_id_col) := as.character(!!sym(input_enumerator_id_col)),
           !!paste0("i.check.", input_location_col) := !!sym(input_location_col),
           start = as_datetime(start),
           end = as_datetime(end)) %>%
    mutate(int.survey_time_interval = lubridate::time_length(end - start, unit = "min"),
           int.survey_time_interval = ceiling(int.survey_time_interval),
           i.check.type = "remove_survey",
           i.check.name = "",
           i.check.current_value = "",
           i.check.value = "",
           i.check.issue_id = case_when(
             int.survey_time_interval < input_min_time ~ "less_survey_time",
             int.survey_time_interval > input_max_time ~ "more_survey_time",
             TRUE ~ "normal_survey_time" ),
           i.check.issue = glue::glue("{int.survey_time_interval} min taken to do the survey"),
           i.check.other_text = "",
           i.check.checked_by = "",
           i.check.checked_date = as_date(today()),
           i.check.comment = "",
           i.check.reviewed = "",
           i.check.adjust_log = "",
           i.check.so_sm_choices = "") %>%
    filter(i.check.issue_id %in% c("less_survey_time", "more_survey_time")) %>%
    batch_select_rename()
}


#' Check interval between surveys by the same enumerator
#'
#' @param input_tool_data Specify the data frame for the tool data
#' @param input_enumerator_id_col Specify the enumerator id column
#' @param input_location_col Specify the location description column
#' @param input_min_time Specify the minimum expected time for an enumerator to move from one survey location to another
#'
#' @return The resulting data frame of surveys not meeting this requirement
#' @export
#'
#' @examples
#'
check_time_interval_btn_surveys <- function(input_tool_data,
                                            input_enumerator_id_col = "enumerator_id",
                                            input_location_col,
                                            input_min_time) {
  input_tool_data %>%
    mutate(i.check.uuid = `_uuid`,
           i.check.start_date = as_date(start),
           !!paste0("i.check.", input_enumerator_id_col) := as.character(!!sym(input_enumerator_id_col)),
           !!paste0("i.check.", input_location_col) := !!sym(input_location_col),
           start = as_datetime(start),
           end = as_datetime(end)) %>%
    group_by(i.check.start_date, i.check.enumerator_id) %>%
    filter(n()>1) %>%
    arrange(start, .by_group = TRUE) %>%
    mutate(int.time_between_survey = lubridate::time_length(start - lag(end, default = first(start)), unit = "min"),
           int.time_between_survey = ceiling(int.time_between_survey)) %>%
    filter(int.time_between_survey != 0 & int.time_between_survey < input_min_time) %>%
    mutate(i.check.type = "remove_survey",
           i.check.name = "",
           i.check.current_value = "",
           i.check.value = "",
           i.check.issue_id = "less_time_btn_surveys",
           i.check.issue = glue::glue("{int.time_between_survey} min taken between surveys"),
           i.check.other_text = "",
           i.check.checked_by = "",
           i.check.checked_date = as_date(today()),
           i.check.comment = "",
           i.check.reviewed = "",
           i.check.adjust_log = "",
           i.check.so_sm_choices = "") %>%
    batch_select_rename()
}


#' Get average survey time
#' This function tends to exclude outliers
#'
#' @param input_tool_data Specify the data frame for the tool data
#' @param input_lower_limit Specify the quantile lower limit
#' @param input_upper_limit Specify the quantile upper limit
#'
#' @return
#' @export
#'
#' @examples
get_average_survey_time <- function(input_tool_data, input_lower_limit = 0.025, input_upper_limit = 0.975) {

  df_tool_data_with_time_interval <- input_tool_data |>
    mutate(int.survey_time_interval = lubridate::time_length(end - start, unit = "min"),
           int.survey_time_interval = ceiling(int.survey_time_interval)
    )|>
    filter(int.survey_time_interval > 0 ) |> # consider only surveys with positive time interval
    select(int.survey_time_interval)

  # lower and upper quantiles of survey duration
  lower_limit = quantile(df_tool_data_with_time_interval$int.survey_time_interval, input_lower_limit, na.rm =TRUE)
  upper_limit = quantile(df_tool_data_with_time_interval$int.survey_time_interval, input_upper_limit, na.rm =TRUE)

  df_tool_data_with_time_interval |>
    filter(int.survey_time_interval > lower_limit | int.survey_time_interval < upper_limit) |>
    summarise(average_time = round(mean(int.survey_time_interval, na.rm = TRUE), 0)) |>
    pull()
}
