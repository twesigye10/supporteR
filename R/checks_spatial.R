#' Check duplicate point numbers
#'
#' @param input_tool_data Specify the data frame for the tool data
#' @param input_sample_pt_nos_list Specify a list of sample point numbers
#'
#' @return Data frame of surveys with duplicate point_number
#' @export
#'
#' @examples
#'
check_duplicate_pt_numbers <- function(input_tool_data, input_sample_pt_nos_list) {
  input_tool_data %>%
    mutate(i.check.uuid = `_uuid`,
           i.check.start_date = as_date(start),
           i.check.enumerator_id = enumerator_id,
           i.check.district_name = district_name,
           i.check.point_number = point_number) %>%
    mutate(unique_pt_number = paste0(status, "_", point_number )) %>%
    group_by(i.check.district_name, status, i.check.point_number) %>%
    filter(n() > 1, unique_pt_number %in% input_sample_pt_nos_list) %>%
    mutate(i.check.type = "change_response",
           i.check.name = "point_number",
           i.check.current_value = point_number,
           i.check.value = "",
           i.check.issue_id = "spatial_c_duplicate_pt_no",
           i.check.issue = glue("point_number: {point_number} is duplicated: check that its not a repeated survey"),
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



#' Check for point numbers not in samples
#'
#' @param input_tool_data Specify the data frame for the tool data
#' @param input_sample_pt_nos_list Specify a list of sample point numbers
#'
#' @return Data frame of surveys with specified point numbers that are not in the samples
#' @export
#'
#' @examples
#'
check_pt_number_not_in_samples <- function(input_tool_data, input_sample_pt_nos_list) {
  input_tool_data %>%
    mutate(i.check.uuid = `_uuid`,
           i.check.start_date = as_date(start),
           i.check.enumerator_id = enumerator_id,
           i.check.district_name = district_name,
           i.check.point_number = point_number) %>%
    mutate(unique_pt_number = paste0(status, "_", point_number )) %>%
    filter(!unique_pt_number %in% input_sample_pt_nos_list) %>%
    mutate(i.check.type = "change_response",
           i.check.name = "point_number",
           i.check.current_value = point_number,
           i.check.value = "",
           i.check.issue_id = "spatial_c_pt_no_not_in_sample",
           i.check.issue = glue("point_number: {point_number} not in samples"),
           i.check.other_text = "",
           i.check.checked_by = "",
           i.check.checked_date = as_date(today()),
           i.check.comment = "",
           i.check.reviewed = "",
           i.check.adjust_log = "",
           i.check.uuid_cl = "",
           i.check.so_sm_choices = "") %>%
    dplyr::select(starts_with("i.check"))%>%
    rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))
}
