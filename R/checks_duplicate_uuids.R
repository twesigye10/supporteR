#' Check data for duplicate uuids
#'
#' @param input_tool_data Specify the data frame for the tool data
#'
#' @return
#' @export
#'
#' @examples
checks_duplicate_uuids <- function(input_tool_data) {
  input_tool_data %>%
    dplyr::group_by(i.check.uuid) %>%
    dplyr::filter(row_number()  > 1) %>%
    dplyr::mutate(
      i.check.type = "remove_survey",
      i.check.name = "uuid",
      i.check.current_value = "",
      i.check.value = "",
      i.check.issue_id = "duplicate_uuid",
      i.check.issue = glue::glue("The uuid: {i.check.uuid} is duplicate in the data"),
      i.check.other_text = "",
      i.check.checked_by = "",
      i.check.checked_date = as_date(today()),
      i.check.comment = "",
      i.check.reviewed = "",
      i.check.adjust_log = "",
      i.check.so_sm_choices = "") %>%
    batch_select_rename()
}

#' Cleaningtools format check data for duplicate uuids
#'
#' @param input_tool_data Specify the data frame for the tool data
#' @param input_uuid_col Specify the uuid column
#'
#' @return
#' @export
#'
#' @examples
cts_checks_duplicate_uuids <- function(input_tool_data, input_uuid_col = "_uuid") {
  input_tool_data %>%
    mutate("i.check.uuid" := as.character(!!sym(input_uuid_col))) %>%
    dplyr::group_by(i.check.uuid) %>%
    dplyr::filter(row_number()  > 1) %>%
    dplyr::mutate(
      i.check.change_type = "remove_survey",
      i.check.question = "uuid",
      i.check.old_value = "",
      i.check.new_value = "",
      i.check.issue = glue::glue("The uuid is duplicate in the data"),
      i.check.other_text = "",
      i.check.comment = "",
      i.check.reviewed = "",
      i.check.so_sm_choices = "") %>%
    batch_select_rename()
}
