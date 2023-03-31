#' Add extra columns to harmonise checking
#' This adds columns uuid, start_date, enum_id_col and location_col to the data
#' @param input_tool_data Specify the data frame for the tool data
#' @param input_enumerator_id_col Specify the enumerator id column
#' @param input_location_col Specify the location description column
#'
#' @return
#' @export
#'
#' @examples
#'
checks_add_extra_cols <- function(input_tool_data, input_enumerator_id_col, input_location_col) {
  input_tool_data |>
    dplyr::mutate(i.check.uuid = `_uuid`,
           i.check.start_date = lubridate::as_date(start),
           !!paste0("i.check.", input_enumerator_id_col) := as.character(!!sym(input_enumerator_id_col)),
           !!paste0("i.check.", input_location_col) := !!sym(input_location_col))
}
