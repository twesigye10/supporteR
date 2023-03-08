#' Batch select and rename columns
#'
#' @param input_df
#' @param input_selection_str
#' @param input_replacement_str
#'
#' @return
#' @export
#'
#' @examples
batch_select_rename <- function(input_df, input_selection_str = "i.check.", input_replacement_str = "") {
  input_df |>
    dplyr::select(starts_with(input_selection_str)) |>
    dplyr::rename_with(.fn = ~str_replace(string = .x, pattern = input_selection_str, replacement =  input_replacement_str))
}
