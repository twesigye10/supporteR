#' Add question label to cleaning log
#'
#' @param input_cl Specify the data frame for the cleaning log
#' @param input_cl_name_col Specify the question/name column of cleaning log
#' @param input_tool Specify the data frame for the survey tool
#' @param input_tool_name_col Specify the name column of the tool
#' @param input_tool_label_col Specify the label column of the tool
#'
#' @return
#' @export
#'
#' @examples
#'
add_qn_label_to_cl <- function(input_cl, input_cl_name_col = "question",
                               input_tool, input_tool_name_col = "name", input_tool_label_col = "label") {
  if(!input_cl_name_col %in% colnames(input_cl)|!input_tool_name_col %in% colnames(input_tool)|!input_tool_label_col %in% colnames(input_tool)){
    stop("check the columns provided for the cleaning log and tool")
  }

  input_cl %>%
    mutate(int.name = !!sym(input_cl_name_col),
           int.name = ifelse(str_detect(string = int.name, pattern = "\\/"),
                             str_replace_all(string = int.name, pattern = "\\/+.*", replacement = ""), int.name),
           question_label = ifelse(int.name %in% input_tool[[input_tool_name_col]],
                                   recode(int.name, !!!setNames(input_tool[[input_tool_label_col]], input_tool[[input_tool_name_col]])),
                                   int.name)) %>%
    relocate(question_label, .after = !!sym(input_cl_name_col)) %>%
    select(-int.name)
}
