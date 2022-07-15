#' Add checks dataframe to a list
#'
#' @param input_list_name List where to add the checks
#' @param input_df_name Checks dataframe in a global environment
#'
#' @return Updated list with added dataframe
#' @export
#'
#' @examples
#'
add_checks_data_to_list <- function(input_list_name, input_df_name) {
  if(exists(input_list_name) & exists(input_df_name)){
    # get the current values of these objects
    global_list_data <- get(input_list_name, envir = .GlobalEnv)
    global_df_data <- get(input_df_name, envir = .GlobalEnv)
    # check if the dataframe of interest has data
    if(nrow(global_df_data) > 0){
      # append the data frame to the list
      global_list_data[[input_df_name]] <-  global_df_data
      # assign the data to the global environment
      assign(x = input_list_name, value = global_list_data, envir = .GlobalEnv)
    }
  } else{
    message("given objects not in the global environment.")
  }
}
