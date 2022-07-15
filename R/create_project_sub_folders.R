#' Create sub folders for a project in RStudio
#'
#' @param input_sub_folders a vector of strings representing names of folders.
#' The default folders are c("inputs", "outputs", "R", "support_files")
#'
#' @return
#' @export
#'
#' @examples create_project_sub_folders(input_sub_folders = c("inputs", "outputs", "R", "support_files"))
#' This will create the sub folders folders "inputs", "outputs", "R", "support_files" in the current R project
#'
create_project_sub_folders <- function(input_sub_folders = c("inputs", "outputs", "R", "support_files")) {
  sub_directories <- input_sub_folders
  # iterate the sub folders and create them
  for(x in sub_directories){
    # check folder existence
    check_folder <- dir.exists(paste0(getwd(),"/",x))

    if (check_folder == FALSE) {
      dir.create(paste0("./",x), showWarnings = FALSE)
      message(paste("Folder created :", x))
    } else{
      warning(paste("Folder already exists :", x))
    }
  }
}
