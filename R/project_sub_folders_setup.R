#' Create sub folders for a project in RStudio
#'
#' @param input_sub_folders a vector of strings representing names of sub folders to be created.
#' The default folders are "inputs", "outputs", "R", "support_files"
#'
#' @return
#' @export
#'
#' @examples create_project_sub_folders(input_sub_folders = c("inputs", "outputs", "R", "support_files"))
#' # This will create the sub folders folders "inputs", "outputs", "R", "support_files" in the current R project
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


#' Add .gitkeep file to ignored sub folders in .gitignore
#'This file helps to be able to commit folders ignored in the gitignore
#'
#' @param input_sub_folders Sub folders to be maintained as empty folders in the project structure while committing
#' The default folders are "inputs", "outputs"
#'
#' @return
#' @export
#'
#' @examples add_dot_gitkeep_to_folder(input_sub_folders = c("inputs", "outputs"))
#' # This will add .gitkeep in the sub folders "inputs", "outputs"
#'
add_dot_gitkeep_to_folder <- function(input_sub_folders = c("inputs", "outputs")) {
  sub_directories <- input_sub_folders
  # iterate the sub folders and add the required file
  for(x in sub_directories){
    # check folder existence
    check_folder <- dir.exists(paste0(getwd(),"/",x))

    if (check_folder == TRUE) {
      file_path <- paste0("./",x,"/.gitkeep")
      if(!file.exists(file_path)){
        file.create(file_path)
        message(paste("added .gitkeep to :", x))
      }
    } else{
      warning(paste("Folder does not exist :", x))
    }
  }
}
