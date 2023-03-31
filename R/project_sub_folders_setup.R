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
#' @examples add_dot_gitkeep_to_folder(input_sub_folders = c("inputs", "outputs", "support_files))
#' # This will add .gitkeep in the sub folders "inputs", "outputs"
#'
add_dot_gitkeep_to_folder <- function(input_sub_folders = c("inputs", "outputs", "support_files")) {
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


#' Add gitignore entries
#' This adds entries to the gitignore file in the project so that
#' the files are not followed and committed for version management
#'
#' @param input_lines_to_add A vector of files and folders to ignore
#'
#' @return
#' @export
#'
#' @examples
#'
add_gitignore_entries <- function(input_lines_to_add = c("\n# Add file types to ignore", "*.html", "*.xlsx", "*.xls", ".csv", ".pdf", ".doc", ".docx", "inputs/*","outputs/*","support_files/*","!/**/.gitkeep")) {
  # check file existence
  check_file <- file.exists(paste0(getwd(),"/.gitignore"))
  # if gitignore file exists
  if (check_file == TRUE) {
    file_path <- paste0(getwd(),"/.gitignore")
    current_ignore_entries <-  read_lines(paste0(getwd(),"/.gitignore"))
    for (x in input_lines_to_add) {
      if (!x %in% current_ignore_entries){
        write(x, file_path, append = TRUE)
      } else{warning( paste(x, ": already exists in gitignore file"))}
    }
  } else{
    warning("gitignore file does not exist, first enable git in the project")
  }
}
