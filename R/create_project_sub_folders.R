create_project_sub_folders <- function(input_sub_folders = c("outputs", "inputs", "R", "support_files")) {
  sub_directories <- input_sub_folders
  # iterate the sub folders and create them
  for(x in sub_directories){
    # check folder existence
    check_folder <- dir.exists(paste0(getwd(),"/",x))

    if (check_folder == FALSE) {
      dir.create(paste0("./",x), showWarnings = FALSE)
      message(paste("Folder created :", x))
      # add ".gitkeep" files to maintain folders while pushing code to github.
      # works with gitignore
      if (x %in% c("outputs", "inputs")){
        file.create(paste0("./",x,"/.gitkeep"))
      }
    } else{
      warning(paste("Folder already exists :", x))
    }
  }
}
