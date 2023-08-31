#' Sample points for kobo
#' This function prepares coordinates and other columns for adding external data
#' to kobo to support spatial checks during data collection
#'
#' @param input_gis_pt_layer Specify the layer where to extract data
#' @param input_pt_no_col Specify the input point number column
#' @param output_longitude_col Specify the longitude column to output
#' @param output_latitude_col Specify the latitude column to output
#' @param output_description_col Specify the description column to output
#' @param output_file_name Specify the file name to output
#' @param output_folder_name Specify the folder where to export data
#'
#' @return
#' @export
#'
#' @examples
samples_for_kobo <- function(input_gis_pt_layer,
                                     input_pt_no_col = "pt_num",
                                     output_longitude_col = "longitude",
                                     output_latitude_col = "latitude",
                                     output_description_col = "description",
                                     output_file_name = "sample_pts_data",
                                     output_folder_name = "outputs"
                                     ) {
  # test type of the geometry in the layer
  if(sf::st_is(x = input_gis_pt_layer[1,],type = "POINT")){
    # get coordinate system
    current_cs <- st_crs(input_gis_pt_layer)
    if(current_cs %in% c("EPSG:4326")){
      data_extraction_layer <- input_gis_pt_layer
    }else{
      data_extraction_layer <- sf::st_transform(input_gis_pt_layer, crs = 4326)
    }
    # add and format columns
    data_extraction <- data_extraction_layer %>%
    mutate( !!paste0("i.check.", input_pt_no_col) := as.character(!!sym(input_pt_no_col)),
            !!paste0("i.check.", output_latitude_col) := sf::st_coordinates(.)[,1],
            !!paste0("i.check.", output_longitude_col) := sf::st_coordinates(.)[,2],
            !!paste0("i.check.", output_description_col) := as.character(!!sym(output_description_col)) ) %>%
      sf::st_drop_geometry() %>%
      batch_select_rename()
    # check if folder exists and if not create it
    if(dir.exists(paste0(getwd(),"/", output_folder_name)) == FALSE){
      dir.create(paste0("./", output_folder_name), showWarnings = FALSE)
      message(paste("Folder created :", output_folder_name))
    }
    # export the data
    readr::write_csv(x = data_extraction, file = paste0(output_folder_name, "/", butteR::date_file_prefix(), "_", output_file_name, ".csv"))
  }else{
    stop("The provided layer is not a point layer!")
  }
}
