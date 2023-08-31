#' Sample points for kobo
#' This function prepares coordinates and other columns for adding external data
#' to kobo to support spatial checks during data collection
#' @param input_gis_pt_layer Specify the layer where to extract data
#' @param output_pt_no_col Specify the input point number column
#' @param output_longitude_col Specify the longitude column to output
#' @param output_latitude_col Specify the latitude column to output
#' @param output_description_col Specify the description column to output
#' @param output_file_name Specify the file name to output
#'
#' @return
#' @export
#'
#' @examples
prepare_samples_for_kobo <- function(input_gis_pt_layer,
                                     input_pt_no_col = "pt_num",
                                     output_longitude_col = "longitude",
                                     output_latitude_col = "latitude",
                                     output_description_col = "description",
                                     output_file_name = "sample_pts_data",
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
    data_extraction <- data_extraction_layer %>%
    mutate( lat = sf::st_coordinates(.)[,1],
            lon = sf::st_coordinates(.)[,2],
            )
  }else{
    warning("The provided layer is not a point layer")
  }


}
