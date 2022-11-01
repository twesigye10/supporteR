#' map_with_tmap
#' Function to iterate over given columns and produce maps based on those columns
#'
#' @param input_shepefile The shapefile/layer to visulise
#' @param input_col Column for styling the map. This column should be present in the layer provided
#' @param input_ctry_code Provide country code/location of the map
#'
#' @return
#' @export
#'
#' @examples
map_with_tmap <- function(input_shepefile, input_col, input_ctry_code) {
  if (input_col %in% colnames(input_shepefile)){
    tmap::tm_shape(input_shepefile) +
      tmap::tm_polygons(col = input_col, border.col = "white") +
      tmap::tm_layout(paste("Map of",stringr::str_to_upper(input_ctry_code),  "showing", input_col), title.size=.8)+
      tmap::tm_borders("white", lwd = .1) +
      tmap::tm_compass(type = "4star", size = 2, position = c("left", "top"))+
      tmap::tm_scale_bar(position = c("right", "bottom"))
  }else{
    warning("The given column does not exist in the given layer")
  }

}



#' map_with_ggplot Create a map with ggplot
#'
#' @param input_shepefile The shapefile/layer to visulise
#' @param input_col Column for styling the map. This column should be present in the layer provided
#' @param input_ctry_code Provide country code/location of the map
#'
#' @return
#' @export
#'
#' @examples
map_with_ggplot <- function(input_shepefile, input_col, input_ctry_code) {
  ggplot()+
    geom_sf(data = input_shepefile, aes(fill = .data[[input_col]]),  colour = "white") +
    scale_fill_distiller(palette = "YlOrBr", direction = 1, na.value = "grey80") +
    labs(x = " ", y = " ", subtitle = paste("Map of",input_ctry_code,  "showing", input_col))+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_blank(), axis.ticks.x = element_blank(),
          axis.text.y = element_blank(), axis.ticks.y = element_blank(),
          axis.line = element_blank(),
          legend.position="none")
}
