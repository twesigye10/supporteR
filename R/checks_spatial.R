#' Check duplicate point numbers
#'
#' @param input_tool_data Specify the data frame for the tool data
#' @param input_sample_pt_nos_list Specify a list of sample point numbers
#'
#' @return Data frame of surveys with duplicate point_number
#' @export
#'
#' @examples
#'
check_duplicate_pt_numbers <- function(input_tool_data, input_sample_pt_nos_list) {
  if("status" %in% colnames(input_tool_data)){
    input_tool_data %>%
      mutate(i.check.uuid = `_uuid`,
             i.check.start_date = as_date(start),
             i.check.enumerator_id = enumerator_id,
             i.check.district_name = district_name,
             i.check.point_number = point_number) %>%
      mutate(unique_pt_number = paste0(status, "_", point_number )) %>%
      group_by(i.check.district_name, status, i.check.point_number) %>%
      filter(n() > 1, unique_pt_number %in% input_sample_pt_nos_list) %>%
      mutate(i.check.type = "change_response",
             i.check.name = "point_number",
             i.check.current_value = point_number,
             i.check.value = "",
             i.check.issue_id = "spatial_c_duplicate_pt_no",
             i.check.issue = glue("point_number: {point_number} is duplicated: check that its not a repeated survey"),
             i.check.other_text = "",
             i.check.checked_by = "",
             i.check.checked_date = as_date(today()),
             i.check.comment = "",
             i.check.reviewed = "",
             i.check.adjust_log = "",
             i.check.so_sm_choices = "") %>%
      ungroup() %>%
      dplyr::select(starts_with("i.check"))%>%
      rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))
  }else{
    input_tool_data %>%
      mutate(i.check.uuid = `_uuid`,
             i.check.start_date = as_date(start),
             i.check.enumerator_id = enumerator_id,
             i.check.district_name = district_name,
             i.check.point_number = point_number) %>%
      mutate(unique_pt_number = point_number) %>%
      group_by(i.check.district_name, i.check.point_number) %>%
      filter(n() > 1, unique_pt_number %in% input_sample_pt_nos_list) %>%
      mutate(i.check.type = "change_response",
             i.check.name = "point_number",
             i.check.current_value = point_number,
             i.check.value = "",
             i.check.issue_id = "spatial_c_duplicate_pt_no",
             i.check.issue = glue("point_number: {point_number} is duplicated: check that its not a repeated survey"),
             i.check.other_text = "",
             i.check.checked_by = "",
             i.check.checked_date = as_date(today()),
             i.check.comment = "",
             i.check.reviewed = "",
             i.check.adjust_log = "",
             i.check.so_sm_choices = "") %>%
      ungroup() %>%
      dplyr::select(starts_with("i.check"))%>%
      rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))
  }

}



#' Check for point numbers not in samples
#'
#' @param input_tool_data Specify the data frame for the tool data
#' @param input_sample_pt_nos_list Specify a list of sample point numbers
#'
#' @return Data frame of surveys with specified point numbers that are not in the samples
#' @export
#'
#' @examples
#'
check_pt_number_not_in_samples <- function(input_tool_data, input_sample_pt_nos_list) {
  if("status" %in% colnames(input_tool_data)){
    input_tool_data %>%
      mutate(i.check.uuid = `_uuid`,
             i.check.start_date = as_date(start),
             i.check.enumerator_id = enumerator_id,
             i.check.district_name = district_name,
             i.check.point_number = point_number) %>%
      mutate(unique_pt_number = paste0(status, "_", point_number )) %>%
      filter(!unique_pt_number %in% input_sample_pt_nos_list) %>%
      mutate(i.check.type = "change_response",
             i.check.name = "point_number",
             i.check.current_value = point_number,
             i.check.value = "",
             i.check.issue_id = "spatial_c_pt_no_not_in_sample",
             i.check.issue = glue("point_number: {point_number} not in samples"),
             i.check.other_text = "",
             i.check.checked_by = "",
             i.check.checked_date = as_date(today()),
             i.check.comment = "",
             i.check.reviewed = "",
             i.check.adjust_log = "",
             i.check.so_sm_choices = "") %>%
      dplyr::select(starts_with("i.check"))%>%
      rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))
  }else{
    input_tool_data %>%
      mutate(i.check.uuid = `_uuid`,
             i.check.start_date = as_date(start),
             i.check.enumerator_id = enumerator_id,
             i.check.district_name = district_name,
             i.check.point_number = point_number) %>%
      mutate(unique_pt_number = point_number) %>%
      filter(!unique_pt_number %in% input_sample_pt_nos_list) %>%
      mutate(i.check.type = "change_response",
             i.check.name = "point_number",
             i.check.current_value = point_number,
             i.check.value = "",
             i.check.issue_id = "spatial_c_pt_no_not_in_sample",
             i.check.issue = glue("point_number: {point_number} not in samples"),
             i.check.other_text = "",
             i.check.checked_by = "",
             i.check.checked_date = as_date(today()),
             i.check.comment = "",
             i.check.reviewed = "",
             i.check.adjust_log = "",
             i.check.so_sm_choices = "") %>%
      dplyr::select(starts_with("i.check"))%>%
      rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))
  }

}


#' check that collected point is not at a distance greater than the threshold
#'
#' @param input_sample_data Specify a GIS layer with sample data
#' @param input_tool_data Specify the data frame for the tool data with "_geopoint_longitude" and "_geopoint_latitude" columns
#' @param input_threshold_dist Specify threshold distance. Default is 150m
#'
#' @return Data frame with surveys collected at a distance greater than the specified threshold distance
#' @export
#'
#' @examples
#'
check_threshold_distance <- function(input_sample_data, input_tool_data, input_threshold_dist = 150) {

  if("status" %in% colnames(input_tool_data)){
    df_sample_data_thresh <- input_sample_data %>%
      mutate(unique_pt_number = paste0(status, "_", Name )) %>%
      sf::st_transform(4326)

    df_tool_data_thresh <- input_tool_data %>%
      mutate(unique_pt_number = paste0(status, "_", point_number )) %>%
      sf::st_as_sf(coords = c("_geopoint_longitude","_geopoint_latitude"), crs = 4326)
  }else{
    df_sample_data_thresh <- input_sample_data %>%
      mutate(unique_pt_number = Name) %>%
      sf::st_transform(4326)

    df_tool_data_thresh <- input_tool_data %>%
      mutate(unique_pt_number = point_number) %>%
      sf::st_as_sf(coords = c("_geopoint_longitude","_geopoint_latitude"), crs = 4326)
  }


  # sample_data_unique_pts
  sample_data_unique_pts <- df_sample_data_thresh %>%
    pull(unique_pt_number) %>%
    unique()
  # tool_data_unique_pts
  tool_data_unique_pts <- df_tool_data_thresh %>%
    pull(unique_pt_number) %>%
    unique()

  sample_pt_nos_thresh <- sample_data_unique_pts[sample_data_unique_pts %in% tool_data_unique_pts]

  if(length(sample_pt_nos_thresh) > 0){

    # tibble to hold the data
    df_data_with_distance <- tibble()

    for (pt_number in sample_pt_nos_thresh){
      current_sample <- df_sample_data_thresh %>%
        filter(unique_pt_number == pt_number)
      current_tool_data <- df_tool_data_thresh %>%
        filter(unique_pt_number == pt_number)

      if(nrow(current_tool_data) > 0){
        current_sample_target_dist <- sf::st_distance(x = current_sample, y = current_tool_data, by_element = TRUE)

        current_data_with_dist <- current_tool_data %>%
          sf::st_drop_geometry() %>%
          mutate(distance = round(x = current_sample_target_dist, digits = 0))

        df_data_with_distance <- bind_rows(df_data_with_distance, current_data_with_dist)
      }
    }

    # format the required data
    df_data_with_distance %>%
      mutate(i.check.uuid = `_uuid`,
             i.check.start_date = as_date(start),
             i.check.enumerator_id = enumerator_id,
             i.check.district_name = district_name,
             i.check.point_number = point_number) %>%
      filter(as.numeric(distance) >= input_threshold_dist) %>%
      mutate(i.check.type = "remove_survey",
             i.check.name = "point_number",
             i.check.current_value = point_number,
             i.check.value = "",
             i.check.issue_id = "spatial_c_dist_to_sample_greater_than_threshold",
             i.check.issue = glue("{distance} m greater_than_threshold: {input_threshold_dist} m"),
             i.check.other_text = "",
             i.check.checked_by = "",
             i.check.checked_date = as_date(today()),
             i.check.comment = "",
             i.check.reviewed = "",
             i.check.adjust_log = "",
             i.check.so_sm_choices = "") %>%
      dplyr::select(starts_with("i.check"))%>%
      rename_with(~str_replace(string = .x, pattern = "i.check.", replacement = ""))
  }
}
