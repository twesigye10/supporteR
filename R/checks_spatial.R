#' Check duplicate point numbers
#'
#' @param input_tool_data Specify the data frame for the tool data
#' @param input_enumerator_id_col Specify the enumerator id column
#' @param input_location_col Specify the location description column
#' @param input_point_id_col Specify the point id column
#' @param input_sample_pt_nos_list Specify a list of sample point numbers
#'
#' @return Data frame of surveys with duplicate point ids
#' @export
#'
#' @examples
#'
check_duplicate_pt_numbers <- function(input_tool_data,
                                       input_enumerator_id_col = "enumerator_id",
                                       input_location_col,
                                       input_point_id_col,
                                       input_sample_pt_nos_list) {
  if("status" %in% colnames(input_tool_data)){
    input_tool_data %>%
      mutate(i.check.uuid = `_uuid`,
             i.check.start_date = as_date(start),
             !!paste0("i.check.", input_enumerator_id_col) := as.character(!!sym(input_enumerator_id_col)),
             !!paste0("i.check.", input_location_col) := !!sym(input_location_col),
             !!paste0("i.check.", input_point_id_col) := !!sym(input_point_id_col)) %>%
      mutate(unique_pt_number = paste0(status, "_", !!sym(input_point_id_col) )) %>%
      group_by(!!paste0("i.check.", input_location_col), status, !!paste0("i.check.", input_point_id_col)) %>%
      filter(n() > 1, unique_pt_number %in% input_sample_pt_nos_list) %>%
      mutate(i.check.type = "change_response",
             i.check.name = input_point_id_col,
             i.check.current_value = as.character(!!sym(input_point_id_col)),
             i.check.value = "",
             i.check.issue_id = "spatial_c_duplicate_pt_no",
             i.check.issue = paste0(input_point_id_col, ": ", !!sym(input_point_id_col), " is duplicated: check that its not a repeated survey"),
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
             !!paste0("i.check.", input_enumerator_id_col) := as.character(!!sym(input_enumerator_id_col)),
             !!paste0("i.check.", input_location_col) := !!sym(input_location_col),
             !!paste0("i.check.", input_point_id_col) := !!sym(input_point_id_col)) %>%
      mutate(unique_pt_number = !!sym(input_point_id_col)) %>%
      group_by(!!paste0("i.check.", input_location_col), !!paste0("i.check.", input_point_id_col)) %>%
      filter(n() > 1, unique_pt_number %in% input_sample_pt_nos_list) %>%
      mutate(i.check.type = "change_response",
             i.check.name = input_point_id_col,
             i.check.current_value = as.character(!!sym(input_point_id_col)),
             i.check.value = "",
             i.check.issue_id = "spatial_c_duplicate_pt_no",
             i.check.issue = paste0(input_point_id_col,": ", !!sym(input_point_id_col)," is duplicated: check that its not a repeated survey"),
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
#' @param input_enumerator_id_col Specify the enumerator id column
#' @param input_location_col Specify the location description column
#' @param input_point_id_col Specify the point id column
#' @param input_sample_pt_nos_list Specify a list of sample point numbers
#'
#' @return Data frame of surveys with specified point numbers that are not in the samples
#' @export
#'
#' @examples
#'
check_pt_number_not_in_samples <- function(input_tool_data,
                                           input_enumerator_id_col = "enumerator_id",
                                           input_location_col,
                                           input_point_id_col,
                                           input_sample_pt_nos_list) {
  if("status" %in% colnames(input_tool_data)){
    input_tool_data %>%
      mutate(i.check.uuid = `_uuid`,
             i.check.start_date = as_date(start),
             !!paste0("i.check.", input_enumerator_id_col) := as.character(!!sym(input_enumerator_id_col)),
             !!paste0("i.check.", input_location_col) := !!sym(input_location_col),
             !!paste0("i.check.", input_point_id_col) := !!sym(input_point_id_col)) %>%
      mutate(unique_pt_number = paste0(status, "_", !!sym(input_point_id_col) )) %>%
      filter(!unique_pt_number %in% input_sample_pt_nos_list) %>%
      mutate(i.check.type = "change_response",
             i.check.name = input_point_id_col,
             i.check.current_value = as.character(!!sym(input_point_id_col)),
             i.check.value = "",
             i.check.issue_id = "spatial_c_pt_no_not_in_sample",
             i.check.issue = paste0(input_point_id_col,": ",!!sym(input_point_id_col), " not in samples"),
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
             !!paste0("i.check.", input_enumerator_id_col) := as.character(!!sym(input_enumerator_id_col)),
             !!paste0("i.check.", input_location_col) := !!sym(input_location_col),
             !!paste0("i.check.", input_point_id_col) := !!sym(input_point_id_col)) %>%
      mutate(unique_pt_number = !!sym(input_point_id_col)) %>%
      filter(!unique_pt_number %in% input_sample_pt_nos_list) %>%
      mutate(i.check.type = "change_response",
             i.check.name = input_point_id_col,
             i.check.current_value = as.character(!!sym(input_point_id_col)),
             i.check.value = "",
             i.check.issue_id = "spatial_c_pt_no_not_in_sample",
             i.check.issue = paste0(input_point_id_col, ": ", !!sym(input_point_id_col), "not in samples"),
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
#' @param input_enumerator_id_col Specify the enumerator id column
#' @param input_location_col Specify the location description column
#' @param input_point_id_col Specify the point id column
#' @param input_threshold_dist Specify threshold distance. Default is 150m
#'
#' @return Data frame with surveys collected at a distance greater than the specified threshold distance
#' @export
#'
#' @examples
#'
check_threshold_distance <- function(input_sample_data,
                                     input_tool_data,
                                     input_enumerator_id_col = "enumerator_id",
                                     input_location_col,
                                     input_point_id_col,
                                     input_threshold_dist = 150) {

  if("status" %in% colnames(input_tool_data)){
    df_sample_data_thresh <- input_sample_data %>%
      mutate(unique_pt_number = paste0(status, "_", Name )) %>%
      sf::st_transform(4326)

    df_tool_data_thresh <- input_tool_data %>%
      mutate(unique_pt_number = paste0(status, "_", !!sym(input_point_id_col) )) %>%
      sf::st_as_sf(coords = c("_geopoint_longitude","_geopoint_latitude"), crs = 4326)
  }else{
    df_sample_data_thresh <- input_sample_data %>%
      mutate(unique_pt_number = Name) %>%
      sf::st_transform(4326)

    df_tool_data_thresh <- input_tool_data %>%
      mutate(unique_pt_number = !!sym(input_point_id_col)) %>%
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
             !!paste0("i.check.", input_enumerator_id_col) := as.character(!!sym(input_enumerator_id_col)),
             !!paste0("i.check.", input_location_col) := !!sym(input_location_col),
             !!paste0("i.check.", input_point_id_col) := !!sym(input_point_id_col)) %>%
      filter(as.numeric(distance) >= input_threshold_dist) %>%
      mutate(i.check.type = "remove_survey",
             i.check.name = input_point_id_col,
             i.check.current_value = as.character(!!sym(input_point_id_col)),
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
