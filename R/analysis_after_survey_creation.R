#' Analysis after survey creation
#' This function helps to analyse the data. It is based on butteR::survey_collapse.
#' @param input_svy_obj Input survey object. The object is created using srvyr::as_survey.
#' @param input_dap Input dap. The dap template has columns 'variable', 'split', 'subset_1'.
#'
#' @return
#' @export
#'
#' @examples
analysis_after_survey_creation <- function(input_svy_obj, input_dap) {

  # store analyses
  outputs <-list()

  # analysis -----------------------------------------------------------------

  dap_refugee <- input_dap %>%
    filter(split %in% c("all", "refugee_only"))

  # no subsets
  refugee_variables_no_subsets <- dap_refugee %>%
    pull(variable) %>% unique()

  # overall, no additional subset
  outputs$ref_overall <- butteR::survey_collapse(df = input_svy_obj,
                                                 vars_to_analyze = refugee_variables_no_subsets) %>%
    mutate(population = "")

  #  subsets
  dap_refugee_subset1 <- input_dap %>%
    filter(split %in%  c("all","refugee_only"), !is.na(subset_1))

  # overall, subset 1
  dap_refugee_subset_split <- dap_refugee_subset1 %>%
    split(.$subset_1)

  ref_overall_subset1 <-list()

  for(i in seq_along(dap_refugee_subset_split)){
    print(i)
    subset_temp <- dap_refugee_subset_split[[i]]
    subset_value <- unique(subset_temp$subset_1)
    vars_temp <- subset_temp %>% pull(variable)
    ref_overall_subset1[[subset_value]] <- butteR::survey_collapse(df = input_svy_obj,
                                                                   vars_to_analyze = vars_temp ,
                                                                   disag = c( subset_value)
    )
  }

  outputs$ref_overall_subset1 <- bind_rows(ref_overall_subset1) %>%
    mutate(population = "")

  # merge analysis ----------------------------------------------------------

  bind_rows(outputs)
}
