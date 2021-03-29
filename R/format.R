#' Convert wide output to long.
#'
#' @param output Simulation output from \code{individual::simulate}
#'
#' @return Long output
convert_to_long <- function(output){
  output %>%
    tidyr::pivot_longer(cols = -.data$timestep, names_to = "variable", values_to = "y") %>%
    dplyr::arrange(.data$variable, .data$timestep)
}

#' Replace NA in output
#'
#' When rendering occurs in a function that is not called every timestep, missing outputs
#' are NA. We can selectivly convert these to zeros
#'
#' @param output Model output
#' @param variables Variables where we want to convert NAs -> 0s
replace_render_na <- function(output, variables = c("chw_patients", "chw_ors", "chw_followup",
                                                             "private_patients", "private_ors",
                                                             "hf_patients", "hf_ors", "hf_severe_diarrhoea_tx",
                                                             "graduation")){
  variables <- variables[variables %in% names(output)]
  output[,variables][is.na(output[,variables])] <- 0
  return(output)
}
