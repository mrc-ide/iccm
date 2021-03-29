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

replace_render_na <- function(output, variables = c("chw_patients", "chw_ors", "chw_followup",
                                                             "private_patients", "private_ors",
                                                             "hf_patients", "hf_ors", "hf_severe_diarrhoea_tx",
                                                             "graduation")){
  output[,variables][is.na(output[,variables])] <- 0
  return(output)
}
