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

