#' Plot simulation output
#'
#' @param output Long simulation output
#' @param vars Variables to select
#'
#' @return Output plot, one panel for each variable
#' @export
plot_sim <- function(output, vars = NULL) {
  if(is.null(vars)){
    pd <- output
  } else {
    pd <- dplyr::filter(output, .data$variable %in% vars)
  }

  ggplot2::ggplot(pd, ggplot2::aes(x = .data$timestep, y = .data$y, col = .data$variable)) +
    ggplot2::geom_line() +
    ggplot2::xlab("Time") +
    ggplot2::ylab("") +
    ggplot2::facet_wrap(~.data$variable, scales = "free_y") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none",
                   strip.background = ggplot2::element_rect(fill = "white"))
}
