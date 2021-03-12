#' Plot simulation output
#'
#' @param output Long simulation output
#' @param return_list Return list of component plots
#'
#' @return Output plot, one panel for each variable
#' @export
plot_sim <- function(output, return_list = FALSE) {
  plots <- list(
    dia_prev_plot = sub_plot(output,
                             "Diarrhoea prevalence",
                             c("dia_prevalence",
                               "dia_bacteria_prevalence",
                               "dia_virus_prevalence",
                               "dia_parasite_prevalence",
                               "dia_rotavirus_prevalence"),
                             c("All", "Bacteria", "Virus","Parasite", "Rotavirus"),
                             "Prevalence"),
    dia_inc_plot = sub_plot(output,
                             "Diarrhoea incidence",
                             c("dia_bacteria_incidence",
                               "dia_virus_incidence",
                               "dia_parasite_incidence",
                               "dia_rotavirus_incidence"),
                             c("Bacteria", "Virus","Parasite", "Rotavirus"),
                             "Incidence"),
    dia_prior_plot = sub_plot(output,
                             "Diarrhoea prior",
                             c("dia_bacteria_prior",
                               "dia_virus_prior",
                               "dia_parasite_prior",
                               "dia_rotavirus_prior"),
                             c("Bacteria", "Virus","Parasite", "Rotavirus"),
                             "N"),
    events_plot = sub_plot(output, "Events",
                           c("graduation", "background_mortality"),
                           c("Graduation", "Background mortality"),
                           "N"),
    n_plot =  sub_plot(output, "N", "N", "N","N"),
    age_plot = sub_plot(output,
                        "Ages",
                        c("age_0", "age_1","age_2", "age_3", "age_4"),
                        c("0-1", "1-2", "2-3", "3-4", "4-5"),
                        "N"),
    average_age_plot =  sub_plot(output, "Average age", "average_age", "Average age", "Years")
  )

  if(!return_list){
    plots <- patchwork::wrap_plots(plots)
  }

  return(plots)
}


#' Sub plot
#'
#' Create sub-plots from names variables
#'
#' @param output Model output (long format)
#' @param group_vars Names of variables to plot together
#' @param group_name Name of group
#' @param var_names Vector of variable names to relabel variables with
#' @param ylab Y axis label
#'
#' @return A sub plot
sub_plot <- function(output, group_name, group_vars, var_names, ylab){
  subpd <- dplyr::filter(output, .data$variable %in% group_vars) %>%
    dplyr::mutate(variable = factor(.data$variable, levels = group_vars, labels = var_names))

  ggplot2::ggplot(subpd, ggplot2::aes(x = .data$timestep, y = .data$y, col = .data$variable)) +
    ggplot2::geom_line() +
    ggplot2::scale_color_discrete(name = "") +
    ggplot2::xlab("Time") +
    ggplot2::ylab(ylab) +
    ggplot2::theme_bw() +
    ggplot2::ggtitle(group_name)
}
