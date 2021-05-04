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
                             c("bacterial_diarrhoea_prevalence", "viral_diarrhoea_prevalence"),
                             c("Bacteria", "Virus"),
                             "Prevalence"),
    dia_inc_plot = sub_plot(output,
                            "Diarrhoea clinical incidence",
                            c("bacterial_diarrhoea_clinical_infection", "viral_diarrhoea_clinical_infection"),
                            c("Bacteria", "Virus"),
                            "N"),
    dia_sev_inc_plot = sub_plot(output,
                            "Diarrhoea severe incidence",
                            c("bacterial_diarrhoea_severe_incidence", "viral_diarrhoea_severe_incidence"),
                            c("Bacteria", "Virus"),
                            "N"),
    dia_death_plot = sub_plot(output,
                              "Diarrhoea mortality",
                              c("bacterial_diarrhoea_death", "viral_diarrhoea_death"),
                              c("Bacteria", "Virus"),
                              "N"),
    dia_prior_plot = sub_plot(output,
                              "Diarrhoea prior",
                              c("bacterial_diarrhoea_prior_exposure", "viral_diarrhoea_prior_exposure"),
                              c("Bacteria", "Virus"),
                              "N"),
    pneumonia_prev_plot = sub_plot(output,
                                   "Pneumonia prevalence",
                                   c("viral_pneumonia_prevalence"),
                                   c("Virus"),
                                   "Prevalence"),
    pneumonia_inc_plot = sub_plot(output,
                                  "Pneumonia clinical incidence",
                                  c("viral_pneumonia_clinical_infection"),
                                  c("Virus"),
                                  "N"),
    pneumonia_sev_inc_plot = sub_plot(output,
                                  "Pneumonia severe incidence",
                                  c("viral_pneumonia_severe_incidence"),
                                  c("Virus"),
                                  "N"),
    pneumonia_death_plot = sub_plot(output,
                                    "Pneumonia mortality",
                                    c("viral_pneumonia_death"),
                                    c("Virus"),
                                    "N"),
    pneumonia_prior_plot = sub_plot(output,
                                    "Pneumonia prior",
                                    c("viral_pneumonia_prior_exposure"),
                                    c("Virus"),
                                    "N"),
    malaria_prev_plot = sub_plot(output,
                                 "Malaria prevalence",
                                 c("plasmodium_falciparum_prevalence"),
                                 c("pf"),
                                 "Prevalence"),
    malaria_inc_plot = sub_plot(output,
                                "Malaria clinical incidence",
                                c("plasmodium_falciparum_clinical_infection"),
                                c("pf"),
                                "N"),
    malaria_sev_inc_plot = sub_plot(output,
                                "Malaria severe incidence",
                                c("plasmodium_falciparum_severe_incidence"),
                                c("pf"),
                                "N"),
    malaria_death_plot = sub_plot(output,
                                  "Malaria mortality",
                                  c("plasmodium_falciparum_death"),
                                  c("pf"),
                                  "N"),
    malaria_prior_plot = sub_plot(output,
                                  "Malaria prior",
                                  c("plasmodium_falciparum_prior_exposure"),
                                  c("pf"),
                                  "N"),
    HF_plot = sub_plot(output,
                       "HF activity",
                       c("hf_ors",
                         "hf_act",
                         "hf_severe_diarrhoea_tx",
                         "hf_severe_malaria_tx",
                         "hf_patients"),
                       c("ORS", "ACT", "Severe diarrhoea tx", "Severe malaria tx","Patients"),
                       "N"),
    fever_plot =  sub_plot(output, "Fevers", "fever_prevalence", "Fever", "Prevalence"),
    events_plot = sub_plot(output, "Events",
                           c("graduation"),
                           c("Graduation"),
                           "N"),
    pop_plot = sub_plot(output,
                        "Population",
                        c("N", "age_0", "age_1","age_2", "age_3", "age_4"),
                        c("All", "0-1", "1-2", "2-3", "3-4", "4-5"),
                        "N"),
    average_age_plot =  sub_plot(output, "Average age", "average_age", "Average age", "Years")
  )

  if(!return_list){
    plots <- patchwork::wrap_plots(plots, ncol = 5)
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
