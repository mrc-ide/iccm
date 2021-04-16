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
                             c("dia_incidence",
                               "dia_bacteria_incidence",
                               "dia_virus_incidence",
                               "dia_parasite_incidence",
                               "dia_rotavirus_incidence"),
                             c("All", "Bacteria", "Virus","Parasite", "Rotavirus"),
                             "Incidence"),
    dia_mort_plot = sub_plot(output,
                            "Diarrhoea mortality",
                            c("dia_bacteria_mortality",
                              "dia_virus_mortality",
                              "dia_parasite_mortality",
                              "dia_rotavirus_mortality"),
                            c("Bacteria", "Virus","Parasite", "Rotavirus"),
                            "Mortality"),
    dia_prior_plot = sub_plot(output,
                             "Diarrhoea prior",
                             c("dia_prior_bacteria",
                               "dia_prior_virus",
                               "dia_prior_parasite",
                               "dia_prior_rotavirus"),
                             c("Bacteria", "Virus","Parasite", "Rotavirus"),
                             "N"),
    pneumonia_prev_plot = sub_plot(output,
                             "pneumonia prevalence",
                             c("pneumonia_prevalence",
                               "pneumonia_bacteria_prevalence",
                               "pneumonia_virus_prevalence",
                               "pneumonia_fungus_prevalence",
                               "pneumonia_pneumococcus_prevalence",
                               "pneumonia_hib_prevalence"),
                             c("All", "Bacteria", "Virus","Fungus", "Pneumococcus", "hib"),
                             "Prevalence"),
    pneumonia_inc_plot = sub_plot(output,
                            "pneumonia incidence",
                            c("pneumonia_incidence",
                              "pneumonia_bacteria_incidence",
                              "pneumonia_virus_incidence",
                              "pneumonia_fungus_incidence",
                              "pneumonia_pneumococcus_incidence",
                              "pneumonia_hib_incidence"),
                            c("All", "Bacteria", "Virus","Fungus", "Pneumococcus", "hib"),
                            "Incidence"),
    pneumonia_mort_plot = sub_plot(output,
                             "pneumonia mortality",
                             c("pneumonia_bacteria_mortality",
                               "pneumonia_virus_mortality",
                               "pneumonia_fungus_mortality",
                               "pneumonia_pneumococcus_mortality",
                               "pneumonia_hib_mortality"),
                             c("Bacteria", "Virus","Fungus", "Pneumococcus", "hib"),
                             "Mortality"),
    pneumonia_prior_plot = sub_plot(output,
                              "pneumonia prior",
                              c("pneumonia_prior_bacteria",
                                "pneumonia_prior_virus",
                                "pneumonia_prior_fungus",
                                "pneumonia_prior_pneumococcus",
                                "pneumonia_prior_hib"),
                              c("Bacteria", "Virus","Fungus", "Pneumococcus", "hib"),
                              "N"),
    malaria_prev_plot = sub_plot(output,
                             "Malaria prevalence",
                             c("malaria_prevalence",
                               "malaria_pf_prevalence"),
                             c("All", "pf"),
                             "Prevalence"),
    malaria_inc_plot = sub_plot(output,
                            "Malaria incidence",
                            c("malaria_incidence",
                              "malaria_pf_incidence"),
                            c("All", "pf"),
                            "Incidence"),
    malaria_mort_plot = sub_plot(output,
                             "Malaria mortality",
                             c("malaria_pf_mortality"),
                             "pf",
                             "Mortality"),
    malaria_prior_plot = sub_plot(output,
                              "Malaria prior",
                              c("malaria_prior_pf"),
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
    chw_plot = sub_plot(output,
                       "CHW activity",
                       c("chw_referral", "chw_ors",
                         "chw_patients", "chw_followup"),
                       c("Referrals", "ORS", "Patients", "followups"),
                       "N"),
    private_plot = sub_plot(output,
                       "Private provider activity",
                       c("private_ors", "private_act", "private_patients"),
                       c("ORS", "ACT", "Patients"),
                       "N"),
    fever_plot =  sub_plot(output, "Fevers", "Fever_prevalence", "Fever", "Prevalence"),
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
    plots <- patchwork::wrap_plots(plots, ncol = 4)
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
