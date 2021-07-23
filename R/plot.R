#' Plot simulation output
#'
#' @param output Long simulation output
#' @param return_list Return list of component plots
#'
#' @return Output plot, one panel for each variable
#' @export
plot_sim <- function(output, return_list = FALSE) {

  dia <- c("bacterial_diarrhoea", "parasitic_diarrhoea", "viral_diarrhoea", "rotavirus")
  dia_names <- c("Bacterial", "Parasitic", "Viral", "Rotavirus")

  pneumo <- c("bacterial_pneumonia", "fungal_pneumonia", "viral_pneumonia", "pneumococcus", "hib")
  pneumo_names <- c("Bacterial", "Fungal", "Viral", "Pneumococcus", "Hib")

  malaria <- c("plasmodium_falciparum")
  malaria_names <- c("Pf")

  plots <- list(
    dia_prev_plot = sub_plot(output,
                             "Diarrhoea prevalence",
                             paste0(dia, "_prevalence"),
                             dia_names,
                             "Prevalence"),
    dia_inc_plot = sub_plot(output,
                            "Diarrhoea clinical incidence",
                            paste0(dia, "_clinical_infection"),
                            dia_names,
                            "N"),
    dia_sev_inc_plot = sub_plot(output,
                                "Diarrhoea severe incidence",
                                paste0(dia, "_severe_incidence"),
                                dia_names,
                                "N"),
    dia_death_plot = sub_plot(output,
                              "Diarrhoea mortality",
                              paste0(dia, "_death"),
                              dia_names,
                              "N"),
    dia_prior_plot = sub_plot(output,
                              "Diarrhoea prior",
                              paste0(dia, "_prior_exposure"),
                              dia_names,
                              "N"),
    pneumonia_prev_plot = sub_plot(output,
                                   "Pneumonia prevalence",
                                   paste0(pneumo, "_prevalence"),
                                   pneumo_names,
                                   "Prevalence"),
    pneumonia_inc_plot = sub_plot(output,
                                  "Pneumonia clinical incidence",
                                  paste0(pneumo, "_clinical_infection"),
                                  pneumo_names,
                                  "N"),
    pneumonia_sev_inc_plot = sub_plot(output,
                                      "Pneumonia severe incidence",
                                      paste0(pneumo, "_severe_incidence"),
                                      pneumo_names,
                                      "N"),
    pneumonia_death_plot = sub_plot(output,
                                    "Pneumonia mortality",
                                    paste0(pneumo, "_death"),
                                    pneumo_names,
                                    "N"),
    pneumonia_prior_plot = sub_plot(output,
                                    "Pneumonia prior",
                                    paste0(pneumo, "_prior_exposure"),
                                    pneumo_names,
                                    "N"),
    malaria_prev_plot = sub_plot(output,
                                 "Malaria prevalence",
                                 paste0(malaria, "_prevalence"),
                                 malaria_names,
                                 "Prevalence"),
    malaria_inc_plot = sub_plot(output,
                                "Malaria clinical incidence",
                                paste0(malaria, "_clinical_infection"),
                                malaria_names,
                                "N"),
    malaria_sev_inc_plot = sub_plot(output,
                                    "Malaria severe incidence",
                                    paste0(malaria, "_severe_incidence"),
                                    malaria_names,
                                    "N"),
    malaria_death_plot = sub_plot(output,
                                  "Malaria mortality",
                                  paste0(malaria, "_death"),
                                  malaria_names,
                                  "N"),
    malaria_prior_plot = sub_plot(output,
                                  "Malaria prior",
                                  paste0(malaria, "_prior_exposure"),
                                  malaria_names,
                                  "N"),
    HF_plot = sub_plot(output,
                       "HF activity",
                       c("hf_ors",
                         "hf_act",
                         "hf_amoxicillin",
                         "hf_severe_diarrhoea_tx",
                         "hf_severe_malaria_tx",
                         "hf_severe_pneumonia_tx"),
                       c("ORS", "ACT", "Amoxicillin", "Severe diarrhoea tx", "Severe malaria tx", "Severe pneumonia tx"),
                       "N"),
    CHW_plot = sub_plot(output,
                        "CHW activity",
                        c("chw_ors",
                          "chw_act",
                          "chw_amoxicillin",
                          "chw_referral",
                          "chw_followup"),
                        c("ORS", "ACT", "Amoxicillin", "Referrals", "Followup"),
                        "N"),
    private_plot = sub_plot(output,
                            "Private activity",
                            c("private_ors",
                              "private_act",
                              "private_amoxicillin"),
                            c("ORS", "ACT", "Amoxicillin"),
                            "N"),
    patients = sub_plot(output,
                        "Patients",
                        c("hf_patients", "chw_patients", "private_patients"),
                        c("HF", "CHW", "Private"),
                        "N"),
    fever_plot =  sub_plot(output, "Fevers", "fever_prevalence", "Fever", "Prevalence"),
    events_plot = sub_plot(output, "Events",
                           c("graduation", "other_death"),
                           c("Graduation", "Death (other)"),
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
#' @param smooth Apply 2 month rolling average smoothing
#'
#' @return A sub plot
sub_plot <- function(output, group_name, group_vars, var_names, ylab, smooth = TRUE){
  subpd <- dplyr::filter(output, .data$variable %in% group_vars) %>%
    dplyr::mutate(variable = factor(.data$variable, levels = group_vars, labels = var_names)) %>%
    dplyr::group_by(.data$variable) %>%
    dplyr::mutate(rolly = RcppRoll::roll_mean(.data$y, n = 60, fill = NA)) %>%
    dplyr::ungroup()

  if(smooth){
    p1 <- ggplot2::ggplot(subpd, ggplot2::aes(x = .data$timestep, col = .data$variable)) +
      ggplot2::geom_line(ggplot2::aes(y = .data$y), alpha = 0.25) +
      ggplot2::geom_line(ggplot2::aes(y = .data$rolly), na.rm=TRUE) +
      ggplot2::scale_color_discrete(name = "") +
      ggplot2::xlab("Time") +
      ggplot2::ylab(ylab) +
      ggplot2::theme_bw() +
      ggplot2::ggtitle(group_name)
  } else {
    p1 <- ggplot2::ggplot(subpd, ggplot2::aes(x = .data$timestep, y = .data$y, col = .data$variable)) +
      ggplot2::geom_line() +
      ggplot2::scale_color_discrete(name = "") +
      ggplot2::xlab("Time") +
      ggplot2::ylab(ylab) +
      ggplot2::theme_bw() +
      ggplot2::ggtitle(group_name)
  }

  return(p1)
}
