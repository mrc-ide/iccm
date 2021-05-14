#' @title Get model parameters
#' @description
#' Create a named list of parameters for use in the model.
#'
#' @param user_overwrite User overwrites to default parameters
#'
#' The parameters are defined below.
#'
#' \strong{Demography:}
#'
#' * pop - Population size
#' * average_age - The average lifespan of an individual within the population
#' * age_upper - The upper limit to modelled ages
#'
get_parameters <- function(user_overwrite = NULL){

  parameters <- list(
    # Demography
    population = 1000,
    average_age = 60 * 365,
    age_lower = 30,
    age_upper = 5 * 365 - 1,
    # Epidemiology
    het_sd = 0,#1.6,
    # Interventions
    llin_coverage = 0,
    rotavirus_vx_coverage = 0.8,
    pneumococcal_vx_coverage = 0,
    hib_vx_coverage = 0
  )

  parameters$disease = list(
    bacterial_diarrhoea = list(
      type = "diarrhoea",
      sigma = 0.0001,
      clinical_duration = 14,
      asymptomatic_duration = 0,
      daily_probability_severe = 0.000001,
      severe_duration = 14,
      daily_probability_death = 0.000001,
      probability_fever = 0.5,
      maternal_immunity_halflife = 100,
      infection_immunity_shape = 5,
      infection_immunity_rate = 1,
      clinical_immunity_shape = 0,
      clinical_immunity_rate = 0,
      vaccine = FALSE,
      asymptomatic_pathway = FALSE,
      amoxicillin_efficacy = 0.8
    ),
    viral_diarrhoea = list(
      type = "diarrhoea",
      sigma = 0.001,
      clinical_duration = 14,
      asymptomatic_duration = 0,
      daily_probability_severe = 0.000001,
      severe_duration = 14,
      daily_probability_death = 0.001,
      probability_fever = 0.5,
      maternal_immunity_halflife = 100,
      infection_immunity_shape = 5,
      infection_immunity_rate = 1,
      clinical_immunity_shape = 0,
      clinical_immunity_rate = 0,
      vaccine = FALSE,
      asymptomatic_pathway = FALSE
    ),
    rotavirus = list(
      type = "diarrhoea",
      sigma = 0.001,
      clinical_duration = 14,
      asymptomatic_duration = 0,
      daily_probability_severe = 0.000001,
      severe_duration = 14,
      daily_probability_death = 0.001,
      probability_fever = 0.5,
      maternal_immunity_halflife = 100,
      infection_immunity_shape = 5,
      infection_immunity_rate = 1,
      clinical_immunity_shape = 0,
      clinical_immunity_rate = 0,
      vaccine = TRUE,
      vx_start = 100,
      vx_initial_efficacy = 0.8,
      vx_hl = 365,
      asymptomatic_pathway = FALSE
    ),
    plasmodium_falciparum = list(
      type = "malaria",
      sigma = 0.01,
      clinical_duration = 14,
      asymptomatic_duration = 30,
      daily_probability_severe = 0.000001,
      severe_duration = 14,
      daily_probability_death = 0.001,
      probability_fever = 0.5,
      maternal_immunity_halflife = 100,
      infection_immunity_shape = 5,
      infection_immunity_rate = 1,
      clinical_immunity_shape = 5,
      clinical_immunity_rate = 1,
      vaccine = FALSE,
      asymptomatic_pathway = TRUE,
      llin_efficacy = 0.8
    ),
    viral_pneumonia = list(
      type = "pneumonia",
      sigma = 0.1,
      clinical_duration = 14,
      asymptomatic_duration = 0,
      daily_probability_severe = 0.000001,
      severe_duration = 1,
      daily_probability_death = 0.001,
      probability_fever = 0.5,
      maternal_immunity_halflife = 100,
      infection_immunity_shape = 5,
      infection_immunity_rate = 1,
      clinical_immunity_shape = 0,
      clinical_immunity_rate = 0,
      vaccine = FALSE,
      asymptomatic_pathway = FALSE
    )
  )

  parameters$treatment_seeking <- list(
    prob_seek_treatment = 0,
    prob_seek_treatment_severe = 0,
    treat_seeking_behaviour_delay = 1,
    provider_preference_weights = c(1, 1, 1)
  )

  parameters$dx_tx <- list(
    rdt_sensitivity = 1,
    rdt_specificity = 1,
    ors_efficacy = 0.75,
    ors_efficacy_severe = 0.75,
    act_efficacy = 0.95
  )

  parameters$hf <- list(
    hf = 1,
    travel_time = 3,
    efficacy = 1,
    severe_diarrhoea_sensitivity = 1,
    severe_diarrhoea_specificity = 1,
    severe_diarrhoea_efficacy = 1,
    diarrhoea_sensitivity = 1,
    diarrhoea_specificity = 1,
    severe_malaria_sensitivity = 1,
    severe_malaria_specificity = 1,
    severe_malaria_efficacy = 1,
    severe_pneumonia_sensitivity = 1,
    severe_pneumonia_specificity = 1,
    pneumonia_sensitivity = 1,
    pneumonia_specificity = 1
  )

  parameters$chw <- list(
    chw = 1,
    travel_time = 0,
    efficacy = 1,
    severe_diarrhoea_sensitivity = 1,
    severe_diarrhoea_specificity = 1,
    severe_diarrhoea_efficacy = 1,
    diarrhoea_sensitivity = 1,
    diarrhoea_specificity = 1,
    severe_malaria_sensitivity = 1,
    severe_malaria_specificity = 1,
    severe_malaria_efficacy = 1,
    severe_pneumonia_sensitivity = 1,
    severe_pneumonia_specificity = 1,
    pneumonia_sensitivity = 1,
    pneumonia_specificity = 1,
    followup_period = 14,
    diarrhoea_long_symptoms  = 14,
    malaria_long_symptoms  = 14,
    pneumonia_long_symptoms  = 14
  )

  parameters$private <- list(
    private = 1,
    travel_time = 1,
    efficacy = 1,
    severe_diarrhoea_efficacy = 1,
    diarrhoea_sensitivity = 1,
    diarrhoea_specificity = 1,
    pneumonia_sensitivity = 1,
    pneumonia_specificity = 1
  )

  # Overwrite_defaults
  if(!is.null(user_overwrite)){
    parameters <- overwrite_params(parameters, user_overwrite)
  }

  return(parameters)
}

#' Extract names from nested list
#'
#' @param x List
#'
#' @return Names from nested list. Levels are separated by ".".
nested_list_names <- function(x){
  names(rapply(x, function(y)  utils::head(y, 1)))
}

#' Overwrite default parameters
#'
#' @param parameters Default parameters
#' @param user_overwrite User input (nested) list of parameters
#'
#' @return Set of user-modified simulation parameters
overwrite_params <- function(parameters, user_overwrite){
  stopifnot(is.list(user_overwrite))
  # Check user input parameter names are all recognised
  missing <- setdiff(nested_list_names(user_overwrite), nested_list_names(parameters))
  if(length(missing) > 0){
    missing_names <- gsub("[.]", "$", missing)
    stop("User input parameter(s) not recognised: ", paste(missing_names, collapse = " "))
  }
  # Overwrite defaults
  parameters <- utils::modifyList(parameters, user_overwrite)
  return(parameters)
}
