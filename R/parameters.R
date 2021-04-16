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
    rotavirus_vx_coverage = 0,
    pneumococcal_vx_coverage = 0,
    hib_vx_coverage = 0
  )

  parameters$dia <- list(
    groups = 4,
    disease = c("bacteria", "virus", "parasite", "rotavirus"),
    index = c(1, 2, 3, 4),
    sigma = c(0.005, 0.005, 0.005, 0.005),
    # Average duration of clinical episode
    clin_dur = c(14, 14, 14, 14),
    asymp_dur = c(0, 0, 0, 0),
    daily_prob_severe = c(0.1, 0.2, 0.3, 0),
    daily_prob_death = c(0.001, 0.002, 0.003, 0),
    prob_fever = c(0.5, 0.5, 0.5, 0.5),
    # Maternal immunity
    ## Half life
    mi_hl = c(100, 100, 100, 100),
    # Infection immunity
    ## shape
    ii_shape = c(5, 5, 5, 5),
    ## rate
    ii_rate = c(1, 1, 1, 1),
    # Clinical immunity
    ## shape
    ci_shape = c(0, 0, 0, 0),
    ## rate
    ci_rate = c(0, 0, 0, 0),
    # Vaccination
    vx_start = c(0, 0, 0, 0),
    vx_initial_efficacy = c(0, 0, 0, 0),
    vx_hl = c(0, 0, 0, 0),
    # TODO this should be a function of coverage:
    vx_ci = c(0, 0, 0, 0),
    # Treatment prophylaxis
    prophylaxis_hl = c(30, 0, 0, 14),
    symptom_time_refer = 5
  )

  parameters$pneumonia <- list(
    groups = 5,
    disease = c("bacteria", "virus", "fungus", "pneumococcus", "hib"),
    index = c(1, 2, 3, 4, 5),
    sigma = c(0.005, 0.005, 0.005, 0.005, 0.005),
    # Average duration of clinical episode
    clin_dur = c(14, 14, 14, 14, 14),
    asymp_dur = c(0, 0, 0, 0, 0),
    daily_prob_severe = c(0.1, 0.2, 0.3, 0.04, 0.1),
    daily_prob_death = c(0.001, 0.002, 0.003, 0.002, 0.0001),
    prob_fever = c(0.5, 0.5, 0.5, 0.5, 0.5),
    # Maternal immunity
    ## Half life
    mi_hl = c(100, 100, 100, 100, 100),
    # Infection immunity
    ## shape
    ii_shape = c(5, 5, 5, 5, 5),
    ## rate
    ii_rate = c(1, 1, 1, 1, 1),
    # Clinical immunity
    ## shape
    ci_shape = c(0, 0, 0, 0, 0),
    ## rate
    ci_rate = c(0, 0, 0, 0, 0),
    # Vaccination
    vx_start = c(0, 0, 0, 0, 0),
    vx_initial_efficacy = c(0, 0, 0, 0, 0),
    vx_hl = c(0, 0, 0, 0, 0),
    # TODO this should be a function of coverage:
    vx_ci = c(0, 0, 0, 0, 0),
    # Treatment prophylaxis
    prophylaxis_hl = c(30, 0, 0, 14, 0),
    symptom_time_refer = 5
  )

  parameters$malaria <- list(
    groups = 1,
    disease = "pf",
    index = 1,
    sigma = 0.001,
    # Average duration of clinical episode
    clin_dur = 14,
    asymp_dur = 50,
    daily_prob_severe = 0.1,
    daily_prob_death = 0.001,
    prob_fever = 0.1,
    # Maternal immunity
    ## Half life
    mi_hl = 100,
    # Infection immunity
    ## shape
    ii_shape = 5,
    ## rate
    ii_rate = 1,
    # Clinical immunity
    ## shape
    ci_shape = 5,
    ## rate
    ci_rate = 1,
    # Vaccination
    vx_start = 0,
    vx_initial_efficacy = 0,
    vx_hl = 0,
    # TODO this should be a function of coverage:
    vx_ci = 0,
    # Treatment prophylaxis
    prophylaxis_hl = 28,
    llin_efficacy = 0.5,
    symptom_time_refer = 5
  )

  parameters$treatment_seeking <- list(
    prob_seek_treatment = 0,
    prob_seek_treatment_severe = 1,
    treat_seeking_behaviour_delay = 1,
    provider_preference_weights = c(1, 1, 1)
  )

  parameters$dx_tx <- list(
    rdt_sensitivity = 1,
    rdt_specificity = 1,
    ors_efficacy = 0.75,
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
    severe_malaria_efficacy = 1
  )

  parameters$chw <- list(
    chw = 0,
    travel_time = 1,
    efficacy = 1,
    severe_diarrhoea_sensitivity = 1,
    severe_diarrhoea_specificity = 1,
    severe_diarrhoea_efficacy = 1,
    diarrhoea_sensitivity = 1,
    diarrhoea_specificity = 1,
    severe_malaria_sensitivity = 1,
    severe_malaria_specificity = 1,
    followup_period = 14
  )

  parameters$private <- list(
    private = 0,
    travel_time = 1,
    efficacy = 1,
    severe_diarrhoea_sensitivity = 1,
    severe_diarrhoea_specificity = 1,
    severe_diarrhoea_efficacy = 1,
    diarrhoea_sensitivity = 1,
    diarrhoea_specificity = 1
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
