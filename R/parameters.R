#' @title Get model parameters
#' @description
#' Create a named list of parameters for use in the model.
#' The parameters are defined below.
#'
#' \strong{Demography:}
#'
#' \describe{
#'  \item{pop}{Population size}
#'  \item{average_age}{The average lifespan of an individual within the population}
#'  \item{age_upper}{The upper limit to modelled ages}
#' }
#'
#' \strong{Disease:}
#'
#' Diseases may be added to the nested disease list. All parameter options are
#' documented below.
#'
#' \describe{
#'  \item{type}{Disease type. Current supported types are: diarrhoea, malaria, pneumonia}
#'  \item{sigma}{Infection rate. This will be setting specific}
#'  \item{clinical_duration}{The average duration of a clinical episode}
#'  \item{daily_probability_severe}{The daily probability of developing a severe infection | a clinical infection}
#'  \item{severe_duration}{The average duration of a severe episode}
#'  \item{daily_probability_death}{Daily probability of dying | a severe infection}
#'  \item{probability_fever}{Probability of fever with clinical disease}
#'  \item{maternal_immunity_halflife}{Average duration of protection from maternal immunity}
#'  \item{infection_immunity_shape}{Infection immunity shape parameter}
#'  \item{infection_immunity_rate}{Infection immunity rate parameter}
#'  \item{clinical_immunity_shape}{Clinical immunity shape parameter}
#'  \item{clinical_immunity_rate}{Clinical immunity rate parameter}
#'  \item{vaccine_coverage}{Vaccine coverage (set to 0 for no vaccination)}
#'  \item{asymptomatic_pathway}{Boolean indicator if asmptomatic infections are to be modelled}
#'  \item{asymptomatic_duration}{Duration of asymptomatic infection (if modelled)}
#'  \item{amoxicillin_efficacy}{Efficacy of amoxicillin against this infection}
#' }
#'
#' \strong{Treatment seeking:}
#'
#' \describe{
#'   \item{prob_seek_treatment}{Baseline probability of seeking treatment given symptom onset}
#'   \item{treat_seeking_behaviour_delay}{Average delay between symptom onset and seeking treatment}
#'   \item{prob_seek_treatment_severe}{Baseline probability of seeking treatment given onset of severe disease}
#'   \item{provider_preference_weights}{Population-level preference weights for treatment providers}
#' }
#'
#' \strong{dx_tx:}
#'
#' \describe{
#'   \item{rdt_sensitivity}{Sensitivity of malaria rapid diagnostic test}
#'   \item{rdt_sepcificity}{Specificity of malaria rapid diagnostic test}
#'   \item{ors_efficacy}{Efficacy of oral rehydration therapy against diarrhoea}
#'   \item{act_efficacy}{Efficacy of artemisinin combination therapy against malaria}
#'   \item{act_halflife}{Halflife of artemisinin combination therapy}
#'   \item{introduction_time}{Introduction time of CHW to community}
#'   \item{followup_period}{Period between initial visit to a CHW and scheduled followup}
#'   \item{_long_symptoms}{Threshold periods for diarrhoea, malaria and pneumonia that indicate a more severe illness}
#' }
#'
#' \strong{Healthcare providers:}
#'
#' Most definitions are shared across providers (although parameterisations are unique to each). Some are specific.
#'
#' \describe{
#'   \item{efficacy}{This efficacy parameter captures all "human error" from the provider}
#'   \item{_sensitivity}{Sensitivity for named conditions. These are sensitivities associated with clinical diagnoses}
#'   \item{_specificity}{Specificity for named conditions. These are specificites associated with clinical diagnoses}
#'   \item{travel_time}{Travel time to reach provider}
#' }
#'
#' @param user_overwrite User overwrites to default parameters
get_parameters <- function(user_overwrite = NULL){

  parameters <- list(
    # Demography
    population = 1000,
    average_age = 60 * 365,
    age_lower = 30,
    age_upper = 5 * 365 - 1,
    # Epidemiology
    het_sd = 0, #1.6,
    llin_coverage = 0
  )

  parameters$disease = list(
    bacterial_diarrhoea = list(
      type = "diarrhoea",
      sigma = 0.0001,
      clinical_duration = 14,
      daily_probability_severe = 0.01,
      severe_duration = 14,
      daily_probability_death = 0.000001,
      probability_fever = 0.5,
      maternal_immunity_halflife = 100,
      infection_immunity_shape = 5,
      infection_immunity_rate = 1,
      clinical_immunity_shape = 0,
      clinical_immunity_rate = 0,
      vaccine_coverage = 0,
      asymptomatic_pathway = FALSE,
      amoxicillin_efficacy = 0.8
    ),
    viral_diarrhoea = list(
      type = "diarrhoea",
      sigma = 0.001,
      clinical_duration = 14,
      daily_probability_severe = 0.0001,
      severe_duration = 14,
      daily_probability_death = 0.01,
      probability_fever = 0.5,
      maternal_immunity_halflife = 100,
      infection_immunity_shape = 5,
      infection_immunity_rate = 1,
      clinical_immunity_shape = 0,
      clinical_immunity_rate = 0,
      vaccine_coverage = 0,
      asymptomatic_pathway = FALSE,
      amoxicillin_efficacy = 0
    ),
    parasitic_diarrhoea = list(
      type = "diarrhoea",
      sigma = 0.001,
      clinical_duration = 14,
      daily_probability_severe = 0.0001,
      severe_duration = 14,
      daily_probability_death = 0.01,
      probability_fever = 0.5,
      maternal_immunity_halflife = 100,
      infection_immunity_shape = 5,
      infection_immunity_rate = 1,
      clinical_immunity_shape = 0,
      clinical_immunity_rate = 0,
      vaccine_coverage = 0,
      asymptomatic_pathway = FALSE,
      amoxicillin_efficacy = 0
    ),
    rotavirus = list(
      type = "diarrhoea",
      sigma = 0.001,
      clinical_duration = 14,
      daily_probability_severe = 0.01,
      severe_duration = 14,
      daily_probability_death = 0.01,
      probability_fever = 0.5,
      maternal_immunity_halflife = 100,
      infection_immunity_shape = 5,
      infection_immunity_rate = 1,
      clinical_immunity_shape = 0,
      clinical_immunity_rate = 0,
      vaccine_coverage = 1,
      vaccine_start = 100,
      vaccine_initial_efficacy = 0.8,
      vaccine_hl = 365,
      asymptomatic_pathway = FALSE,
      amoxicillin_efficacy = 0
    ),
    plasmodium_falciparum = list(
      type = "malaria",
      sigma = 0.01,
      clinical_duration = 14,
      asymptomatic_pathway = TRUE,
      asymptomatic_duration = 30,
      daily_probability_severe = 0.001,
      severe_duration = 14,
      daily_probability_death = 0.01,
      probability_fever = 0.5,
      maternal_immunity_halflife = 100,
      infection_immunity_shape = 5,
      infection_immunity_rate = 1,
      clinical_immunity_shape = 5,
      clinical_immunity_rate = 1,
      vaccine_coverage = 0,
      llin_efficacy = 0.8,
      amoxicillin_efficacy = 0
    ),
    bacterial_pneumonia = list(
      type = "pneumonia",
      sigma = 0.1,
      clinical_duration = 14,
      daily_probability_severe = 0.001,
      severe_duration = 1,
      daily_probability_death = 0.01,
      probability_fever = 0.5,
      maternal_immunity_halflife = 100,
      infection_immunity_shape = 5,
      infection_immunity_rate = 1,
      clinical_immunity_shape = 0,
      clinical_immunity_rate = 0,
      vaccine_coverage = 0,
      asymptomatic_pathway = FALSE,
      amoxicillin_efficacy = 0.8
    ),
    fungal_pneumonia = list(
      type = "pneumonia",
      sigma = 0.1,
      clinical_duration = 14,
      daily_probability_severe = 0.01,
      severe_duration = 1,
      daily_probability_death = 0.01,
      probability_fever = 0.5,
      maternal_immunity_halflife = 100,
      infection_immunity_shape = 5,
      infection_immunity_rate = 1,
      clinical_immunity_shape = 0,
      clinical_immunity_rate = 0,
      vaccine_coverage = 0,
      asymptomatic_pathway = FALSE,
      amoxicillin_efficacy = 0
    ),
    viral_pneumonia = list(
      type = "pneumonia",
      sigma = 0.1,
      clinical_duration = 14,
      daily_probability_severe = 0.001,
      severe_duration = 1,
      daily_probability_death = 0.01,
      probability_fever = 0.5,
      maternal_immunity_halflife = 100,
      infection_immunity_shape = 5,
      infection_immunity_rate = 1,
      clinical_immunity_shape = 0,
      clinical_immunity_rate = 0,
      vaccine_coverage = 0,
      asymptomatic_pathway = FALSE,
      amoxicillin_efficacy = 0
    ),
    pneumococcus = list(
      type = "pneumonia",
      sigma = 0.1,
      clinical_duration = 14,
      daily_probability_severe = 0.001,
      severe_duration = 1,
      daily_probability_death = 0.01,
      probability_fever = 0.5,
      maternal_immunity_halflife = 100,
      infection_immunity_shape = 5,
      infection_immunity_rate = 1,
      clinical_immunity_shape = 0,
      clinical_immunity_rate = 0,
      vaccine_coverage = 0.8,
      vaccine_start = 100,
      vaccine_initial_efficacy = 0.8,
      vaccine_hl = 365,
      asymptomatic_pathway = FALSE,
      amoxicillin_efficacy = 0
    ),
    hib = list(
      type = "pneumonia",
      sigma = 0.1,
      clinical_duration = 14,
      daily_probability_severe = 0.001,
      severe_duration = 1,
      daily_probability_death = 0.01,
      probability_fever = 0.5,
      maternal_immunity_halflife = 100,
      infection_immunity_shape = 5,
      infection_immunity_rate = 1,
      clinical_immunity_shape = 0,
      clinical_immunity_rate = 0,
      vaccine_coverage = 0.8,
      vaccine_start = 100,
      vaccine_initial_efficacy = 0.8,
      vaccine_hl = 365,
      asymptomatic_pathway = FALSE,
      amoxicillin_efficacy = 0
    )
  )

  parameters$treatment_seeking <- list(
    prob_seek_treatment = 0.2,
    prob_seek_treatment_severe = 0.3,
    treat_seeking_behaviour_delay = 1,
    provider_preference_weights = c(1, 1, 1)
  )

  parameters$dx_tx <- list(
    rdt_sensitivity = 1,
    rdt_specificity = 1,
    ors_efficacy = 0.75,
    ors_efficacy_severe = 0.75,
    act_efficacy = 0.95,
    act_halflife = 2000
  )

  parameters$hf <- list(
    hf = 1,
    travel_time = 10,
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
    introduction_time = 365,
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
    private = 0,
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
