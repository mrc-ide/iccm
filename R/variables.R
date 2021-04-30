#' Create individual variables
#'
#' @param parameters Model parameters
create_variables <- function(parameters){
  variables <- list()

  # Demography
  initial_age <- floor(rtexp(parameters$population, rate = 1 / parameters$average_age, lower = parameters$age_lower, upper = parameters$age_upper))
  variables$birth_t <- individual::DoubleVariable$new(-initial_age)

  # Epidemiology
  est_het <- heterogeneity(parameters$population, parameters$het_sd)
  variables$het <- individual::DoubleVariable$new(est_het)

  # Diseases
  ## Symptom onset
  variables$diarrhoea_symptom_onset <- individual::IntegerVariable$new(initial_values = rep(NA, parameters$population))
  variables$malaria_symptom_onset <- individual::IntegerVariable$new(initial_values = rep(NA, parameters$population))
  variables$pneumonia_symptom_onset <- individual::IntegerVariable$new(initial_values = rep(NA, parameters$population))
  ## Prior exposure
  for(disease in names(parameters$disease)){
    prior <- prior_exposure_equilibrium(parameters$disease[[disease]], initial_age, parameters$age_upper, est_het)
    variables[[paste0(disease, "_prior_exposure")]] <- individual::IntegerVariable$new(initial_values = prior)
  }
  ## Infection status
  for(disease in names(parameters$disease)){
    variables[[paste0(disease, "_status")]] <- individual::CategoricalVariable$new(c("uninfected", "asymptomatic", "symptomatic", "severe"), rep("uninfected", parameters$population))
  }
  ## Fever status
  for(disease in names(parameters$disease)){
    variables[[paste0(disease, "_fever")]] <- individual::CategoricalVariable$new(c("nonfebrile", "febrile"), rep("nonfebrile", parameters$population))
  }
  ## Fever status

  # Treatment
  # Treatment seeking
  est_provider_preference <- sample_preference(parameters$population, parameters)
  variables$provider_preference <- individual::CategoricalVariable$new(c("none", "hf", "chw", "private"), est_provider_preference)
  variables$awaiting_followup <- individual::IntegerVariable$new(rep(0,  parameters$population))
  # Previous drugs (for prophylaxis)
  variables$time_of_last_act <- individual::IntegerVariable$new(rep(NA,  parameters$population))
  variables$time_of_last_amoxicillin <- individual::IntegerVariable$new(rep(NA,  parameters$population))

  # Interventions
  variables$llin <- individual::IntegerVariable$new(stats::rbinom(parameters$population, 1, parameters$llin_coverage))
  variables$rotavirus_vx <- individual::IntegerVariable$new(stats::rbinom(parameters$population, 1, parameters$rotavirus_vx_coverage))
  variables$pneumococcal_vx <- individual::IntegerVariable$new(stats::rbinom(parameters$population, 1, parameters$pneumococcal_vx_coverage))
  variables$hib_vx <- individual::IntegerVariable$new(stats::rbinom(parameters$population, 1, parameters$hib_vx_coverage))

  return(variables)
}

#' Add default values to render variables that won't get called every timestep
#'
#' @param renderer Model renderer
#' @param zero_default Variables to set a default 0
initialise_render_defaults <- function(renderer, zero_default = c("chw_patients", "chw_ors", "chw_followup", "chw_referral",
                                                                  "private_patients", "private_ors",
                                                                  "hf_patients", "hf_ors", "hf_severe_diarrhoea_tx",
                                                                  "graduation", "dia_bacteria_mortality",
                                                                  "dia_virus_mortality", "dia_parasite_mortality",
                                                                  "dia_rotavirus_mortality", "malaria_pf_mortality",
                                                                  "pneumonia_bacteria_mortality", "pneumonia_virus_mortality",
                                                                  "pneumonia_fungus_mortality", "pneumonia_pneumococcus_mortality",
                                                                  "pneumonia_hib_mortality", "malaria_pf_mortality",
                                                                  "dia_bacteria_incidence",
                                                                  "dia_virus_incidence", "dia_parasite_incidence",
                                                                  "dia_rotavirus_incidence", "malaria_pf_incidence",
                                                                  "pneumonia_bacteria_incidence", "pneumonia_virus_incidence",
                                                                  "pneumonia_fungus_incidence", "pneumonia_pneumococcus_incidence",
                                                                  "pneumonia_hib_incidence", "malaria_pf_incidence")){
  for(var in zero_default){
    renderer$set_default(var, 0)
  }
}


#' Prior exposure equilibrium
#'
#' @param p Condition parameters
#' @param initial_age Age of each child
#' @param maximum_age Maximum age
#' @param est_het Heterogeneity of each child variable
prior_exposure_equilibrium <- function(p, initial_age, maximum_age, est_het){
  ages <- 1:maximum_age
  vx <- rep(1, length(ages))
  if(p$vaccine){
    vx <- 1 - vaccine_effect(ages, p$vx_start[i], p$vx_initial_efficacy[i], p$vx_hl[i])
  }
  mi <- maternal_immunity(ages, p$maternal_immunity_halflife)
  dp <- rep(NA, parameters$population)
  for(i in 1:parameters$population){
    dp[i] <- round(eq_prior_indiv(initial_age[i],
                                  p$sigma,
                                  est_het[i],
                                  vx,
                                  mi,
                                  p$infection_immunity_shape,
                                  p$infection_immunity_rate))
  }
  return(dp)
}
