#' Create individual variables
#'
#' @param parameters Model parameters
create_variables <- function(parameters){
  variables <- list()

  # Demography
  initial_age <- floor(rtexp(parameters$population, rate = 1 / parameters$average_age, lower = parameters$age_lower, upper = parameters$age_upper))
  variables$birth_t <- individual::DoubleVariable$new(-initial_age)

  # Heterogeneity
  est_het <- heterogeneity(parameters$population, parameters$het_sd)
  variables$heterogeneity <- individual::DoubleVariable$new(est_het)

  # Diseases
  n_disease <- length(parameters$disease)
  disease_states <- c("uninfected", "asymptomatic", "symptomatic", "severe")
  fever_states <- c("nonfebrile", "febrile")
  variables$prior_exposure <- list()
  variables$infection_status <- list()
  variables$fever <- list()
  variables$symptom_onset <- list()
  for(disease in 1:n_disease){
    ## Prior exposure
    prior <- round(prior_exposure_equilibrium(parameters$disease[[disease]], parameters$population, initial_age, parameters$age_upper, est_het))
    variables$prior_exposure[[disease]] <- individual::IntegerVariable$new(initial_values = prior)
    ## Infection status
    variables$infection_status[[disease]] <- individual::CategoricalVariable$new(disease_states, rep("uninfected", parameters$population))
    ## Fever status
    variables$fever[[disease]] <- individual::CategoricalVariable$new(fever_states, rep("nonfebrile", parameters$population))
    ## Symptom onset
    variables$symptom_onset[[disease]] <- individual::IntegerVariable$new(initial_values = rep(NA, parameters$population))
    ## Vaccination status
    variables$vaccine[[disease]] <- individual::IntegerVariable$new(initial_values = stats::rbinom(parameters$population, 1, parameters$disease[[disease]]$vaccine_coverage))
  }

  # Treatment seeking
  est_provider_preference <- sample_preference(parameters$population, parameters)
  variables$provider_preference <- individual::CategoricalVariable$new(c("none", "hf", "chw", "private"), est_provider_preference)
  variables$awaiting_followup <- individual::IntegerVariable$new(rep(0,  parameters$population))
  # Previous drugs (for prophylaxis)
  variables$time_of_last_act <- individual::IntegerVariable$new(rep(NA,  parameters$population))
  variables$time_of_last_amoxicillin <- individual::IntegerVariable$new(rep(NA,  parameters$population))

  # Interventions
  variables$llin <- individual::IntegerVariable$new(stats::rbinom(parameters$population, 1, parameters$llin_coverage))

  return(variables)
}

#' Prior exposure equilibrium
#'
#' @param p Condition parameters
#' @param population Population size
#' @param initial_age Age of each child
#' @param maximum_age Maximum age
#' @param est_het Heterogeneity of each child variable
prior_exposure_equilibrium <- function(p, population, initial_age, maximum_age, est_het){
  ages <- 1:maximum_age
  vx <- rep(1, length(ages))
  if(p$vaccine_coverage > 0){
    vx <- 1 - vaccine_effect(ages, p$vaccine_start, p$vaccine_initial_efficacy, p$vaccine_hl)
  }
  mi <- maternal_immunity(ages, p$maternal_immunity_halflife)
  dp <- rep(NA, population)
  for(i in 1:population){
    dp[i] <- eq_prior_indiv(initial_age[i],
                            p$sigma,
                            est_het[i],
                            vx,
                            mi,
                            p$infection_immunity_shape,
                            p$infection_immunity_rate)
  }
  return(dp)
}

