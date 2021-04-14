#' Create individual variables
#'
#' @param parameters Model parameters
create_variables <- function(parameters){
  size <- parameters$population

  # Demography
  initial_age <- floor(rtexp(size, rate = 1 / parameters$average_age, lower = parameters$age_lower, upper = parameters$age_upper))
  birth_t <- individual::DoubleVariable$new(-initial_age)

  # Epidemiology
  est_het <- heterogeneity(size, parameters$het_sd)
  het <- individual::DoubleVariable$new(est_het)

  # Disease
  ## Diarrhoea
  ### Infection status (0 = Uninfected, 1 = Asymptomatically infected, 2 = clinically infected, 3 = severely infected)
  dia_status <- individual::IntegerVariable$new(rep(0, size))
  ### Disease type (see parameters$dia$type for index)
  dia_disease <- individual::IntegerVariable$new(rep(0, size))
  ### Start time of any active infection symptoms
  dia_symptom_start <- individual::IntegerVariable$new(rep(NA, size))
  ### Fever indicator
  dia_fever <- individual::IntegerVariable$new(rep(0, size))
  ### Prior exposures
  dia_prior <- prior_exposure_matrix(parameters$dia, size, initial_age, est_het)
  dia_prior_bacteria <- individual::IntegerVariable$new(initial_values = dia_prior[,1])
  dia_prior_virus <- individual::IntegerVariable$new(initial_values = dia_prior[,2])
  dia_prior_parasite <- individual::IntegerVariable$new(initial_values = dia_prior[,3])
  dia_prior_rotavirus <- individual::IntegerVariable$new(initial_values = dia_prior[,4])
  ### Previous treatment
  dia_last_tx <- individual::IntegerVariable$new(rep(NA, size))

  ## Malaria
  ### Infection status (0 = Uninfected, 1 = Asymptomatically infected, 2 = clinically infected, 3 = severely infected)
  malaria_status <- individual::IntegerVariable$new(rep(0, size))
  ### Disease type (see parameters$malaria$type for index)
  malaria_disease <- individual::IntegerVariable$new(rep(0, size))
  ### Start time of any active infection symptoms
  malaria_symptom_start <- individual::IntegerVariable$new(rep(NA, size))
  ### Fever indicator
  malaria_fever <- individual::IntegerVariable$new(rep(0, size))
  malaria_prior <- prior_exposure_matrix(parameters$malaria, size, initial_age, est_het)
  malaria_prior_pf <- individual::IntegerVariable$new(initial_values = malaria_prior[,1])
  ### Previous treatment
  malaria_last_tx <- individual::IntegerVariable$new(rep(NA, size))

  # Treatment seeking
  est_provider_preference <- sample_preference(size, parameters)
  provider_preference <- individual::CategoricalVariable$new(c("None", "HF", "CHW", "Private"), est_provider_preference)
  awaiting_followup <- individual::IntegerVariable$new(rep(0, size))

  # Interventions
  llin <- individual::IntegerVariable$new(stats::rbinom(size, 1, parameters$llin_coverage))
  rotavirus_vx <- individual::IntegerVariable$new(stats::rbinom(size, 1, parameters$rotavirus_vx_coverage))
  pneumococcal_vx <- individual::IntegerVariable$new(stats::rbinom(size, 1, parameters$pneumococcal_vx_coverage))
  hib_vx <- individual::IntegerVariable$new(stats::rbinom(size, 1, parameters$hib_vx_coverage))



  variables <- list(
    birth_t = birth_t,

    dia_status = dia_status,
    dia_disease = dia_disease,
    dia_symptom_start = dia_symptom_start,
    dia_fever = dia_fever,
    dia_prior_bacteria = dia_prior_bacteria,
    dia_prior_virus = dia_prior_virus,
    dia_prior_parasite = dia_prior_parasite,
    dia_prior_rotavirus = dia_prior_rotavirus,
    dia_last_tx = dia_last_tx,

    malaria_status = malaria_status,
    malaria_disease = malaria_disease,
    malaria_symptom_start = malaria_symptom_start,
    malaria_fever = malaria_fever,
    malaria_prior_pf = malaria_prior_pf,
    malaria_last_tx = malaria_last_tx,

    provider_preference = provider_preference,
    awaiting_followup = awaiting_followup,

    het = het,
    llin = llin,
    rotavirus_vx = rotavirus_vx,
    pneumococcal_vx = pneumococcal_vx,
    hib_vx = hib_vx
  )

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
                                         "dia_rotavirus_mortality", "malaria_pf_mortality")){
  for(var in zero_default){
    renderer$set_default(var, 0)
  }
}


#' Prior exposure equilibrium matrix
#'
#' @param p Condition parameters
#' @param size Population size
#' @param initial_age Age variable
#' @param est_het Het variable
prior_exposure_matrix <- function(p, size, initial_age, est_het){
  n <- p$groups
  dp <- matrix(rep(0, size * n), ncol = n)
  for(i in 1:n){
    vx <- 1 - vaccine_effect(1:(365*5), p$vx_start[i], p$vx_initial_efficacy[i], p$vx_hl[i])
    mi <- maternal_immunity(1:(365*5), p$mi_hl[i])
    sigma <- p$sigma[i]
    ii_shape <- p$ii_shape[i]
    ii_rate <- p$ii_rate[i]
    for(j in 1:size){
      dp[j, i] <- round(eq_prior_indiv(initial_age[j],
                                       sigma,
                                       est_het[j],
                                       vx,
                                       mi,
                                       ii_shape,
                                       ii_rate))
    }
  }
  return(dp)
}
