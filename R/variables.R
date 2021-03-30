#' Create individual variables
#'
#' @param parameters Model parameters
create_variables <- function(parameters){
  size <- parameters$population

  # Demography
  initial_age <- floor(rtexp(size, rate = 1 / parameters$average_age, lower = parameters$age_lower, upper = parameters$age_upper))
  birth_t <- individual::DoubleVariable$new(-initial_age)

  # Disease
  ## Infection status
  dia_status <- individual::IntegerVariable$new(rep(0, size))
  ## Disease type
  dia_disease <- individual::IntegerVariable$new(rep(0, size))
  ## Start time of any active infection symptoms
  dia_symptom_start <- individual::IntegerVariable$new(rep(NA, size))
  ## Fever indicator
  dia_fever <- individual::IntegerVariable$new(rep(0, size))

  # Treatment seeking
  est_provider_preference <- sample_preference(size, parameters)
  provider_preference <- individual::CategoricalVariable$new(c("None", "HF", "CHW", "Private"), est_provider_preference)
  awaiting_followup <- individual::IntegerVariable$new(rep(0, size))

  # Epidemiology
  est_het <- heterogeneity(size, parameters$het_sd)
  het <- individual::DoubleVariable$new(est_het)

  # Interventions
  llin <- individual::IntegerVariable$new(stats::rbinom(size, 1, parameters$llin_coverage))
  rotavirus_vx <- individual::IntegerVariable$new(stats::rbinom(size, 1, parameters$rotavirus_vx_coverage))
  pneumococcal_vx <- individual::IntegerVariable$new(stats::rbinom(size, 1, parameters$pneumococcal_vx_coverage))
  hib_vx <- individual::IntegerVariable$new(stats::rbinom(size, 1, parameters$hib_vx_coverage))

  # Prior exposures
  # TODO:: Prior exposure counters - update to be DoubleMatrixVariable
  dp <- matrix(rep(0, size * 4), ncol = 4)
  for(i in 1:4){
    vx <- 1 - vaccine_effect(1:(365*5), parameters$dia$vx_start[i], parameters$dia$vx_initial_efficacy[i], parameters$dia$vx_hl[i])
    mi <- maternal_immunity(1:(365*5), parameters$dia$mi_hl[i])
    sigma <- parameters$dia$sigma[i]
    ii_shape <- parameters$dia$ii_shape[i]
    ii_rate <- parameters$dia$ii_rate[i]
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
  dia_prior_bacteria <- individual::IntegerVariable$new(initial_values = dp[,1])
  dia_prior_virus <- individual::IntegerVariable$new(initial_values = dp[,2])
  dia_prior_parasite <- individual::IntegerVariable$new(initial_values = dp[,3])
  dia_prior_rotavirus <- individual::IntegerVariable$new(initial_values = dp[,4])

  variables <- list(
    birth_t = birth_t,
    dia_status = dia_status,
    dia_disease = dia_disease,
    dia_symptom_start = dia_symptom_start,
    dia_fever = dia_fever,
    provider_preference = provider_preference,
    awaiting_followup = awaiting_followup,
    dia_prior_bacteria = dia_prior_bacteria,
    dia_prior_virus = dia_prior_virus,
    dia_prior_parasite = dia_prior_parasite,
    dia_prior_rotavirus = dia_prior_rotavirus,
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
                                         "dia_rotavirus_mortality")){
  for(var in zero_default){
    renderer$set_default(var, 0)
  }
}
