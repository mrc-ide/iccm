#' Create individual variables
#'
#' @param parameters Model parameters
create_variables <- function(parameters){
  size <- parameters$population

  # Demography variables
  initial_age <- floor(rtexp(size, rate = 1 / parameters$average_age, lower = parameters$age_lower, upper = parameters$age_upper))
  birth_t <- individual::DoubleVariable$new(-initial_age)

  # Disease vaiables
  states <- c("S", "A", "I", "V")
  dia_types <- c("None", parameters$dia$type)
    # Infection status
  dia_status <- individual::CategoricalVariable$new(categories = states, initial_values = rep("S", size))
    # Disease type
  dia_type <- individual::CategoricalVariable$new(categories = dia_types, initial_values = rep("None", size))
    # TODO:: Prior exposure counters - update to be DoubleMatrixVariable
  dia_prior_bacteria <- individual::IntegerVariable$new(initial_values = rep(0, size))
  dia_prior_virus <- individual::IntegerVariable$new(initial_values = rep(0, size))
  dia_prior_parasite <- individual::IntegerVariable$new(initial_values = rep(0, size))
  dia_prior_rotavirus <- individual::IntegerVariable$new(initial_values = rep(0, size))

  # Epidemiology
  het <- individual::DoubleVariable$new(heterogeneity(size, parameters$het_sd))

  # Interventions
  llin <- individual::IntegerVariable$new(stats::rbinom(size, 1, parameters$llin_coverage))
  rotavirus_vx <- individual::IntegerVariable$new(stats::rbinom(size, 1, parameters$rotavirus_vx_coverage))
  pneumococcal_vx <- individual::IntegerVariable$new(stats::rbinom(size, 1, parameters$pneumococcal_vx_coverage))
  hib_vx <- individual::IntegerVariable$new(stats::rbinom(size, 1, parameters$hib_vx_coverage))

  variables <- list(
    birth_t = birth_t,
    dia_status = dia_status,
    dia_type = dia_type,
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

