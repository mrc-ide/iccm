#' @title Get model parameters
#' @description
#' Create a named list of parameters for use in the model.
#'
#' The parameters are defined below.
#'
#' \strong{Demography:}
#'
#' * pop - Population size
#' * average_age - The average lifespan of an individual within the population
#' * age_upper - The upper limit to modelled ages
#'
get_parameters <- function(){

  parameters <- list(
    # Demography
    population = 1000,
    average_age = 60 * 365,
    age_lower = 30,
    age_upper = 5 * 365 - 1,
    # Epidemiology
    het_sd = 1.6,
    # Interventions
    llin_coverage = 0.5,
    rotavirus_vx_coverage = 1,
    pneumococcal_vx_coverage = 0.1,
    hib_vx_coverage = 0.1
  )

  parameters$diarrhoea <- list(
    groups = 4,
    type = c("bacteria", "virus", "parasite", "rotavirus"),
    index = c(1, 2, 3, 4),
    sigma = c(0.01, 0.02, 0.03, 0.04),
    # Average duration of clinical episode
    clin_dur = c(14, 14, 14, 14),
    # Maternal immunity
    ## Half life
    mi_hl =  c(100, 200, 100, 100),
    # Infection immunity
    ## shape
    ii_shape = c(7, 5, 6, 4),
    ## rate
    ii_rate = c(10, 0.5, 3, 6),
    # Vaccination
    vx_start = c(0, 0, 0, 0),
    vx_initial_efficacy = c(0, 0, 0, 1),
    vx_hl = c(0, 0, 0, 365 * 100),
    # TODO this should be a function of coverage:
    vx_ci = c(0, 0, 0, 0)
  )
  return(parameters)
}
