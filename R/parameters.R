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
    age_upper = 5 * 365 - 1
  )

  parameters$diarrhoea <- list(
    groups = 2,
    type = c("bacteria", "virus"),
    index = c(1, 2),
    sigma = c(0.01, 0.02),
    # Maternal immunity half life
    mi =  c(100, 200),
    # Average duration of clinical episode
    clin_dur = c(14, 14)
  )
  return(parameters)
}
