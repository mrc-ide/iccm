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
#' @param overrides a named list of parameter values to use instead of defaults.
get_parameters <- function(overrides = list()){

  parameters <- list(
    # Demography
    population = 1000,
    average_age = 60 * 365,
    age_upper = 5 * 365 - 1
  )

  # Override parameters with any client specified ones
  if (!is.list(overrides)) {
    stop('overrides must be a list')
  }

  for (name in names(overrides)) {
    if (!(name %in% names(parameters))) {
      stop(paste('Unknown parameter', name, sep = ' '))
    }
    parameters[[name]] <- overrides[[name]]
  }

  return(parameters)
}
