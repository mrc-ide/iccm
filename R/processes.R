#' Create list of model processes
#'
#' @param parameters Model parameters
#' @param individuals Model individuals
#' @param variables Model variables
#' @param events Model events
#'
#' @return List of model process functions
create_processes <- function(parameters, individuals, variables, events){
  processes <- list(
    # Demographic
    graduate(parameters, individuals, variables),
    background_mortality(parameters, individuals, variables),
    render_demography(individuals, variables),

    # Disease
    condition_exposure("diarrhoea", individuals, variables, parameters, events),
    render_prevalence("diarrhoea", individuals, variables, parameters)
  )
}

