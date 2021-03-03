#' Create list of model processes
#'
#' @param parameters Model parameters
#' @param individuals Model individuals
#' @param variables Model variables
#' @param events Model events
#'
#' @return List of model process functions
create_processes <- function(parameters, variables, renderer, events){
  processes <- list(
    # Demographic
    graduate(parameters, variables, renderer),
    background_mortality(parameters, variables, renderer),
    render_demography(variables, renderer),

    # Disease
    condition_exposure("dia", variables, parameters, events),

    render_prevalence("dia", variables, parameters, renderer)
  )
}

