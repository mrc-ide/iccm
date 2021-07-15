#' Create list of model processes
#'
#' @param parameters Model parameters
#' @param variables Model variables
#' @param renderer Model renderer
#' @param events Model events
#'
#' @return List of model process functions
create_processes <- function(parameters, variables, renderer, events){
  processes <- list(
    exposure(variables, parameters, events, renderer),
    mortality(parameters, variables, events, renderer),
    resample_preference(parameters, variables),

    render_prevalence(variables, renderer, parameters),
    render_demography(variables, renderer),
    render_prior(variables, renderer,parameters),
    render_fever_prevalence(parameters, variables, renderer)
  )
}

