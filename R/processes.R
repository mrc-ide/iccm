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
    # Disease
    condition_exposure("dia", variables, parameters, events, renderer),
    progress_severe("dia", parameters, variables),
    die("dia", parameters, variables, events, renderer),

    render_prevalence("dia", variables, parameters, renderer),
    render_prior_exposure("dia", variables, parameters, renderer),
    # Demographic
    #graduate(parameters, variables, renderer, events),
    background_mortality(parameters, variables, renderer, events),
    render_demography(variables, renderer)
  )
}

