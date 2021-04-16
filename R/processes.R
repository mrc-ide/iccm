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
    check_disease("A", variables),
    ## Diarrhoea
    condition_exposure("dia", variables, parameters, events, renderer),
    progress_severe("dia", parameters, variables, events),
    die("dia", parameters, variables, events, renderer),
    render_prevalence("dia", variables, parameters, renderer),
    render_prior_exposure("dia", variables, parameters, renderer),
    ## Pneumonia
    condition_exposure("pneumonia", variables, parameters, events, renderer),
    progress_severe("pneumonia", parameters, variables, events),
    die("pneumonia", parameters, variables, events, renderer),
    render_prevalence("pneumonia", variables, parameters, renderer),
    render_prior_exposure("pneumonia", variables, parameters, renderer),
    ## Malaria
    condition_exposure("malaria", variables, parameters, events, renderer),
    progress_severe("malaria", parameters, variables, events),
    die("malaria", parameters, variables, events, renderer),
    render_prevalence("malaria", variables, parameters, renderer),
    render_prior_exposure("malaria", variables, parameters, renderer),

    render_fevers(variables, parameters, renderer),
    # Demographic
    #graduate(parameters, variables, renderer, events),
    background_mortality(parameters, variables, renderer, events),
    render_demography(variables, renderer),
    check_disease("B", variables)
  )
}

