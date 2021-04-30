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
    exposure(variables, parameters, events),
    render_prevalence(variables, renderer),
    individual::categorical_count_renderer_process(renderer, variables$plasmodium_falciparum_status, "uninfected")
  )
}

