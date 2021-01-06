#' Create list of model processes
#'
#' @param parameters Model parameters
#' @param individuals Model individuals
#' @param variables Model variables
#'
#' @return List of model process functions
create_processes <- function(parameters, individuals, variables){
  processes <- list(
    graduate(parameters, individuals, variables),
    background_mortality(parameters, individuals, variables),
    render_demography(individuals, variables)
  )
}

