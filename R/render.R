#' Record prevalence
#'
#' Record overall (condition) and disaggregated (disease) prevalence
#'
#' @inheritParams condition_exposure
render_prevalence <- function(variables, renderer){
  function(timestep){
    for(disease in names(parameters$disease)){
      prevalence <- (parameters$population - variables[[paste0(disease, "_status")]]$get_index_of("uninfected")$size()) / parameters$population
      renderer$render(paste0(disease, "_prevalence"), prevalence, timestep)
    }
  }
}
