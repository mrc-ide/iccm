
#' Create events
#'
#' @param individuals  Model individuals
#' @param variables Model variables
#'
#' @return List of events
create_events <- function(individuals, variables){
  events <- list()

  # Symptomatic to susceptible
  events$diarrhoea_recover <- recover_event("diarrhoea", individuals, variables)

  return(events)
}


#' Recovery event
#'
#' A recovery event. Resets status back to 0 (susceptible) and clears the disease index.
#'
#' @param condition Condition recovered from
#' @param individuals Model individuals
#' @param variables Model variables
#'
#' @return Event
recover_event <- function(condition, individuals, variables){
  event <- individual::Event$new(paste0(condition, "_recover"))
  # Add listener
  event$add_listener(
    function(api, target) {
      # Set status = 0 = susceptible
      api$queue_variable_update(individuals$child, variables[[paste0(condition, "_status")]], 0, target)
       # Set disease index = 0 = no disease
      api$queue_variable_update(individuals$child, variables[[paste0(condition, "_disease_index")]], 0, target)
    })

  return(event)
}
