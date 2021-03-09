
#' Create events
#'
#' @param variables Model variables
#' @param parameters  Model parameters
#'
#' @return List of events
create_events <- function(variables, parameters){
  # Symptomatic to susceptible
  dia_recover <- individual::TargetedEvent$new(parameters$population)

  events <- list(
    dia_recover = dia_recover
  )
  return(events)
}

create_event_listeners <- function(events, variables){
  events$dia_recover$add_listener(recover_event(variables, "dia"))
}

#' Recovery event
#'
#' A recovery event. Resets status back to 0 (susceptible) and clears the disease index.
#'
#' @param condition Condition recovered from
#' @param variables Model variables
#'
#' @return Event
recover_event <- function(variables, condition){
  status_name <- paste0(condition, "_status")
  type_name <- paste0(condition, "_type")

  function(timestep, target){
    # Set status = 0 = susceptible
    variables[[status_name]]$queue_update("S", target)
    variables[[type_name]]$queue_update("None", target)
  }
}
