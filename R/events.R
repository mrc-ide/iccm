
#' Create events
#'
#' @param individuals  Model individuals
#' @param variables Model variables
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
#' @param individuals Model individuals
#' @param variables Model variables
#'
#' @return Event
recover_event <- function(variables, disease){
  status_name <- paste0(disease, "_status")
  type_name <- paste0(disease, "_type")
  function(target){
    # Set status = 0 = susceptible
    variables[[status_name]]$queue_update(value = "S", index = target)
    variables[[type_name]]$queue_update(values = "none", index = target)
  }
}
