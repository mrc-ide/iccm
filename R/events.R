
#' Create events
#'
#' @param variables Model variables
#' @param parameters  Model parameters
#'
#' @return List of events
create_events <- function(variables, parameters){
  # Symptomatic to susceptible
  dia_recover <- individual::TargetedEvent$new(parameters$population)

  # Demographic
  graduate <- individual::TargetedEvent$new(parameters$population)

  events <- list(
    dia_recover = dia_recover,
    graduate = graduate
  )
  return(events)
}

#' Add event listeners
#'
#' These are the functions that are called when an event is triggered
#'
#' @param events Model events
#' @param variables Model variables
#' @param parameters Model parameters
#' @param renderer Model renderer
create_event_listeners <- function(events, variables, parameters, renderer){
  events$dia_recover$add_listener(recover_event(variables, "dia"))
  events$graduate$add_listener(graduate_event(variables, parameters, events, renderer))
}

#' Initialise events
#'
#' Run any scheduling that is required when initialising the population
#'
#' @inheritParams create_event_listeners
initialise_events <- function(events, variables, parameters){
  # Initialise gradutation
  to_graduate <- individual::Bitset$new(parameters$population)
  to_graduate <- to_graduate$insert(1:parameters$population)
  graduate_t <- parameters$age_upper - get_age(0, variables)
  events$graduate$schedule(to_graduate, delay = graduate_t)
}

#' Recovery event
#'
#' A recovery event. Resets status back to 0 (susceptible) and clears the disease index.
#'
#' @param condition Condition recovered from
#' @inheritParams create_event_listeners
#'
#' @return Event
recover_event <- function(variables, condition){
  condition_status <- paste0(condition, "_status")
  condition_disease <- paste0(condition, "_disease")

  function(timestep, target){
    # Set status = susceptible
    variables[[condition_status]]$queue_update("S", target)
    variables[[condition_disease]]$queue_update(0, target)
  }
}

#' Graduate event
#'
#' Replaces a child that has reached age five with a newborn
#'
#' @inheritParams create_event_listeners
#'
#' @return Event
graduate_event <- function(variables, parameters, events, renderer){
  function(timestep, target){
    renderer$render("graduation", target$size(), timestep)
    replace_child(target, timestep, variables, parameters, events)
  }
}
