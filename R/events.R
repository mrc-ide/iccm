
#' Create events
#'
#' @param variables Model variables
#' @param parameters  Model parameters
#'
#' @return List of events
create_events <- function(variables, parameters){
  # To susceptible
  dia_recover <- individual::TargetedEvent$new(parameters$population)
  malaria_recover <- individual::TargetedEvent$new(parameters$population)

  # To asymptomatic
  dia_asymptomatic <- individual::TargetedEvent$new(parameters$population)
  malaria_asymptomatic <- individual::TargetedEvent$new(parameters$population)

  # Treatment
  hf_treatment <- individual::TargetedEvent$new(parameters$population)
  chw_treatment <- individual::TargetedEvent$new(parameters$population)
  private_treatment <- individual::TargetedEvent$new(parameters$population)

  # Demographic
  graduate <- individual::TargetedEvent$new(parameters$population)

  # Note ordering of events does matter here
  events <- list(
    dia_asymptomatic = dia_asymptomatic,
    dia_recover = dia_recover,
    malaria_asymptomatic = malaria_asymptomatic,
    malaria_recover = malaria_recover,
    hf_treatment = hf_treatment,
    chw_treatment = chw_treatment,
    private_treatment = private_treatment,
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
  events$dia_asymptomatic$add_listener(asymptomatic_event(variables, "dia"))
  events$dia_recover$add_listener(recover_event(variables, "dia"))
  events$malaria_asymptomatic$add_listener(asymptomatic_event(variables, "malaria"))
  events$malaria_recover$add_listener(recover_event(variables, "malaria"))
  events$hf_treatment$add_listener(hf_treat(variables, parameters, renderer, events))
  events$chw_treatment$add_listener(chw_treat(variables, parameters, renderer, events))
  events$private_treatment$add_listener(private_treat(variables, parameters, renderer, events))
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
  condition_fever <- paste0(condition, "_fever")

  function(timestep, target){
    #if(condition == "dia"){
    #  print("Recover")
    #  print(target$size())
    #}
    # Set status = susceptible
    variables[[condition_status]]$queue_update(0, target)
    variables[[condition_disease]]$queue_update(0, target)
    variables[[condition_fever]]$queue_update(0, target)
  }
}

#' Asymptomatic event
#'
#' A move to asymptomatic. Resets status back to 1 (asymptomatic) and clears any fever.
#'
#' @param condition Condition recovered from
#' @inheritParams create_event_listeners
#'
#' @return Event
asymptomatic_event <- function(variables, condition){
  condition_status <- paste0(condition, "_status")
  condition_fever <- paste0(condition, "_fever")

  function(timestep, target){
    #if(condition == "dia"){
    #  print("Asymp")
    #  print(target$size())
    #}
    # Set status = asymptomatic
    variables[[condition_status]]$queue_update(1, target)
    variables[[condition_fever]]$queue_update(0, target)
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
