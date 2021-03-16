
#' Create events
#'
#' @param variables Model variables
#' @param parameters  Model parameters
#'
#' @return List of events
create_events <- function(variables, parameters){
  # Symptomatic to susceptible
  dia_recover <- individual::TargetedEvent$new(parameters$population)
  dia_progress_severe <- individual::TargetedEvent$new(parameters$population)
  dia_death <- individual::TargetedEvent$new(parameters$population)

  events <- list(
    dia_recover = dia_recover,
    dia_progress_severe = dia_progress_severe,
    dia_death = dia_death
  )
  return(events)
}

create_event_listeners <- function(events, variables, parameters, renderer){
  events$dia_recover$add_listener(recover_event(variables, "dia"))
  events$dia_progress_severe$add_listener(progress_severe_event(variables, "dia"))
  events$dia_death$add_listener(death_event(variables, parameters, events, renderer, "dia"))
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
    # Set status = susceptible
    variables[[status_name]]$queue_update("S", target)
    variables[[type_name]]$queue_update("None", target)
  }
}

progress_severe_event <- function(variables, condition){
  status_name <- paste0(condition, "_status")

  function(timestep, target){
    # Set status = susceptible
    variables[[status_name]]$queue_update("V", target)
  }
}

death_event <- function(variables, parameters, events, renderer, condition){
  type_name <- paste0(condition, "_type")
  types <- parameters[[condition]]$type
  mortality_render_names <- paste0(condition, "_", types, "_mortality")

  function(timestep, target){
    replace_child(target, timestep, variables, parameters, events)

    # Rendering deaths
    for(i in seq_along(types)){
      n <- variables[[type_name]]$get_index_of(types[i])$and(target)
      renderer$render(mortality_render_names[i], n$size(), timestep)
    }
  }
}

