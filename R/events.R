#' Create events
#'
#' @param variables Model variables
#' @param parameters  Model parameters
#'
#' @return List of events
create_events <- function(variables, parameters){
  events <- list()

  # Progression events for each disease
  for(disease in names(parameters$disease)){
    events[[paste0(disease, "_progress_to_asymptomatic_infection")]] <- individual::TargetedEvent$new(parameters$population)
    events[[paste0(disease, "_progress_to_clinical_infection")]] <- individual::TargetedEvent$new(parameters$population)
    events[[paste0(disease, "_progress_to_severe_infection")]] <- individual::TargetedEvent$new(parameters$population)
    events[[paste0(disease, "_progress_to_uninfected")]] <- individual::TargetedEvent$new(parameters$population)
  }
  # Treatment provider visits
  events$hf_treatment <- individual::TargetedEvent$new(parameters$population)
  events$chw_treatment <- individual::TargetedEvent$new(parameters$population)
  events$private_treatment <- individual::TargetedEvent$new(parameters$population)
  # Graduation
  events$graduate <- individual::TargetedEvent$new(parameters$population)
  return(events)
}


#' Adds listeners to specific events
#'
#' @param events Model events
#' @param variables Model variables
#' @param parameters Model parameters
#' @param renderer Model renderer
create_event_listeners <- function(events, variables, parameters, renderer){
  # Add listeners
  for(disease in names(parameters$disease)){
    ### Progress to clinical infection #########################################
    event <- paste0(disease, "_progress_to_clinical_infection")
    events[[event]]$add_listener(
      update_status(
        variable = variables[[paste0(disease, "_status")]],
        new_status = "symptomatic"
      )
    )
    events[[event]]$add_listener(
      update_fever(
        variable = variables[[paste0(disease, "_fever")]],
        probability_fever = parameters$disease[[disease]]$probability_fever
      )
    )
    events[[event]]$add_listener(
      onset_symptoms(
        variable = variables[[paste0(disease, "_symptom_onset")]]
      )
    )
    events[[event]]$add_listener(
      schedule_treatment_seeking(
        probability_seek_treatment = parameters$treatment_seeking$prob_seek_treatment,
        behavioural_delay = parameters$treatment_seeking$treat_seeking_behaviour_delay,
        provider_preference = variables$provider_preference,
        parameters = parameters,
        events = events
      )
    )
    # This is needed in the case of superinfection
    events[[event]]$add_listener(
      clear_scheduled_recovery(
        event = events[[paste0(disease, "_progress_to_uninfected")]]
      ))
    events[[event]]$add_listener(
      schedule_progression_from_clinical_infection(
        disease,
        parameters,
        progress_to_severe_infection = events[[paste0(disease, "_progress_to_severe_infection")]],
        progress_to_asymptomatic_infection = events[[paste0(disease, "_progress_to_asymptomatic_infection")]],
        progress_to_uninfected = events[[paste0(disease, "_progress_to_uninfected")]]
      ))
    ############################################################################

    ### Progress to asymptomatic infection #####################################
    event <- paste0(disease, "_progress_to_asymptomatic_infection")
    events[[event]]$add_listener(
      update_status(
        variable = variables[[paste0(disease, "_status")]],
        new_status = "asymptomatic"
      )
    )
    events[[event]]$add_listener(
      clear_fever(
        variable = variables[[paste0(disease, "_fever")]]
      )
    )
    events[[event]]$add_listener(
      schedule_progression_from_asymptomatic_infection(
        disease,
        parameters,
        progress_to_uninfected = events[[paste0(disease, "_progress_to_uninfected")]]
      ))
    ############################################################################

    ### Progress to uninfected #################################################
    event <- paste0(disease, "_progress_to_uninfected")
    events[[event]]$add_listener(
      update_status(
        variable = variables[[paste0(disease, "_status")]],
        new_status = "uninfected"
      )
    )
    events[[event]]$add_listener(
      clear_fever(
        variable = variables[[paste0(disease, "_fever")]]
      )
    )
    events[[event]]$add_listener(
      clear_onset_symptoms(
        variable = variables[[paste0(disease, "_symptom_onset")]]
      )
    )
    ############################################################################

    ### Progress to severe disease #############################################
    event <- paste0(disease, "_progress_to_severe_infection")
    events[[event]]$add_listener(
      update_status(
        variable = variables[[paste0(disease, "_status")]],
        new_status = "severe"
      )
    )
    events[[event]]$add_listener(
      update_fever(
        variable = variables[[paste0(disease, "_fever")]],
        probability_fever = 1
      )
    )
    events[[event]]$add_listener(
      schedule_treatment_seeking(
        probability_seek_treatment = parameters$treatment_seeking$prob_seek_treatment_severe,
        behavioural_delay = 0,
        provider_preference = variables$provider_preference,
        parameters = parameters,
        events = events
      )
    )
    events[[event]]$add_listener(
      schedule_progression_from_severe_infection(
        disease,
        parameters,
        progress_to_asymptomatic_infection = events[[paste0(disease, "_progress_to_asymptomatic_infection")]],
        progress_to_uninfected = events[[paste0(disease, "_progress_to_uninfected")]]

      ))
    events[[event]]$add_listener(
      record_severe_incidence(paste0(disease, "_severe_incidence"), renderer)
    )
    ############################################################################
  }
  events$graduate$add_listener(graduate_event(variables, parameters, events, renderer))
  events$hf_treatment$add_listener(hf_treat(variables, parameters, renderer, events))
  events$chw_treatment$add_listener(chw_treat(variables, parameters, renderer, events))
  events$private_treatment$add_listener(private_treat(variables, parameters, renderer, events))
}

update_status <- function(variable, new_status){
  force(variable)
  force(new_status)
  function(timestep, target){
    variable$queue_update(new_status, target)
  }
}

update_fever <- function(variable, probability_fever){
  force(variable)
  force(probability_fever)
  function(timestep, target){
    get_fever <- target$copy()$sample(probability_fever)
    variable$queue_update("febrile", get_fever)
  }
}

clear_fever <- function(variable){
  force(variable)
  function(timestep, target){
    variable$queue_update("nonfebrile", target)
  }
}

clear_onset_symptoms <- function(variable){
  force(variable)
  function(timestep, target){
    variable$queue_update(as.numeric(NA), target)
  }
}

onset_symptoms <- function(variable){
  force(variable)
  function(timestep, target){
    variable$queue_update(timestep, target)
  }
}

record_severe_incidence <- function(name, renderer){
  force(name)
  force(renderer)
  function(timestep, target){
    renderer$render(name, target$size(), timestep)
  }
}

clear_scheduled_recovery <- function(event){
  force(event)
  function(timestep, target){
    event$clear_schedule(target)
  }
}

schedule_treatment_seeking <- function(probability_seek_treatment, behavioural_delay, provider_preference, parameters, events){
  force(probability_seek_treatment)
  force(behavioural_delay)
  force(provider_preference)
  force(parameters)
  force(events)
  function(timestep, target){
    to_treat <- target$copy()$sample(probability_seek_treatment)
    to_treat_hf <- provider_preference$get_index_of("hf")$and(to_treat)
    to_treat_chw <- provider_preference$get_index_of("chw")$and(to_treat)
    to_treat_private <- provider_preference$get_index_of("private")$and(to_treat)

    events$hf_treatment$schedule(to_treat_hf, delay = parameters$hf$travel_time + stats::rpois(to_treat_hf$size(), behavioural_delay) + 1)
    events$chw_treatment$schedule(to_treat_chw, delay = parameters$chw$travel_time + stats::rpois(to_treat_chw$size(), behavioural_delay) + 1)
    events$private_treatment$schedule(to_treat_private, delay = parameters$private$travel_time + stats::rpois(to_treat_private$size(), behavioural_delay) + 1)
  }
}

schedule_progression_from_clinical_infection <- function(disease, parameters, progress_to_severe_infection, progress_to_asymptomatic_infection, progress_to_uninfected){
  force(disease)
  force(parameters)
  force(progress_to_severe_infection)
  force(progress_to_asymptomatic_infection)
  force(progress_to_uninfected)
  function(timestep, target){
    clinical_duration <- stats::rexp(target$size(), 1 / parameters$disease[[disease]]$clinical_duration)
    time_to_severe <- stats::rgeom(target$size(), parameters$disease[[disease]]$daily_probability_severe)
    to_severe_index <- time_to_severe < clinical_duration
    to_severe <- individual::filter_bitset(target, which(to_severe_index))
    progress_to_severe_infection$schedule(to_severe, time_to_severe[to_severe_index])

    recovering <- individual::filter_bitset(target, which(!to_severe_index))

    if(parameters$disease[[disease]]$asymptomatic_pathway){
      progress_to_asymptomatic_infection$schedule(recovering, delay = clinical_duration[!to_severe_index])
    } else {
      progress_to_uninfected$schedule(recovering, delay = clinical_duration[!to_severe_index])
    }
  }
}

schedule_progression_from_asymptomatic_infection <- function(disease, parameters, progress_to_uninfected){
  force(disease)
  force(parameters)
  force(progress_to_uninfected)
  function(timestep, target){
    asymptomatic_duration <- stats::rexp(target$size(), 1 / parameters$disease[[disease]]$asymptomatic_duration)
    progress_to_uninfected$schedule(target, delay = asymptomatic_duration)
  }
}

schedule_progression_from_severe_infection <- function(disease, parameters, progress_to_asymptomatic_infection, progress_to_uninfected){
  force(disease)
  force(parameters)
  force(progress_to_asymptomatic_infection)
  force(progress_to_uninfected)
  function(timestep, target){
    severe_duration <- stats::rexp(target$size(), 1 / parameters$disease[[disease]]$severe_duration)
    if(parameters$disease[[disease]]$asymptomatic_pathway){
      progress_to_asymptomatic_infection$schedule(target, delay = severe_duration)
    } else {
      progress_to_uninfected$schedule(target, delay = severe_duration)
    }
  }
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

