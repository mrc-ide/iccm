#' Create events
#'
#' @param variables Model variables
#' @param parameters  Model parameters
#'
#' @return List of events
create_events <- function(variables, parameters){
  events <- list()

  # Progression events for each disease
  events$asymptomatic <- list()
  events$clinical <- list()
  events$susceptible <- list()
  for(disease in 1:length(parameters$disease)){
    events$asymptomatic[[disease]] <- individual::TargetedEvent$new(parameters$population)
    events$clinical[[disease]] <- individual::TargetedEvent$new(parameters$population)
    events$susceptible[[disease]] <- individual::TargetedEvent$new(parameters$population)
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

  for(disease in 1:length(parameters$disease)){
    p <- parameters$disease[[disease]]

    ### Progress to clinical infection #########################################
    # Update status
    events$clinical[[disease]]$add_listener(
      update_status(
        variable = variables$infection_status[[disease]],
        new_status = "symptomatic"
      )
    )
    # Update fever status
    events$clinical[[disease]]$add_listener(
      update_fever(
        variable = variables$fever[[disease]],
        probability_fever = p$probability_fever
      )
    )
    # Update time of onset of symptoms
    events$clinical[[disease]]$add_listener(
      onset_symptoms(
        variable = variables$symptom_onset[[disease]]
      )
    )
    # Schedule treatment seeking
    events$clinical[[disease]]$add_listener(
      schedule_treatment_seeking(
        probability_seek_treatment = parameters$treatment_seeking$prob_seek_treatment,
        behavioural_delay = parameters$treatment_seeking$treat_seeking_behaviour_delay,
        provider_preference = variables$provider_preference,
        hf_treatment = events$hf_treatment,
        hf_travel_time = parameters$hf$travel_time,
        chw_treatment = events$chw_treatment,
        chw_travel_time = parameters$chw$travel_time,
        private_treatment = events$private_treatment,
        private_travel_time = parameters$private$travel_time
      )
    )
    # In the case of super-infection, a recovery may already be scheduled. We need to clear this
    events$clinical[[disease]]$add_listener(
      clear_scheduled_recovery(
        event = events$susceptible[[disease]]
      ))
    # Schedule progression from clinical infection
    events$clinical[[disease]]$add_listener(
      schedule_progression_from_clinical_infection(
        p = p,
        asymptomatic = events$asymptomatic[[disease]],
        susceptible = events$susceptible[[disease]]
      ))
    ############################################################################

    ### Progress to asymptomatic infection #####################################
    # Update status
    events$asymptomatic[[disease]]$add_listener(
      update_status(
        variable = variables$infection_status[[disease]],
        new_status = "asymptomatic"
      )
    )
    # Clear any disease-related fever
    events$asymptomatic[[disease]]$add_listener(
      clear_fever(
        variable = variables$fever[[disease]]
      )
    )
    # Schedule progression from asymptomatic infection
    events$asymptomatic[[disease]]$add_listener(
      schedule_progression_from_asymptomatic_infection(
        p = p,
        susceptible = events$susceptible[[disease]]
      )
    )
    ############################################################################

    ### Progress to uninfected #################################################
    # Update status
    events$susceptible[[disease]]$add_listener(
      update_status(
        variable = variables$infection_status[[disease]],
        new_status = "uninfected"
      )
    )
    # Clear any disease-related fever
    events$susceptible[[disease]]$add_listener(
      clear_fever(
        variable = variables$fever[[disease]]
      )
    )
    # Clear any symptom-onset time
    events$susceptible[[disease]]$add_listener(
      clear_onset_symptoms(
        variable = variables$symptom_onset[[disease]]
      )
    )
    ############################################################################
  }

  events$graduate$add_listener(
    graduate_event(
      variables,
      parameters,
      events,
      renderer
    )
  )
  events$hf_treatment$add_listener(
    hf_treat(
      variables,
      parameters,
      renderer,
      events
    )
  )
  events$chw_treatment$add_listener(
    chw_treat(
      variables,
      parameters,
      renderer,
      events
    )
  )
  events$private_treatment$add_listener(
    private_treat(
      variables,
      parameters,
      renderer,
      events
    )
  )
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


clear_scheduled_recovery <- function(event){
  force(event)
  function(timestep, target){
    event$clear_schedule(target)
  }
}

schedule_treatment_seeking <- function(probability_seek_treatment,
                                       behavioural_delay,
                                       provider_preference,
                                       hf_treatment,
                                       hf_travel_time,
                                       chw_treatment,
                                       chw_travel_time,
                                       private_treatment,
                                       private_travel_time){
  force(probability_seek_treatment)
  force(behavioural_delay)
  force(provider_preference)
  force(hf_treatment)
  force(hf_travel_time)
  force(chw_treatment)
  force(chw_travel_time)
  force(private_treatment)
  force(private_travel_time)
  function(timestep, target){
    to_treat <- target$copy()$sample(probability_seek_treatment)
    to_treat_hf <- provider_preference$get_index_of("hf")$and(to_treat)
    to_treat_chw <- provider_preference$get_index_of("chw")$and(to_treat)
    to_treat_private <- provider_preference$get_index_of("private")$and(to_treat)

    hf_treatment$schedule(to_treat_hf, delay = hf_travel_time + stats::rpois(to_treat_hf$size(), behavioural_delay) + 1)
    chw_treatment$schedule(to_treat_chw, delay = chw_travel_time + stats::rpois(to_treat_chw$size(), behavioural_delay) + 1)
    private_treatment$schedule(to_treat_private, delay = private_travel_time + stats::rpois(to_treat_private$size(), behavioural_delay) + 1)
  }
}

schedule_progression_from_clinical_infection <- function(p, asymptomatic, susceptible){
  force(p)
  force(asymptomatic)
  force(susceptible)

  function(timestep, target){
    clinical_duration <- stats::rexp(target$size(), 1 / p$clinical_duration)
    if(p$asymptomatic_pathway){
      asymptomatic$schedule(target, delay = clinical_duration)
    } else {
      susceptible$schedule(target, delay = clinical_duration)
    }
  }
}

schedule_progression_from_asymptomatic_infection <- function(p, susceptible){
  force(p)
  force(susceptible)

  function(timestep, target){
    asymptomatic_duration <- stats::rexp(target$size(), 1 / p$asymptomatic_duration)
    susceptible$schedule(target, delay = asymptomatic_duration)
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
  graduate_t <- parameters$age_upper - get_age(0, variables$birth_t)
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

