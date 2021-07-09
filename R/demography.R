#' Mortality
#'
#' Samples any death events, assigns a cause and replaces children who have died.
#'
#' @param parameters Model parameters
#' @param variables Model variables
#' @param events Model events
#' @param renderer Model renderer
mortality <- function(parameters, variables, events, renderer){
  function(timestep){
    # Death probability matrix
    mort_prob_matrix <- mortality_probability(parameters, variables)
    # Sample outcomes
    death_index <- competing_hazard(mort_prob_matrix)
    # Death from disease
    death_from_diseases(death_index, parameters, variables, events, renderer, timestep)
    # Death other causes
    death_from_other_causes(death_index, parameters, variables, events, renderer, timestep)
  }
}

#' Estimate matrix of mortality probabilities
#'
#' @inheritParams mortality
mortality_probability <- function(parameters, variables){
  mort_prob_matrix <- matrix(0, ncol = length(parameters$disease) + 1, nrow = parameters$population)
  mort_prob_matrix[ , length(parameters$disease) + 1] <- rate_to_prob(1 / parameters$average_age)
  diseases <- names(parameters$disease)
  for(i in seq_along(diseases)){
    mort_prob_matrix[variables$infection_status[[i]]$get_index_of("severe")$to_vector() , i] <- parameters$disease[[i]]$daily_probability_death
  }
  return(mort_prob_matrix)
}


#' Death from diseases
#'
#' @param death_index Index of cause of deaths
#' @param timestep Model timestep
#' @inheritParams mortality
death_from_diseases <- function(death_index, parameters, variables, events, renderer, timestep){
  diseases <- names(parameters$disease)
  for(i in seq_along(diseases)){
    index <- death_index == i
    if(sum(index) > 0){
      renderer$render(paste0(diseases[i], "_death"), sum(index), timestep)
      replace_child(individual::Bitset$new(parameters$population)$insert(which(index)), timestep, variables, parameters, events)
    }
  }
}

#' Death from other (non-modelled disease) causes
#'
#' @param death_index Index of cause of deaths
#' @param timestep Model timestep
#' @inheritParams mortality
death_from_other_causes <- function(death_index, parameters, variables, events, renderer, timestep){
  index <- death_index == (length(parameters$disease) + 1)
  if(sum(index) > 0){
    renderer$render("other_death", sum(index), timestep)
    replace_child(individual::Bitset$new(parameters$population)$insert(which(index)), timestep, variables, parameters, events)
  }
}

#' Replace child with new
#'
#' @param target Target indices
#' @param timestep Current time
#' @param variables Model variables
#' @param parameters Model parameters
#' @param events  Model events
replace_child <- function(target, timestep, variables, parameters, events) {
  # Clear any future scheduling
  clear_all_scheduled_events(target, events)

  reset_disease_status(target, parameters, variables)

  reset_heterogeneity(target, parameters, variables)

  reset_treatment_and_interventions(target, parameters, variables)

  reset_demography(target, parameters, variables, events, timestep)
}

#' Clear all scheduled events for a given target
#'
#' @param events Model events
#' @param target Children to clear events for
clear_all_scheduled_events <- function(target, events){
  for(event in unlist(events)){
    event$clear_schedule(target)
  }
}

#' Reset disease status for a new child
#'
#' @param target Target children
#' @param parameters Model parameters
#' @param variables Model variables
reset_disease_status <- function(target, parameters, variables){
  for(disease in 1:length(parameters$disease)){
    ## Prior exposure
    variables$prior_exposure[[disease]]$queue_update(0, target)
    ## Infection status
    variables$infection_status[[disease]]$queue_update("uninfected", target)
    ## Fever status
    variables$fever[[disease]]$queue_update("nonfebrile", target)
    ## Symptom onset
    variables$symptom_onset[[disease]]$queue_update(as.numeric(NA), target)
    ## Vaccination status
    variables$vaccine[[disease]]$queue_update(stats::rbinom(target$size(), 1, parameters$disease[[disease]]$vaccine_coverage), target)
  }
}

#' Reset treatment and interventions
#'
#' @param target Target children
#' @param parameters Model parameters
#' @param variables Model variables
reset_treatment_and_interventions <- function(target, parameters, variables){
  variables$time_of_last_act$queue_update(as.numeric(NA), target)
  variables$time_of_last_amoxicillin$queue_update(as.numeric(NA), target)
  variables$awaiting_followup$queue_update(0, target)
  # re-draw interventions and vaccination
  variables$llin$queue_update(stats::rbinom(target$size(), 1, parameters$llin_coverage), target)
}

#' Reset child heterogeneity
#'
#' @param target Target children
#' @param parameters Model parameters
#' @param variables Model variables
reset_heterogeneity <- function(target, parameters, variables){
  # re-draw individual level heterogeneity
  new_het <- heterogeneity(target$size(), parameters$het_sd)
  variables$heterogeneity$queue_update(new_het, target)
}

#' Reset child demographic variables
#'
#' @param target Target children
#' @param parameters Model parameters
#' @param variables Model variables
#' @param events Model events
#' @param timestep Current timestep
reset_demography <- function(target, parameters, variables, events, timestep){
  variables$birth_t$queue_update(value = timestep - parameters$age_lower, index = target)
  graduate_t <- parameters$age_upper - parameters$age_lower
  events$graduate$schedule(target, delay = rep(graduate_t, target$size()))
}

#' Get children's ages
#'
#' @param timestep Current time
#' @param birth_t Birth time variable
#' @param target Target
get_age <- function(timestep, birth_t, target = NULL){
  timestep - birth_t$get_values(index = target)
}




