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
    mort_prob_matrix <- matrix(0, ncol = length(parameters$disease) + 1, nrow = parameters$population)
    mort_prob_matrix[ , length(parameters$disease) + 1] <- rate_to_prob(1 / parameters$average_age)
    diseases <- names(parameters$disease)
    for(i in seq_along(diseases)){
      mort_prob_matrix[variables[[paste0(diseases[i], "_status")]]$get_index_of("severe")$to_vector() , i] <- parameters$disease[[diseases[i]]]$daily_probability_death
    }
    # Sample outcomes
    death_index <- competing_hazard(mort_prob_matrix)
    # Death from disease
    for(i in seq_along(diseases)){
      index <- death_index == i
      if(sum(index) > 0){
        renderer$render(paste0(diseases[i], "_death"), sum(index), timestep)
        replace_child(individual::Bitset$new(parameters$population)$insert(which(index)), timestep, variables, parameters, events)
      }
    }
    # Death other causes
    index <- death_index == i + 1
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
  for(event in events){
    event$clear_schedule(target)
  }

  variables$birth_t$queue_update(value = timestep - parameters$age_lower, index = target)
  graduate_t <- parameters$age_upper - parameters$age_lower
  events$graduate$schedule(target, delay = rep(graduate_t, target$size()))

  # Diseases
  for(disease in names(parameters$disease)){
    variables[[paste0(disease, "_prior_exposure")]]$queue_update(0, target)
    variables[[paste0(disease, "_status")]]$queue_update("uninfected", target)
    variables[[paste0(disease, "_fever")]]$queue_update("nonfebrile", target)
    variables[[paste0(disease, "_symptom_onset")]]$queue_update(as.numeric(NA), target)
  }

  n <- target$size()
  # re-draw individual level heterogeneity
  new_het <- heterogeneity(n, parameters$het_sd)
  variables$het$queue_update(new_het, target)

  variables$time_of_last_act$queue_update(as.numeric(NA), target)
  variables$time_of_last_amoxicillin$queue_update(as.numeric(NA), target)
  variables$awaiting_followup$queue_update(0, target)

  # re-draw interventions and vaccination
  variables$llin$queue_update(stats::rbinom(n, 1, parameters$llin_coverage), target)
  variables$rotavirus_vx$queue_update(stats::rbinom(n, 1, parameters$rotavirus_vx_coverage), target)
  variables$pneumococcal_vx$queue_update(stats::rbinom(n, 1, parameters$pneumococcal_vx_coverage), target)
  variables$hib_vx$queue_update(stats::rbinom(n, 1, parameters$hib_vx_coverage), target)
}

#' Get children's ages
#'
#' @param timestep Current time
#' @param index optionally return a subset of the variable vector
#' @inheritParams background_mortality
get_age <- function(timestep, birth_t, index = NULL){
  timestep - birth_t$get_values(index = index)
}




