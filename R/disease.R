#' Condition exposure
#'
#' Implements exposure to one of three conditions (diarrhoea, malaria, pneumonia). Selects
#' those infected, selects which disease they have been infected with and schedules the disease
#' life course.
#'
#' @param condition Condition: diarrhoea, malaria or pneumonia
#' @param variables Model variables
#' @param parameters Model parameters
#' @param events Model events
#' @param renderer Model renderer
condition_exposure <- function(condition, variables, parameters, events, renderer){
  p <- parameters[[condition]]
  condition_disease <- paste0(condition, "_disease")
  condition_status <- paste0(condition, "_status")
  condition_fever <- paste0(condition, "_fever")
  condition_prior_disease <- paste0(condition, "_prior_", parameters[[condition]]$disease)
  condition_recover <- paste0(condition, "_recover")
  condition_symptom_start <- paste0(condition, "_symptom_start")

  function(timestep){
    # Get susceptible indices
    susceptibles <- variables[[condition_status]]$get_index_of(0)

    if(susceptibles$size() > 0){
      # Get ages (for maternal immunity estimation)
      ages <- get_age(timestep, variables, susceptibles)
      # Individual level heterogeneity modifier
      het <- variables$het$get_values(susceptibles)

      # Create an empty matrix to store the infection probabilities for each child X disease
      infection_prob <- matrix(0, nrow = susceptibles$size(), ncol = p$groups)

      # Estimate infection probability for each disease within condition
      for(i in seq_along(p$disease)){
        disease <- p$disease[i]
        # Maternal immunity modifier
        mi <- maternal_immunity(ages, p$mi_hl[i])
        # Prior infections
        pi <- variables[[condition_prior_disease[i]]]$get_values(susceptibles)
        # Infection immunity modifier
        ii <-  exposure_immunity(pi, p$ii_shape[i], p$ii_rate[i])
        # Vaccine modifier
        vi <- vaccine_impact(disease = disease, index = i, target = susceptibles, ages = ages, p = p, variables = variables)
        # LLIN modifier
        li <- llin_impact(disease = disease, target = susceptibles, p = p, variables = variables)
        # Community impacts modifier (vaccine or LLIN)
        ci <- community_impact(disease = disease, index = i, p = p)
        # Estimate infection rate
        # TODO: treatment prophylaxsis
        infection_rate <- p$sigma[i] * mi * ii * het * vi * li * ci
        # Estimate infection probability
        infection_prob[,i] <- rate_to_prob(infection_rate)
      }

      # Draw those infected
      infected <- stats::runif(susceptibles$size(), 0, 1) < rowSums(infection_prob)
      if(sum(infected) > 0){
        to_infect <- individual::filter_bitset(susceptibles, which(infected))
        # Draw which disease
        infection_prob <- infection_prob[infected, , drop = FALSE]
        disease_index <- apply(infection_prob, 1, sample_disease, n = length(p$disease))

        # Update infected individuals
        variables[[condition_disease]]$queue_update(disease_index, to_infect)
        increment_prior_exposure_counter(disease_index, to_infect, condition_prior_disease, variables)
        variables[[condition_status]]$queue_update(2, to_infect)
        variables[[condition_symptom_start]]$queue_update(timestep, to_infect)
        to_fever <- to_infect$copy()$sample(p$prob_fever[disease_index])
        variables[[condition_fever]]$queue_update(1, to_fever)
        clinical_duration <- stats::rpois(to_infect$size(), p$clin_dur[disease_index])
        events[[condition_recover]]$schedule(to_infect, delay = clinical_duration)
        render_incidence(disease_index, condition, p$disease, timestep, renderer)

        # Schedule treatment
        to_treat <- to_infect$sample(parameters$treatment_seeking$prob_seek_treatment)
        to_treat_hf <- variables$provider_preference$get_index_of("HF")$and(to_treat)
        to_treat_chw <- variables$provider_preference$get_index_of("CHW")$and(to_treat)
        to_treat_private <- variables$provider_preference$get_index_of("Private")$and(to_treat)

        events$hf_treatment$schedule(to_treat_hf, delay = parameters$hf$travel_time + 1 + stats::rpois(to_treat_hf$size(), parameters$treatment_seeking$treat_seeking_behaviour_delay))
        events$chw_treatment$schedule(to_treat_chw, delay = parameters$chw$travel_time + 1 + stats::rpois(to_treat_chw$size(), parameters$treatment_seeking$treat_seeking_behaviour_delay))
        events$private_treatment$schedule(to_treat_private, delay = parameters$private$travel_time + 1 + stats::rpois(to_treat_private$size(), parameters$treatment_seeking$treat_seeking_behaviour_delay))
      }
    }
  }
}

#' Progress to severe disease
#'
#' Sample infected individuals to progress to severe disease
#'
#' @inheritParams condition_exposure
progress_severe <- function(condition, parameters, variables, events){
  dps <- parameters[[condition]]$daily_prob_severe
  condition_status <- paste0(condition, "_status")
  condition_disease <- paste0(condition, "_disease")
  condition_fever <- paste0(condition, "_fever")

  function(timestep){
    # Symptomatic individuals
    target <- variables[[condition_status]]$get_index_of(2)
    # Disease indices
    indices <- variables[[condition_disease]]$get_values(target)
    # Disease-specific probability of becoming severe
    probs <- dps[indices]
    # Sample and progress
    target <- target$sample(probs)
    variables[[condition_status]]$queue_update(3, target)
    variables[[condition_fever]]$queue_update(1, target)

    # Schedule treatment
    to_treat <- target$sample(parameters$treatment_seeking$prob_seek_treatment_severe)
    to_treat_hf <- variables$provider_preference$get_index_of("HF")$and(target)
    to_treat_chw <- variables$provider_preference$get_index_of("CHW")$and(target)
    to_treat_private <- variables$provider_preference$get_index_of("Private")$and(target)

    events$hf_treatment$schedule(to_treat_hf, delay = parameters$hf$travel_time + 1)
    events$chw_treatment$schedule(to_treat_chw, delay = parameters$chw$travel_time + 1)
    events$private_treatment$schedule(to_treat_private, delay = parameters$private$travel_time + 1)
  }
}

#' Death from disease
#'
#' Sample severely-infected individuals to die
#'
#' @inheritParams condition_exposure
die <- function(condition, parameters, variables, events, renderer){
  dpd <- parameters[[condition]]$daily_prob_death
  diseases <- parameters[[condition]]$disease
  condition_status <- paste0(condition, "_status")
  condition_disease <- paste0(condition, "_disease")
  condition_disease_mortality <- paste0(condition, "_", diseases, "_", "mortality")

  function(timestep){
    # Individuals with severe illness
    target <- variables[[condition_status]]$get_index_of(3)
    # Disease indices
    indices <- variables[[condition_disease]]$get_values(target)
    # Disease-specific probability of dieing | severe illness
    probs <- dpd[indices]
    # Sample and death
    target <- target$sample(probs)
    replace_child(target, timestep, variables, parameters, events)
    # Record disease-specific mortality
    death_cause <- variables[[condition_disease]]$get_values(target)
    for(i in seq_along(diseases)){
      renderer$render(condition_disease_mortality[i], sum(death_cause == i), timestep)
    }
  }
}

#' Record new infections in individual's history of infection.
#'
#' @param target Bitset of newly infected individuals
#' @param new_infections Indices of new infections
#' @param condition_prior_disease Names of prior variables
#' @inheritParams condition_exposure
increment_prior_exposure_counter <- function(new_infections, target, condition_prior_disease, variables){
  for(i in seq_along(condition_prior_disease)){
    sub_target <- individual::filter_bitset(target, which(new_infections == i))
    if(sub_target$size() > 0){
      current_prior <- variables[[condition_prior_disease[i]]]$get_values(sub_target)
      variables[[condition_prior_disease[i]]]$queue_update(current_prior + 1, sub_target)
    }
  }
}

#' Sample disease if infected with a condition
#'
#' @param p Disease-specific infection probabilities
#' @param n Number of diseases
#'
#' @return Sampled disease that will infect the individual
sample_disease <- function(p, n){
  sample.int(n = n, size = 1, prob = p)
}

#' Time since onset
#'
#' Estimate the time since the onset of symptoms for a given condition
#'
#' @param target Target children
#' @param condition Condition
#' @param variables Model variables
#' @param timestep Timestep
#'
#' @return Vector of times
time_since_onset <- function(target, condition, variables, timestep){
  timestep - variables[[paste0(condition, "_symptom_start")]]$get_values(target)
}

#' Record prevalence
#'
#' Record overall (condition) and disaggregated (disease) prevalence
#'
#' @inheritParams condition_exposure
render_prevalence <- function(condition, variables, parameters, renderer){
  diseases <- parameters[[condition]]$disease
  condition_disease <- paste0(condition, "_disease")
  condition_prevalence <- paste0(condition, "_", "prevalence")
  condition_diseases_prevalence <- paste0(condition, "_", diseases, "_", "prevalence")

  function(timestep){
    current_diseases <- variables[[condition_disease]]$get_values()
    renderer$render(condition_prevalence, mean(current_diseases != 0), timestep)
    for(i in seq_along(diseases)){
      renderer$render(condition_diseases_prevalence[i], mean(current_diseases == i), timestep)
    }
  }
}

#' Record incidence
#'
#' Record overall (condition) and disaggregated (disease) incidence of new infection
#'
#' @param new_infections Indices of new infections
#' @param diseases vector of diseases for condition
#' @param timestep Current time
#' @inheritParams condition_exposure
render_incidence <- function(new_infections, condition, diseases, timestep, renderer){
  renderer$render(paste0(condition, "_incidence"), length(new_infections), timestep)
  for(i in seq_along(diseases)){
    renderer$render(paste0(condition, "_", diseases[i], "_incidence"), sum(new_infections == i), timestep)
  }
}

#' Record prior exposure
#'
#' Record the average number of prior disease-specific infections
#'
#' @param renderer Model renderer
#' @inheritParams condition_exposure
render_prior_exposure <- function(condition, variables, parameters, renderer){
  diseases <- parameters[[condition]]$disease
  condion_prior_disease <- paste0(condition,  "_prior_", diseases)
  function(timestep){
    for(i in seq_along(diseases)){
      renderer$render(condion_prior_disease[i], mean(variables[[condion_prior_disease[i]]]$get_values()), timestep)
    }
  }
}

#' Record fever prevalence
#'
#' Record prevalence of fever
#' @inheritParams condition_exposure
render_fevers <- function(variables, parameters, renderer){
  function(timestep){
    x <- variables$dia_fever$get_index_of(1)
    renderer$render("Fever_prevalence", x$size() / parameters$population, timestep)
  }
}

