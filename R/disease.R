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
  disease_record_label <- paste0(condition, "_type")
  status_label <- paste0(condition, "_status")
  prior_labels <- paste0(condition, "_prior_", parameters[[condition]]$type)

  function(timestep){
    # Get susceptible indices
    susceptibles <- variables[[status_label]]$get_index_of(values = "S")

    if(susceptibles$size() > 0){
      # Get ages (for maternal immunity estimation)
      ages <- get_age(timestep, variables, susceptibles)
      # Individual level heterogeneity modifier
      het <- variables$het$get_values(susceptibles)

      # Create an empty matrix to store the infection probabilities for each child X disease
      infection_prob <- matrix(0, nrow = susceptibles$size(), ncol = p$groups)

      # Estimate infection probability for each disease within condition
      for(i in seq_along(p$type)){
        type <- p$type[i]
        # Maternal immunity modifier
        mi <- maternal_immunity(ages, p$mi_hl[i])
        # Prior infections
        pi <- variables[[prior_labels[i]]]$get_values(susceptibles)
        # Infection immunity modifier
        ii <-  exposure_immunity(pi, p$ii_shape[i], p$ii_rate[i])
        # Vaccine modifier
        vi <- vaccine_impact(type = type, index = i, target = susceptibles, ages = ages, p = p, variables = variables)
        # LLIN modifier
        li <- llin_impact(type = type, target = susceptibles, p = p, variables = variables)
        # Community impacts modifier (vaccine or LLIN)
        ci <- community_impact(type = type, index = i, p = p)
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
        disease_index <- apply(infection_prob, 1, sample_disease, n = length(p$type))

        # Update infected individuals
        update_disease_record(p$type[disease_index], to_infect, disease_record_label, variables)
        increment_prior_exposure_counter(to_infect, disease_index, prior_labels, variables)
        infection_life_course(to_infect, disease_index, status_label, condition, p, variables, events, renderer, timestep)


      }
    }
  }
}


#' Schedule the disease life course
#'
#' @inheritParams condition_exposure
#' @param type Infection type
#' @param prior_name Name of prior variable
#' @param duration Durations of clinical episodes
#' @param target Target children
#' @param timestep Timestep
infection_life_course <- function(target, disease_index, status_label, condition, p, variables, events, renderer, timestep){
  n <- length(disease_index)
  clinical_duration <- rpois(n, p$clin_dur[disease_index])
  # asymptomatic_durations <- rpois(n, p$asymp_dur[disease_index])
  symptomatic <- runif(n) < rep(1, n)
  time_until_severe <- days_until(p$daily_prob_severe[disease_index])
  time_until_death <- time_until_severe + days_until(p$daily_prob_death[disease_index])

  outcome <- dplyr::case_when(
    # I -> R
    symptomatic & time_until_severe > clinical_duration ~ 1,
    # I -> V -> R
    symptomatic & time_until_severe <= clinical_duration & time_until_death > clinical_duration ~ 2,
    # I -> V -> D
    symptomatic & time_until_death <= clinical_duration ~ 3)

  # Change status -> symptomatic (current all)
  variables[[status_label]]$queue_update("I", target)
  # Schedule recovery
  to_recover_index = outcome %in% c(1, 2)
  to_recover = individual::filter_bitset(target, which(to_recover_index))
  events[[paste0(condition, "_recover")]]$schedule(to_recover, delay = clinical_duration[to_recover_index])
  # Schedule Severe
  to_severe_index <- outcome %in% c(2, 3)
  to_severe = individual::filter_bitset(target, which(to_severe_index))
  events[[paste0(condition, "_progress_severe")]]$schedule(to_severe, delay = time_until_severe[to_severe_index])
  # Schedule Death
  to_die_index <- outcome == 3
  to_die = individual::filter_bitset(target, which(to_die_index))
  events$dia_death$schedule(to_die, delay = time_until_death[to_die_index])

  render_incidence(p$type[disease_index], condition, timestep, renderer)
}

render_incidence <- function(diseases, condition, timestep, renderer){
  inc_tab <- as.data.frame(table(diseases))
  for(i in 1:nrow(inc_tab)){
    renderer$render(paste0(condition, "_", inc_tab[i,1], "_incidence"), inc_tab[i,2], timestep)
  }
}




#' Render prevalence outputs for a condition
#'
#' @param renderer Model renderer
#' @inheritParams condition_exposure
render_prevalence <- function(condition, variables, parameters, renderer){
  status <- paste0(condition, "_status")
  type <- paste0(condition, "_type")
  name1 <- paste0(condition, "_", "prevalence")
  names2 <- paste0(condition, "_", parameters[[condition]]$type, "_", "prevalence")
  types <- parameters[[condition]]$type
  priors <- paste0(condition, "_prior_", parameters[[condition]]$type)
  names3 <- paste0(condition, "_", parameters[[condition]]$type, "_", "prior")

  function(timestep){
    prev <- (parameters$population - variables$dia_status$get_size_of(values = "S")) / parameters$population
    renderer$render(name1, prev, timestep)

    for(i in seq_along(types)){
      sub_prev <- variables$dia_type$get_size_of(values = types[i]) / parameters$population
      renderer$render(names2[i], sub_prev, timestep)

      pe <- mean(variables[[priors[i]]]$get_values())
      renderer$render(names3[i], pe, timestep)
    }
  }
}



increment_prior_exposure_counter <- function(target, disease_index, prior_labels, variables){
  for(i in seq_along(prior_labels)){
    sub_target <- individual::filter_bitset(target, which(disease_index == i))
    if(sub_target$size() > 0){
      current_prior <- variables[[prior_labels[i]]]$get_values(sub_target)
      variables[[prior_labels[i]]]$queue_update(current_prior + 1, sub_target)
    }
  }
}

update_disease_record <- function(disease, target, disease_record_label, variables){
  unique_diseases <- unique(disease)
  for(i in seq_along(unique_diseases)){
    sub_target <- individual::filter_bitset(target, which(disease == unique_diseases[i]))
    variables[[disease_record_label]]$queue_update(unique_diseases[i], sub_target)
  }
}

sample_disease <- function(p, n){
  sample.int(n = n, size = 1, prob = p)
}

days_until <- function(daily_probability){
  daily_probability[daily_probability == 0] <- 0.00000000000001
  days <- rgeom(length(daily_probability), daily_probability)
  return(days)
}
