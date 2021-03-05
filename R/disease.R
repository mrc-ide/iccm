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
condition_exposure <- function(condition, variables, parameters, events){
  p <- parameters[[condition]]
  status <- paste0(condition, "_status")
  priors <- paste0(condition, "_prior_", parameters[[condition]]$type)

  function(timestep){
    # Get susceptible indices
    susceptibles <- variables$dia_status$get_index_of(values = "S")

    if(susceptibles$size() > 0){
      # Get ages (for maternal immunity estimation)
      ages <- get_age(timestep, variables, susceptibles)
      # Individual level heterogeneity modifier
      het <- variables$het$get_values(susceptibles)

      # Create an empty matrix to store the infection probabilities for each child X disease
      infection_prob <- matrix(NA, nrow = susceptibles$size(), ncol = p$groups)

      # Estimate infection probability for each disease within condition
      for(i in seq_along(p$type)){
        type <- p$type[i]
        # Maternal immunity modifier
        mi <- maternal_immunity(ages, p$mi_hl[i])
        # Prior infections
        pi <- variables[[priors[i]]]$get_values(susceptibles)
        # Infection immunity modifier
        ii <- exposure_immunity(pi, p$ii_shape[i], p$ii_rate[i])
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
      infected <- which(stats::runif(susceptibles$size(), 0, 1) < rowSums(infection_prob))

      if(length(infected) > 0){
        # Get indices of people to infect
        to_infect <- individual::filter_bitset(susceptibles, infected)

        # Draw which disease
        infection_prob <- infection_prob[infected, ,drop = FALSE]

        infection_type_index <- apply(infection_prob, 1, function(x){
          sample(p$index, 1, prob = x)
        })

        # Schedule disease life course
        infection_life_course(condition, infection_type_index, priors, p, to_infect,
                              variables, events)
      }
    }
  }
}

#' Schedule the disease life course
#'
#' @inheritParams condition_exposure
#' @param infection_type_index Indices of disease type
#' @param priors Name of prior exposure variables
#' @param p Condition-specific subset of parameters
#' @param target Indices of children
infection_life_course <- function(condition, infection_type_index, priors, p, target, variables, events){
  types <- p$type[infection_type_index]
  ut <- unique(types)
  # Make record of infection in each child's exposure history
  for(i in seq_along(ut)){
    sub_target <- individual::filter_bitset(target, which(types == ut[i]))
    if(sub_target$size() > 0){
      # Record which disease children are infected with
      variables[[paste0(condition, "_type")]]$queue_update(ut[i], sub_target)
      current_prior <- variables[[priors[i]]]$get_values(sub_target)
      variables[[priors[i]]]$queue_update(current_prior + 1, sub_target)
    }
  }

  # Change status -> symptomatic
  variables[[paste0(condition, "_status")]]$queue_update("I", target)

  # Schedule future changes
  symptomatic_length <- stats::rpois(target$size(), lambda = p$clin_dur[infection_type_index])
  ## Schedule recovery
  events[[paste0(condition, "_recover")]]$schedule(target, symptomatic_length)
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

  function(timestep){
    prev <- (parameters$population - variables$dia_status$get_size_of(values = "S")) / parameters$population
    renderer$render(name1, prev, timestep)

    for(i in seq_along(types)){
      sub_prev <- variables$dia_type$get_size_of(values = types[i]) / parameters$population
      renderer$render(names2[i], sub_prev, timestep)
    }
  }
}

