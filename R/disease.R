#' Create disease variables
#'
#' Creates all variables associated with disease.
#'
#' @param parameters Model parameters
#'
#' @return A list of disease variables
create_disease_variables <- function(parameters){
  disease_variables <- list()

  # Diarrhoea
  ## Infection status
  #disease_variables$diarrhoea_status <- individual::Variable$new("diarrhoea_status", rep(0, parameters$population))
  ## Infection type
  # disease_variables$diarrhoea_disease_index <- individual::Variable$new("diarrhoea_disease_index", rep(0, parameters$population))
  ## Prior infections
  disease_variables$diarrhoea_bacteria_prior <- individual::Variable$new("diarrhoea_bacteria_prior", rep(0, parameters$population))
  disease_variables$diarrhoea_virus_prior <- individual::Variable$new("diarrhoea_virus_prior", rep(0, parameters$population))
  disease_variables$diarrhoea_parasite_prior <- individual::Variable$new("diarrhoea_parasite_prior", rep(0, parameters$population))
  disease_variables$diarrhoea_rotavirus_prior <- individual::Variable$new("diarrhoea_rotavirus_prior", rep(0, parameters$population))

  return(disease_variables)
}

#' Condition exposure
#'
#' Implements exposure to one of three conditions (diarrhoea, malaria, pneumonia). Selects
#' those infected, selects which disease they have been infected with and schedules the disease
#' life course.
#'
#' @param condition Condition: diarrhoea, malaria or pneumonia
#' @param individuals Model individuals
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
#' @param infection_disease Inidces of disease type
#' @param p Condition-specific subset of parameters
#' @param target Indices of children
#' @param api Model API
infection_life_course <- function(condition, infection_type_index, priors, p, target, variables, events){
  types <- p$type[infection_type_index]
  # Make record of infection in each child's exposure history
  for(i in seq_along(p$type)){
    sub_target <- individual::filter_bitset(target, which(types == p$type[i]))
    # Record which disease children are infected with
    variables[[paste0(condition, "_type")]]$queue_update(p$type[i], sub_target)
    current_prior <- variables[[priors[i]]]$get_values(sub_target)
    variables[[priors[i]]]$queue_update(current_prior + 1, sub_target)
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
#' @inheritParams condition_exposure
render_prevalence <- function(condition, variables, parameters, renderer){
  status <- paste0(condition, "_status")
  type <- paste0(condition, "_type")
  function(timestep){
    prev <- (parameters$population - variables$dia_status$get_size_of(values = "S")) / parameters$population
    renderer$render(paste0(condition, "_", "prevalence"), prev, timestep)

    for(i in parameters[[condition]]$type){
      sub_prev <- variables$dia_type$get_size_of(values = i) / parameters$population
      renderer$render(paste0(condition, "_", i, "_", "prevalence"), sub_prev, timestep)
    }
  }
}

