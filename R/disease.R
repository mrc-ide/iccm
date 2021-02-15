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
  disease_variables$diarrhoea_status <- individual::Variable$new("diarrhoea_status", rep(0, parameters$population))
  ## Infection type
  disease_variables$diarrhoea_disease_index <- individual::Variable$new("diarrhoea_disease_index", rep(0, parameters$population))
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
condition_exposure <- function(condition, individuals, variables, parameters, events){
  function(api){
    # Get susceptible indices
    susceptibles <- which(api$get_variable(individuals$child, variables[[paste0(condition, "_status")]]) == 0)

    if(length(susceptibles) > 0){
      # Isolate condition parameters
      p <- parameters[[condition]]

      # Get ages (for maternal immunity estimation)
      ages <- get_age(api, individuals, variables, susceptibles)

      # Create an empty matrix to store the infection probabilities for each child X disease
      infection_prob <- matrix(NA, nrow = length(susceptibles), ncol = p$groups)

      # Estimate infection probability for each disease within condition
      for(i in p$index){
        type <- p$type[i]
        # Maternal immunity modifier
        mi <- maternal_immunity(ages, p$mi_hl[i])
        # Prior infections
        pi <- api$get_variable(individuals$child, variables[[paste0(condition, "_", type,"_prior")]], susceptibles)
        # Infection immunity modifier
        ii <- exposure_immunity(pi, p$ii_shape[i], p$ii_rate[i])
        # Individual level heterogeneity modifier
        het <- api$get_variable(individuals$child, variables$het, susceptibles)
        # Vaccine modifier
        vi <- vaccine_impact(type = type, index = i, target = susceptibles, ages = ages, p = p, individuals = individuals, variables = variables, api = api)
        # LLIN modifier
        li <- llin_impact(type = type, target = susceptibles, p = p, individuals = individuals, variables = variables, api = api)
        # Community impacts modifier (vaccine or LLIN)
        ci <- community_impact(type = type, index = i, p = p)
        # Estimate infection rate
        # TODO: treatment prophylaxsis
        infection_rate <- p$sigma[i] * mi * ii * het * vi * li * ci
        # Estimate infection probability
        infection_prob[,i] <- rate_to_prob(infection_rate)
      }
      # Draw those infected
      infected <- stats::runif(length(susceptibles), 0, 1) < rowSums(infection_prob)

      if(sum(infected) > 0){
        # Get indices of people to infect
        to_infect <- susceptibles[infected]
        # Draw which disease
        infection_prob <- infection_prob[infected, ,drop = FALSE]

        infection_disease <- apply(infection_prob, 1, function(x){
          sample(p$index, 1, prob = x)
        })
        # Schedule disease life course
        infection_life_course(condition, infection_disease, p, to_infect,
                              api, individuals, variables, events)
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
infection_life_course <- function(condition, infection_disease, p, target, api, individuals, variables, events){
  # Record which disease children are infected with
  api$queue_variable_update(individuals$child, variables[[paste0(condition, "_disease_index")]], infection_disease, target)
  # Make record of infection in each child's exposure history
  uid <- unique(infection_disease)
  for(i in uid){
    var_name <- paste0(condition, "_", p$type[i], "_prior")
    sub_target <- target[infection_disease == i]
    current_prior <- api$get_variable(individuals$child, variables[[var_name]], sub_target)
    api$queue_variable_update(individuals$child, variables[[var_name]], current_prior + 1, sub_target)
  }

  # Change status -> symptomatic
  api$queue_variable_update(individuals$child, variables[[paste0(condition, "_status")]], 1, target)

  # Schedule future changes
  symptomatic_length <- stats::rpois(length(target), lambda = p$clin_dur[infection_disease])
  ## Schedule recovery
  api$schedule(events[[paste0(condition, "_recover")]], target, symptomatic_length)
}




#' Render prevalence outputs for a condition
#'
#' @inheritParams condition_exposure
render_prevalence <- function(condition, individuals, variables, parameters){
  function(api){
    prev <- 1 - mean(api$get_variable(individuals$child, variables[[paste0(condition, "_status")]]) == 0)
    api$render(paste0(condition, "_", "prevalence"), prev)

    infection_type = api$get_variable(individuals$child, variables[[paste0(condition, "_disease_index")]])
    for(i in parameters[[condition]]$index){
      sub_prev <- mean(infection_type == i)
      api$render(paste0(condition, "_", parameters[[condition]][["type"]][i], "_", "prevalence"), sub_prev)
    }
  }
}

