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
        mi <- maternal_immunity(ages, p$mi[i])
        # Estimate infection rate
        # TODO: 1) Exposure driven immunity, 2) Heterogeneity, 3) intervention modifiers
        infection_rate <- p$sigma[i] * mi
        # Estimate infection probability
        infection_prob[,i] <- rate_to_prob(infection_rate)
      }
      # Draw those infected
      infected <- stats::runif(length(susceptibles), 0, 1) < rowSums(infection_prob)

      if(sum(infected) > 0){
        # Get indices of people to infect
        to_infect <- susceptibles[infected]
        # Draw which disease
        infection_prob <- infection_prob[infected, ]

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

