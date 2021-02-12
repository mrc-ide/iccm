#' Create individual states
#'
#' @param parameters Model parameters
create_states <- function(parameters){
  states <- list(
    NULLSTATE = individual::State$new("NULLSTATE", parameters$pop)
  )

  return(states)
}

#' Create individual variables
#'
#' @param parameters Model parameters
create_variables <- function(parameters){
  size <- parameters$pop

  initial_age <- floor(rtexp(size, rate = 1 / parameters$average_age, lower = parameters$age_lower, upper = parameters$age_upper))

  # Define variables
  demog_variables <- list(
    birth_t = individual::Variable$new("birth_t", -initial_age)
  )

  # Add disease variables
  disease_variables <- create_disease_variables(parameters)

  # Create list of all variables
  variables <- c(demog_variables, disease_variables)

  # Epidemiology
  variables$het = individual::Variable$new("het", heterogeneity(size, parameters$het_sd))
  # Interventions
  variables$llin = individual::Variable$new("llin", stats::rbinom(size, 1, parameters$llin_coverage))
  variables$rotavirus_vx = individual::Variable$new("rotavirus_vx", stats::rbinom(size, 1, parameters$rotavirus_vx_coverage))
  variables$pneumococcal_vx = individual::Variable$new("pneumococcal_vx", stats::rbinom(size, 1, parameters$pneumococcal_vx_coverage))
  variables$hib_vx = individual::Variable$new("hib_vx", stats::rbinom(size, 1, parameters$hib_vx_coverage))

  return(variables)
}

#' Create individuals
#'
#' @param states Model states
#' @param variables Model variables
#' @param events Model events
#'
#' @return List of individuals
create_individuals <- function(states, variables, events){
  individuals <- list(
    child = individual::Individual$new("child", states = states, variables = variables, events = events)
  )
  return(individuals)
}
