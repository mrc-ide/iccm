#' Create individual states
#'
#' @param parameters Model parameters
create_states <- function(parameters){
  states <- list(
    S = individual::State$new("S", parameters$pop)
  )

  return(states)
}

#' Create individual variables
#'
#' @param parameters Model parameters
create_variables <- function(parameters){
  size <- parameters$pop

  initial_age <- floor(rtexp(size, rate = 1 / parameters$average_age, upper = parameters$age_upper))

  # Define variables
  variables <- list(
    birth_t = individual::Variable$new("birth_t", -initial_age)
  )

  return(variables)
}

create_individuals <- function(states, variables){
  individuals <- list(
    child = individual::Individual$new("child", states = states, variables = variables)
  )
  return(individuals)
}
