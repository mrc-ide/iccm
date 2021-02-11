#' Run an iccm simulation
#'
#' @param timesteps The number of timesteps (days) to run the model.
#' @param parameters Model parameter default overrides
#' @param long Return long output?
#'
#' @return Simulation output
#' @export
#'
#' @examples
#' simulation1 <- run_simulation(timesteps = 365)
run_simulation <- function(timesteps, parameters = NULL, long = TRUE){
  # TODO: need to fix this is using nested list of parameters
  if(is.null(parameters)){
    parameters = get_parameters()
  } else {
  #  parameters = get_parameters(overrides = parameters)
  }

  states <- create_states(parameters)
  variables <- create_variables(parameters)
  events <- create_events(individuals, variables)
  individuals <- create_individuals(states, variables, events)
  processes <- create_processes(parameters, individuals, variables, events)

  output <- individual::simulate(individuals = individuals,
                       processes = processes,
                       end_timestep = timesteps)

  if(long){
    output <- convert_to_long(output)
  }

  return(output)
}
