#' Run an iccm simulation
#'
#' @param timesteps The number of timesteps (days) to run the model.
#' @param parameters Model parameter default overrides
#' @param long Return long output?
#'
#' @return Simulation output
#' @export
run_simulation <- function(timesteps, parameters = NULL, long = TRUE){
  # TODO: need to fix this is using nested list of parameters
  if(is.null(parameters)){
    parameters <- get_parameters()
  } else {
    parameters = get_parameters(user_overwrite = parameters)
  }

  variables <- create_variables(parameters)
  events <- create_events(variables, parameters)
  renderer <- individual::Render$new(timesteps)
  initialise_render_defaults(renderer, parameters)
  initialise_events(events, variables, parameters)
  create_event_listeners(events, variables, parameters, renderer)
  processes <- create_processes(parameters, variables, renderer, events)

  individual::simulation_loop(variables = unlist(variables),
                              events = unlist(events),
                              processes = processes,
                              timesteps = timesteps)
  output <- renderer$to_dataframe()# %>%
  #replace_render_na()

  if(long){
    output <- convert_to_long(output)
  }

  return(output)
}
