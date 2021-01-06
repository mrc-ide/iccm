# Create a mock api for testing
mock_api <- function(values, parameters = list(), timestep = 1) {
  list(
    get_state = function(individual, ...) {
      subset <- c()
      for (state in list(...)) {
        subset <- c(subset, values[[individual$name]][[state$name]])
      }
      subset
    },
    get_variable = function(individual, variable, index=NULL) {
      if (!is.null(index)) {
        return(values[[individual$name]][[variable$name]][index])
      }
      values[[individual$name]][[variable$name]]
    },
    queue_state_update = mockery::mock(),
    queue_variable_update = mockery::mock(),
    schedule = mockery::mock(),
    clear_schedule = mockery::mock(),
    get_scheduled = mockery::mock(),
    get_timestep = function() timestep,
    get_parameters = function() parameters,
    render = mockery::mock()
  )
}
