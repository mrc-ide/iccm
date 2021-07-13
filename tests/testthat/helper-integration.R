mock_category <- function(...) {
  v <- individual::CategoricalVariable$new(...)
  list(
    get_index_of = v$get_index_of,
    get_size_of = v$get_size_of,
    queue_update = mockery::mock()
  )
}

mock_double <- function(...) {
  v <- individual::DoubleVariable$new(...)
  list(
    get_values = v$get_values,
    queue_update = mockery::mock()
  )
}

mock_integer <- function(...) {
  v <- individual::IntegerVariable$new(...)
  list(
    get_values = v$get_values,
    get_index_of = v$get_index_of,
    queue_update = mockery::mock()
  )
}

mock_render <- function(...) {
  v <- individual::Render$new(...)
  list(
    render = mockery::mock()
  )
}

mock_event_class <- R6::R6Class("mock_event", list(

  user_scheduled = NULL,
  set_user_scheduled = function(x) self$user_scheduled = x,
  get_scheduled = function() self$user_scheduled,

  schedule = NULL,
  set_schedule = function(x) self$schedule = x,

  clear_schedule = NULL,
  set_clear_schedule = function(x) self$clear_schedule = x
))

mock_event <- function(schedule = NULL) {
  me <- mock_event_class$new()
  me$set_user_scheduled(schedule)
  me$set_schedule(mockery::mock())
  me$set_clear_schedule(mockery::mock())
  return(me)
}

expect_bitset_update <- function(mock, value, index, call = 1) {
  expect_equal(mockery::mock_args(mock)[[call]][[1]], value)
  if("Bitset" %in% is(mockery::mock_args(mock)[[call]][[2]])){
    expect_equal(mockery::mock_args(mock)[[call]][[2]]$to_vector(), index)
  } else {
    expect_equal(mockery::mock_args(mock)[[call]][[2]], index)
  }
}
