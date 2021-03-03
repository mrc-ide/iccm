mock_category <- function(...) {
  v <- individual::CategoricalVariable$new(...)
  list(
    get_index_of = v$get_index_of,
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

mock_render <- function(...) {
  v <- individual::Render$new(...)
  list(
    render = mockery::mock()
  )
}

mock_event <- function(event) {
  list(
    get_scheduled = function(...) event$get_scheduled(...),
    schedule = mockery::mock(),
    clear_schedule = mockery::mock()
  )
}

