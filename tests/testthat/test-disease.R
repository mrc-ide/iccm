timesteps <- 100
parameters = get_parameters()
parameters$population <- 3
variables <- create_variables(parameters)
events <- create_events(variables, parameters)
create_event_listeners(events, variables)
renderer <- individual::Render$new(timesteps)
processes <- create_processes(parameters, variables, renderer, events)

test_that("prevalence rendering works", {
  renderer <- mock_render(2)
  parameters$population <- 4
  variables$dia_status <- mock_category( c("S", "A", "I", "V"), c("I", "I", "I", "S"))
  variables$dia_type <- mock_integer(c(2, 2, 3, 0))
  variables$dia_prior_bacteria <- mock_integer(rep(0,4))
  variables$dia_prior_virus <- mock_integer(rep(0,4))
  variables$dia_prior_parasite <- mock_integer(rep(0,4))
  variables$dia_prior_rotavirus <- mock_integer(rep(0,4))

  prevalence_renderer <- render_prevalence("dia", variables, parameters, renderer)
  prevalence_renderer(timestep  = 1)

  mockery::expect_args(renderer$render, 1, "dia_prevalence", 0.75, 1)
  mockery::expect_args(renderer$render, 2, "dia_bacteria_prevalence", 0, 1)
  mockery::expect_args(renderer$render, 3, "dia_virus_prevalence", 0.5, 1)
  mockery::expect_args(renderer$render, 4, "dia_parasite_prevalence", 0.25, 1)
  mockery::expect_args(renderer$render, 5, "dia_rotavirus_prevalence", 0, 1)
})

test_that("recover event works", {
  parameters$population <- 4
  variables$dia_status <- mock_category( c("S", "A", "I", "V"), c("I", "I", "I", "S"))
  variables$dia_type <- mock_integer(c(2, 2, 3, 0))
  recovery_event <- recover_event(variables, "dia")
  recovery_event(1, 1)

  mockery::expect_args(variables$dia_status$queue_update, 1, "S", 1)
  mockery::expect_args(variables$dia_type$queue_update, 1, 0, 1)
})
