timesteps <- 100
parameters = get_parameters()
parameters$population <- 3
variables <- create_variables(parameters)
events <- create_events(variables, parameters)
create_event_listeners(events, variables)
renderer <- individual::Render$new(timesteps)
processes <- create_processes(parameters, variables, renderer, events)

test_that("infection life course works", {
  #variables$dia_status <- mock_category(c("S", "A", "I", "V"), c("S", "S", "S"))
  variables$dia_type <- mock_category(c("None", parameters$dia$type), c("None", "None", "None"))

  tar <- variables$dia_status$get_index_of(values = "S")
  infection_life_course("dia", "virus", tar, "dia_prior_virus", 10, variables, events)

  expect_bitset_update(variables$dia_type$queue_update, parameters$dia$type[2], tar$to_vector())
})

test_that("disease rendering works", {
  renderer <- mock_render(2)
  parameters$population <- 4
  variables$dia_status <- mock_category( c("S", "A", "I", "V"), c("I", "I", "I", "S"))
  variables$dia_type <- mock_category(c("None", parameters$dia$type), c("virus", "virus", "parasite", "None"))
  variables$dia_prior_bacteria <- mock_integer(rep(0,4))
  variables$dia_prior_virus <- mock_integer(rep(0,4))
  variables$dia_prior_parasite <- mock_integer(rep(0,4))
  variables$dia_prior_rotavirus <- mock_integer(rep(0,4))

  prevalence_renderer <- render_prevalence("dia",  variables, parameters, renderer)
  prevalence_renderer(timestep  = 1)

  mockery::expect_args(renderer$render, 1, "dia_prevalence", 0.75, 1)
  mockery::expect_args(renderer$render, 2, "dia_bacteria_prevalence", 0, 1)
  mockery::expect_args(renderer$render, 3, "dia_bacteria_prior", 0, 1)
  mockery::expect_args(renderer$render, 4, "dia_virus_prevalence", 0.5, 1)
  mockery::expect_args(renderer$render, 5, "dia_virus_prior", 0, 1)
  mockery::expect_args(renderer$render, 6, "dia_parasite_prevalence", 0.25, 1)
  mockery::expect_args(renderer$render, 7, "dia_parasite_prior", 0, 1)
  mockery::expect_args(renderer$render, 8, "dia_rotavirus_prevalence", 0, 1)
  mockery::expect_args(renderer$render, 9, "dia_rotavirus_prior", 0, 1)
})

test_that("recover event works", {
  parameters$population <- 4
  variables$dia_status <- mock_category( c("S", "A", "I", "V"), c("I", "I", "I", "S"))
  variables$dia_type <- mock_category(c("None", parameters$dia$type), c("virus", "virus", "parasite", "None"))
  recovery_event <- recover_event(variables, "dia")
  recovery_event(1, 1)

  mockery::expect_args(variables$dia_status$queue_update, 1, "S", 1)
  mockery::expect_args(variables$dia_type$queue_update, 1, "None", 1)
})
