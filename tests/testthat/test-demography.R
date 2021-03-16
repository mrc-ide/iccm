# Set up simulation
timesteps <- 100
parameters = get_parameters()
variables <- create_variables(parameters)
events <- create_events(variables, parameters)
create_event_listeners(events, variables)
renderer <- mock_render(1)
processes <- create_processes(parameters, variables, renderer, events)

test_that("demography rendering works", {
  # Set up a mock API with 2 children of ages 0 and the age limit
  variables$birth_t <- mock_double(c(-1, -365*4.5))
  # Create the demography rendering function
  demog_renderer <- render_demography(variables, renderer)
  # Make the call using the mock API
  demog_renderer(timestep = 1)
  # Check that the arguments match the mocked API
  mockery::expect_args(renderer$render, 1, "N", 2, 1)
  mockery::expect_args(renderer$render, 2, "age_0", 1, 1)
  mockery::expect_args(renderer$render, 3, "age_1", 0, 1)
  mockery::expect_args(renderer$render, 4, "age_2", 0, 1)
  mockery::expect_args(renderer$render, 5, "age_3", 0, 1)
  mockery::expect_args(renderer$render, 6, "age_4", 1, 1)
  mockery::expect_args(renderer$render, 7, "average_age", 2.2541096, 1)
})

test_that("replace child works", {
  p <- get_parameters()
  p$population <- 3
  # Set coverages of interventions to 1 so we can check the new child is assigned them all
  p$rotavirus_vx_coverage = 1
  p$pneumococcal_vx_coverage = 1
  p$hib_vx_coverage = 1
  p$llin_coverage = 1
  variables <- create_variables(p)
  timestep <- 1
  events <- create_events(variables, p)

  # Mock up variables
  variables$dia_prior_bacteria <- mock_integer(c(1, 2, 3))
  variables$dia_prior_virus <- mock_integer(c(1, 2, 3))
  variables$dia_prior_parasite <- mock_integer(c(1, 2, 3))
  variables$dia_prior_rotavirus <- mock_integer(c(1, 2, 3))
  variables$dia_status <- mock_category( c("S", "A", "I", "V"), c("I", "I", "I"))
  variables$dia_type <- mock_category(c("None", p$dia$type), c("virus", "virus", "virus"))
  variables$birth_t <- mock_double(c(100, 100, 100))
  variables$llin <- mock_integer(c(0, 0, 0))
  variables$rotavirus_vx <- mock_integer(c(0, 0, 0))
  variables$hib_vx <- mock_integer(c(0, 0, 0))
  variables$pneumococcal_vx <- mock_integer(c(0, 0, 0))
  variables$het <- mock_double(c(1, 1))
  # Stub out the random het draw
  mockery::stub(replace_child, "heterogeneity", c(0.5, 1.5))

  to_replace <- individual::Bitset$new(p$population)
  to_replace <- to_replace$insert(1:2)

  # Replace the first 2 children
  replace_child(to_replace,  timestep, variables, p, events)

  # Checks
  expect_bitset_update(variables$dia_status$queue_update, "S", 1:2)
  expect_bitset_update(variables$dia_type$queue_update, "None", 1:2)
  expect_bitset_update(variables$dia_prior_bacteria$queue_update, 0, 1:2)
  expect_bitset_update(variables$dia_prior_virus$queue_update, 0, 1:2)
  expect_bitset_update(variables$dia_prior_parasite$queue_update, 0, 1:2)
  expect_bitset_update(variables$dia_prior_rotavirus$queue_update, 0, 1:2)
  expect_bitset_update(variables$birth_t$queue_update, -29, 1:2)
  expect_bitset_update(variables$llin$queue_update, c(1,1), 1:2)
  expect_bitset_update(variables$rotavirus_vx$queue_update, c(1,1), 1:2)
  expect_bitset_update(variables$hib_vx$queue_update, c(1,1), 1:2)
  expect_bitset_update(variables$pneumococcal_vx$queue_update, c(1,1), 1:2)
  expect_bitset_update(variables$het$queue_update, c(0.5, 1.5), 1:2)
})

test_that("get age", {
  parameters <- get_parameters()
  parameters$population <- 2
  variables <- create_variables(parameters)

  expect_equal(get_age(0, variables), -variables$birth_t$get_values())
  expect_equal(get_age(100, variables), -variables$birth_t$get_values() + 100)
  expect_equal(get_age(0, variables, 1), -variables$birth_t$get_values()[1])
  expect_equal(get_age(0, variables, 2), -variables$birth_t$get_values()[2])
})


test_that("background mortality", {
  p <- get_parameters()
  p$population <- 3
  renderer <- mock_render(1)
  variables <- create_variables(p)
  variables$dia_status <- mock_category( c("S", "A", "I", "V"), c("I", "I", "I"))
  events <- create_events(variables, p)

  # Create the background mortality function
  bm <- background_mortality(p, variables, renderer, events)

  # Stub to force #2 to die
  mockery::stub(bm, "sample.int", 2)
  bm(1)

  mockery::expect_args(renderer$render, 1, "background_mortality", 1, 1)
  # Check changes have filtered through to replace child
  expect_bitset_update(variables$dia_status$queue_update, "S", 2)
})

test_that("graduation", {
  p <- get_parameters()
  p$population <- 3
  renderer <- mock_render(1)
  variables <- create_variables(p)
  events <- create_events(variables, p)
  variables$dia_status <- mock_category( c("S", "A", "I", "V"), c("I", "I", "I"))
  variables$birth_t <- mock_double(-c((365 * 5) - 1, 100, 200))

  # Create the graduation mortality function
  gr <- graduate(p, variables, renderer, events)
  gr(0)

  mockery::expect_args(renderer$render, 1, "graduation", 1, 0)
  # Check changes have filtered through to replace child
  expect_bitset_update(variables$dia_status$queue_update, "S", 1)
})
