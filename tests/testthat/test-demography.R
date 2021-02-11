# Set up simulation
parameters <- get_parameters()
states <- create_states(parameters)
variables <- create_variables(parameters)
events <- create_events(individuals, variables)
individuals <- create_individuals(states, variables, events)
processes <- create_processes(parameters, individuals, variables, events)

# Set up a mock API with 2 children of ages 0 and the age limit
api <- mock_api(
  list(
    child = list(
      S = c(1),
      S = c(2),
      birth_t = -c(100, parameters$age_upper)
    )
  ),
  parameters = parameters,
  timestep = 0
)

test_that("demography rendering works", {
  # Create the demography rendering function
  demog_renderer <- render_demography(individuals, variables)
  # Make the call using the mock API
  demog_renderer(api)
  # Check that the arguments match the mocked API
  mockery::expect_args(api$render, 1, 'N', 2)
  mockery::expect_args(api$render, 2, 'age_0', 1)
  mockery::expect_args(api$render, 3, 'age_1', 0)
  mockery::expect_args(api$render, 4, 'age_2', 0)
  mockery::expect_args(api$render, 5, 'age_3', 0)
  mockery::expect_args(api$render, 6, 'age_4', 1)
})

test_that("background mortality works", {
  # Mock up so that the background mortality draw selects individual 2
  m <- mockery::mock(c(0, 1))
  mockery::stub(background_mortality, 'stats::rbinom', m)

  # Create the background mortality function
  bm <- background_mortality(parameters, individuals, variables)
  # Make the call using the mock API
  bm(api)
  # Check that the arguments match the mocked API
  mockery::expect_args(api$queue_variable_update, 1, individuals$child, variables$birth_t, -parameters$age_lower, 2)
})

test_that("graduation works", {
  # Create the graduation function
  gr <- graduate(parameters, individuals, variables)
  # Make the call using the mock API
  gr(api)
  # Check that the arguments match the mocked API
  mockery::expect_args(api$queue_variable_update, 1, individuals$child, variables$birth_t, -parameters$age_lower, 2)
})
