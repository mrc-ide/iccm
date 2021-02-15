# Set up simulation
parameters <- get_parameters()
states <- create_states(parameters)
variables <- create_variables(parameters)
events <- create_events(individuals, variables)
individuals <- create_individuals(states, variables, events)
processes <- create_processes(parameters, individuals, variables, events)

test_that("demography rendering works", {
  # Set up a mock API with 2 children of ages 0 and the age limit
  api <- mock_api(
    list(
      child = list(
        S = c(1, 2),
        birth_t = -c(100, parameters$age_upper)
      )
    ),
    parameters = parameters,
    timestep = 0
  )

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

test_that("replace child works", {
  p <- get_parameters()
  p$population <- 1
  # Set coverages of interventions to 1 so we can check the new child is assigned them all
  p$rotavirus_vx_coverage = 1
  p$pneumococcal_vx_coverage = 1
  p$hib_vx_coverage = 1
  p$llin_coverage = 1
  states <- create_states(p)
  variables <- create_variables(p)
  events <- create_events(individuals, variables)
  individuals <- create_individuals(states, variables, events)
  processes <- create_processes(p, individuals, variables, events)

  api <- mock_api(list(child = list(
    NULLSTATE = c(1),
    birth_t = -100,
    diarrhoea_status = 1,
    diarrhoea_disease_index = 1,
    diarrhoea_bacteria_prior = 3,
    diarrhoea_virus_prior = 2,
    diarrhoea_parasite_prior = 2,
    diarrhoea_rotavirus_prior = 4,
    het = 1,
    llin = 1,
    rotavirus_vx = 1,
    pneumococcal_vx = 1,
    hib_vx = 1)),
    parameters = p, timestep = 0)

  # Mock up so that the background mortality draw selects individual 1
  m <- mockery::mock(3)
  mockery::stub(replace_child, "heterogeneity", m)

  # Create the background mortality function
  replace_child(api, 1, individuals, variables, p)
  # Check that the arguments match the mocked API
  mockery::expect_args(api$queue_variable_update, 1, individuals$child, variables$birth_t, -p$age_lower, 1)
  mockery::expect_args(api$queue_variable_update, 2, individuals$child, variables$diarrhoea_status, 0, 1)
  mockery::expect_args(api$queue_variable_update, 3, individuals$child, variables$diarrhoea_disease_index, 0, 1)
  mockery::expect_args(api$queue_variable_update, 4, individuals$child, variables$diarrhoea_bacteria_prior, 0, 1)
  mockery::expect_args(api$queue_variable_update, 5, individuals$child, variables$diarrhoea_virus_prior, 0, 1)
  mockery::expect_args(api$queue_variable_update, 6, individuals$child, variables$diarrhoea_parasite_prior, 0, 1)
  mockery::expect_args(api$queue_variable_update, 7, individuals$child, variables$diarrhoea_rotavirus_prior, 0, 1)
  mockery::expect_args(api$queue_variable_update, 8, individuals$child, variables$het, 3, 1)
  mockery::expect_args(api$queue_variable_update, 9, individuals$child, variables$llin, 1, 1)
  mockery::expect_args(api$queue_variable_update, 10, individuals$child, variables$rotavirus_vx, 1, 1)
  mockery::expect_args(api$queue_variable_update, 11, individuals$child, variables$pneumococcal_vx, 1, 1)
  mockery::expect_args(api$queue_variable_update, 12, individuals$child, variables$hib_vx, 1, 1)
})

test_that("get age", {
  parameters <- get_parameters()
  parameters$population <- 2
  states <- create_states(parameters)
  variables <- create_variables(parameters)
  events <- create_events(individuals, variables)
  individuals <- create_individuals(states, variables, events)
  processes <- create_processes(parameters, individuals, variables, events)
  api <- mock_api(list(child = list(
    birth_t = c(-100, -123))),
    parameters = p, timestep = 0)
  expect_equal(get_age(api, individuals, variables), c(100, 123))
  expect_equal(get_age(api, individuals, variables, 1), 100)
  expect_equal(get_age(api, individuals, variables, 2), 123)
})


test_that("background mortality", {
  parameters <- get_parameters()
  parameters$population <- 2
  states <- create_states(parameters)
  variables <- create_variables(parameters)
  events <- create_events(individuals, variables)
  individuals <- create_individuals(states, variables, events)
  processes <- create_processes(parameters, individuals, variables, events)

  api <- mock_api(list(child = list(
    birth_t = c(-100, -1825))),
    parameters = parameters, timestep = 0)

  m <- mockery::mock(c(1, 0))
  mockery::stub(background_mortality, "stats::rbinom", m)
  bm <- background_mortality(parameters, individuals, variables)
  bm(api)

  mockery::expect_args(api$render, 1, "background_mortality", 1)
  mockery::expect_args(api$queue_variable_update, 1, individuals$child, variables$birth_t, -parameters$age_lower, 1)
})

test_that("graduation", {
  parameters <- get_parameters()
  parameters$population <- 2
  states <- create_states(parameters)
  variables <- create_variables(parameters)
  events <- create_events(individuals, variables)
  individuals <- create_individuals(states, variables, events)
  processes <- create_processes(parameters, individuals, variables, events)

  api <- mock_api(list(child = list(
    birth_t = c(-100, -1824))),
    parameters = parameters, timestep = 0)

  g <- graduate(parameters, individuals, variables)
  g(api)

  mockery::expect_args(api$render, 1, "graduation", 1)
  mockery::expect_args(api$queue_variable_update, 1, individuals$child, variables$birth_t, -parameters$age_lower, 2)
})
