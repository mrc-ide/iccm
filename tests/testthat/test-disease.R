parameters <- get_parameters()
parameters$population <- 1
states <- create_states(parameters)
variables <- create_variables(parameters)
events <- create_events(individuals, variables)
individuals <- create_individuals(states, variables, events)
processes <- create_processes(parameters, individuals, variables, events)

test_that("infection life course works", {
  api <- mock_api(list(child = list(
    NULLSTATE = c(1),
    diarrhoea_status = 0,
    diarrhoea_disease_index = 0,
    diarrhoea_bacteria_prior = 3)),
    parameters = parameters, timestep = 0)

  # Stub duration of infection draw
  mockery::stub(infection_life_course, "stats::rpois", 10)

  infection_life_course("diarrhoea", 1, p = parameters$diarrhoea, target = 1, api = api,
                        individuals = individuals, variables = variables, events = events)

  mockery::expect_args(api$queue_variable_update, 1, individuals$child, variables$diarrhoea_disease_index, 1, 1)
  mockery::expect_args(api$queue_variable_update, 2, individuals$child, variables$diarrhoea_bacteria_prior, 4, 1)
  mockery::expect_args(api$queue_variable_update, 3, individuals$child, variables$diarrhoea_status, 1, 1)
  mockery::expect_args(api$schedule, 1, events$diarrhoea_recover, 1, 10)
})

test_that("disease rendering works", {
  api <- mock_api(list(child = list(
    NULLSTATE = c(1),
    diarrhoea_status = 1,
    diarrhoea_disease_index = 1,
    diarrhoea_bacteria_prior = 3)),
    parameters = parameters, timestep = 0)

  prevalence_renderer <- render_prevalence("diarrhoea", individuals, variables, parameters)
  prevalence_renderer(api)

  mockery::expect_args(api$render, 1, 'diarrhoea_prevalence', 1)
  mockery::expect_args(api$render, 2, 'diarrhoea_bacteria_prevalence', 1)
  mockery::expect_args(api$render, 3, 'diarrhoea_virus_prevalence', 0)
  mockery::expect_args(api$render, 4, 'diarrhoea_parasite_prevalence', 0)
  mockery::expect_args(api$render, 5, 'diarrhoea_rotavirus_prevalence', 0)
})

test_that("recover event works", {
  api <- mock_api(list(child = list(
    NULLSTATE = c(1),
    diarrhoea_status = 1,
    diarrhoea_disease_index = 1,
    diarrhoea_bacteria_prior = 3)),
    parameters = parameters, timestep = 0)

  recovery_event <- recover_event("diarrhoea", individuals, variables)
  recovery_event$listeners[[1]](api, 1)

  mockery::expect_args(api$queue_variable_update, 1, individuals$child, variables$diarrhoea_status, 0, 1)
  mockery::expect_args(api$queue_variable_update, 2, individuals$child, variables$diarrhoea_disease_index, 0, 1)
})

test_that("condition exposure works", {
  api <- mock_api(list(child = list(
    NULLSTATE = c(1),
    birth_t = -365,
    diarrhoea_status = 0,
    het = 1,
    diarrhoea_disease_index = 0,
    diarrhoea_bacteria_prior = 3,
    diarrhoea_virus_prior = 1,
    diarrhoea_parasite_prior = 0,
    diarrhoea_rotavirus_prior = 1,
    rotavirus_vx = 1)),
    parameters = p, timestep = 0)


  ci <- condition_exposure("diarrhoea", individuals, variables, parameters, events)

  m1 <- mockery::mock(365)
  mockery::stub(ci, "get_age", m1)

  m2 <- mockery::mock(1, cycle = TRUE)
  mockery::stub(ci, "maternal_immunity", m2)

  m3 <- mockery::mock(-1)
  mockery::stub(ci, "stats::runif", m3)

  m4 <- mockery::mock(0)
  mockery::stub(ci, "infection_life_course", m4)

  ci(api)

  mockery::expect_called(m1, 1)
  mockery::expect_called(m2, parameters$diarrhoea$groups)
  mockery::expect_called(m4, 1)
})
