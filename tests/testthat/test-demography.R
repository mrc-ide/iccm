test_that("demography rendering", {
  renderer <- mock_render(1)
  # Set up a mock API with 2 children of ages 0 and the age limit
  variables <- list(birth_t = mock_double(c(-1, -365*4.5)))
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

test_that("clearing all events works", {
  target <- individual::Bitset$new(2)$insert(1:2)
  events <- list(
    mock_event(),
    mock_event()
  )
  clear_all_scheduled_events(target, events)
  expect_equal(mockery::mock_args(events[[1]]$clear_schedule)[[1]][[1]]$to_vector(), c(1, 2))
  expect_equal(mockery::mock_args(events[[2]]$clear_schedule)[[1]][[1]]$to_vector(), c(1, 2))
})

test_that("Resetting disease variables works", {
  target = individual::Bitset$new(2)$insert(1:2)
  parameters <- list(
    disease = list(
      list(vaccine_coverage = 1)
    )
  )
  variables = list(
    prior_exposure = list(
      mock_integer(c(1, 2))
    ),
    infection_status = list(
      mock_category(c("uninfected", "asymptomatic", "symptomatic", "severe"), c("symptomatic", "severe"))
    ),
    fever = list(
      mock_category(c("nonfebrile", "febrile"), c("nonfebrile", "febrile"))
    ),
    symptom_onset = list(
      mock_integer(c(100, 101))
    ),
    vaccine = list(
      mock_integer(c(0, 0))
    )
  )
  reset_disease_status(target, parameters, variables)
  expect_bitset_update(variables$prior_exposure[[1]]$queue_update, 0, c(1, 2))
  expect_bitset_update(variables$infection_status[[1]]$queue_update, "uninfected", c(1, 2))
  expect_bitset_update(variables$fever[[1]]$queue_update, "nonfebrile", c(1, 2))
  expect_bitset_update(variables$symptom_onset[[1]]$queue_update, as.numeric(NA), c(1, 2))
  expect_bitset_update(variables$vaccine[[1]]$queue_update, c(1, 1), c(1, 2))
})

test_that("Resetting treatment and interventions works", {
  target = individual::Bitset$new(2)$insert(1:2)
  parameters <- list(
    llin_coverage = 1
  )
  variables = list(
    time_of_last_act = mock_integer(c(1, 2)),
    time_of_last_amoxicillin = mock_integer(c(1, 2)),
    awaiting_followup = mock_integer(c(1, 1)),
    llin = mock_integer(c(0, 0))
  )
  reset_treatment_and_interventions(target, parameters, variables)
  expect_bitset_update(variables$time_of_last_act$queue_update, as.numeric(NA), c(1, 2))
  expect_bitset_update(variables$time_of_last_amoxicillin$queue_update, as.numeric(NA), c(1, 2))
  expect_bitset_update(variables$awaiting_followup$queue_update, 0, c(1, 2))
  expect_bitset_update(variables$llin$queue_update, c(1, 1), c(1, 2))
})

test_that("Resetting heterogeneity works", {
  target = individual::Bitset$new(2)$insert(1:2)
  parameters <- list(
    het_sd = 0
  )
  variables = list(
    heterogeneity = mock_double(c(1, 2))
  )
  reset_heterogeneity(target, parameters, variables)
  expect_bitset_update(variables$heterogeneity$queue_update, c(1, 1), c(1, 2))
})

test_that("Resetting demography works", {
  target = individual::Bitset$new(2)$insert(1:2)
  parameters <- list(
    age_lower = 10,
    age_upper = 11
  )
  variables = list(
    birth_t = mock_integer(c(1, 2))
  )
  events = list(
    graduate = mock_event()
  )
  timestep = 100
  reset_demography(target, parameters, variables, events, timestep)
  expect_bitset_update(variables$birth_t$queue_update, timestep - parameters$age_lower, c(1, 2))
  expect_equal(mockery::mock_args(events$graduate$schedule)[[1]][[1]]$to_vector(), c(1, 2))
  expect_equal(mockery::mock_args(events$graduate$schedule)[[1]][[2]], c(1, 1))
})

test_that("get age", {
  variables <- list()
  variables$birth_t <- mock_integer(c(1, 100))

  expect_equal(get_age(0, variables$birth_t), -variables$birth_t$get_values())
  expect_equal(get_age(100, variables$birth_t), -variables$birth_t$get_values() + 100)
  expect_equal(get_age(0, variables$birth_t, 1), -variables$birth_t$get_values()[1])
  expect_equal(get_age(0, variables$birth_t, 2), -variables$birth_t$get_values()[2])
})

