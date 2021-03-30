test_that("sample preference works", {
  parameters <- get_parameters()

  # Provider availability
  parameters$hf$hf = 0
  parameters$chw$chw = 0
  parameters$private$private = 0
  expect_equal(sample_preference(100, parameters), rep("None", 100))

  parameters$hf$hf = 1
  parameters$chw$chw = 0
  parameters$private$private = 0
  expect_equal(sample_preference(100, parameters), rep("HF", 100))

  parameters$hf$hf = 0
  parameters$chw$chw = 1
  parameters$private$private = 0
  expect_equal(sample_preference(100, parameters), rep("CHW", 100))

  parameters$hf$hf = 0
  parameters$chw$chw = 0
  parameters$private$private = 1
  expect_equal(sample_preference(100, parameters), rep("Private", 100))


  # Preference weighting
  parameters$hf$hf = 1
  parameters$chw$chw = 1
  parameters$private$private = 1
  parameters$treatment_seeking$provider_preference_weights = c(1, 0, 0)
  expect_equal(sample_preference(100, parameters), rep("HF", 100))

  parameters$treatment_seeking$provider_preference_weights = c(0, 1, 0)
  expect_equal(sample_preference(100, parameters), rep("CHW", 100))

  parameters$treatment_seeking$provider_preference_weights = c(0, 0, 1)
  expect_equal(sample_preference(100, parameters), rep("Private", 100))
})

test_that("Health facility works", {
  timesteps <- 10
  parameters <- get_parameters()
  parameters$population <- 5
  variables <- create_variables(parameters)
  events <- create_events(variables, parameters)
  renderer <- mock_render(timesteps)
  initialise_events(events, variables, parameters)
  create_event_listeners(events, variables, parameters, renderer)
  processes <- create_processes(parameters, variables, renderer, events)

  hf1 <- hf_treat(variables, parameters, renderer, events)

  # Five individuals to treat
  to_treat <- individual::Bitset$new(5)
  to_treat <- to_treat$insert(1:5)

  mockery::stub(hf1, "dx", mockery::mock(c(TRUE, FALSE, FALSE, FALSE, FALSE),
                             c(TRUE, FALSE, FALSE, FALSE)))
  mockery::stub(hf1, "px", mockery::mock(c(TRUE, FALSE, FALSE, FALSE, FALSE),
                                         c(TRUE, FALSE, FALSE, FALSE)))
  hf1(1, to_treat)

  mockery::expect_args(renderer$render, 1, "hf_patients", 5, 1)
  mockery::expect_args(renderer$render, 2, "hf_severe_diarrhoea_tx", 1, 1)
  mockery::expect_args(renderer$render, 3, "hf_ors", 1, 1)
})

test_that("CHW works", {
  timesteps <- 10
  parameters <- get_parameters()
  parameters$population <- 5
  variables <- create_variables(parameters)
  events <- create_events(variables, parameters)
  renderer <- mock_render(timesteps)
  initialise_events(events, variables, parameters)
  create_event_listeners(events, variables, parameters, renderer)
  processes <- create_processes(parameters, variables, renderer, events)

  hf1 <- chw_treat(variables, parameters, renderer, events)

  # Five individuals to treat
  to_treat <- individual::Bitset$new(5)
  to_treat <- to_treat$insert(1:5)

  mockery::stub(hf1, "dx", mockery::mock(c(TRUE, FALSE, FALSE, FALSE, FALSE),
                                         c(TRUE, FALSE, FALSE, FALSE)))
  mockery::stub(hf1, "px", mockery::mock(c(TRUE, FALSE, FALSE, FALSE, FALSE),
                                         c(TRUE, FALSE, FALSE, FALSE)))
  hf1(1, to_treat)

  mockery::expect_args(renderer$render, 1, "chw_patients", 5, 1)
  mockery::expect_args(renderer$render, 2, "chw_referral", 1, 1)
  mockery::expect_args(renderer$render, 3, "chw_ors", 2, 1)
  mockery::expect_args(renderer$render, 4, "chw_followup", 0, 1)
})

test_that("Private works", {
  timesteps <- 10
  parameters <- get_parameters()
  parameters$population <- 5
  variables <- create_variables(parameters)
  events <- create_events(variables, parameters)
  renderer <- mock_render(timesteps)
  initialise_events(events, variables, parameters)
  create_event_listeners(events, variables, parameters, renderer)
  processes <- create_processes(parameters, variables, renderer, events)

  hf1 <- private_treat(variables, parameters, renderer, events)

  # Five individuals to treat
  to_treat <- individual::Bitset$new(5)
  to_treat <- to_treat$insert(1:5)

  mockery::stub(hf1, "dx", mockery::mock(c(TRUE, FALSE, FALSE, FALSE, FALSE)))
  mockery::stub(hf1, "px", mockery::mock(c(TRUE, FALSE, FALSE, FALSE, FALSE)))
  hf1(1, to_treat)

  mockery::expect_args(renderer$render, 1, "private_patients", 5, 1)
  mockery::expect_args(renderer$render, 2, "private_ors", 1, 1)
})
