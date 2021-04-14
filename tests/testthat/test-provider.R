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
  parameters$hf$efficacy <- 1
  variables <- create_variables(parameters)
  variables$malaria_fever <- mock_integer(c(0, 1, 0, 1, 0))
  events <- create_events(variables, parameters)
  renderer <- mock_render(timesteps)
  initialise_events(events, variables, parameters)
  create_event_listeners(events, variables, parameters, renderer)
  processes <- create_processes(parameters, variables, renderer, events)

  hf1 <- hf_treat(variables, parameters, renderer, events)

  # Five individuals to treat
  to_treat <- individual::Bitset$new(5)
  to_treat <- to_treat$insert(1:5)

  dia_sev <- individual::Bitset$new(5)
  dia_sev <- dia_sev$insert(1)
  mal_sev <- individual::Bitset$new(5)
  mal_sev <- mal_sev$insert(2)
  dia_non_sev <- individual::Bitset$new(5)
  dia_non_sev <- dia_non_sev$insert(3)
  mal_non_sev <- individual::Bitset$new(5)
  mal_non_sev <- mal_non_sev$insert(4)

  gstd <- mockery::mock()
  mockery::stub(hf1, "give_severe_treatment_diarrhoea", gstd)
  gstm <- mockery::mock()
  mockery::stub(hf1, "give_severe_treatment_malaria", gstm)
  go <- mockery::mock()
  mockery::stub(hf1, "give_ors", go)
  ga <- mockery::mock()
  mockery::stub(hf1, "give_act", ga)

  mockery::stub(hf1, "dx", mockery::mock(dia_sev, mal_sev, dia_non_sev, mal_non_sev))

  hf1(1, to_treat)

  mockery::expect_args(renderer$render, 1, "hf_patients", 5, 1)
  expect_equal(mockery::mock_args(gstd)[[1]][[1]]$to_vector(), dia_sev$to_vector())
  expect_equal(mockery::mock_args(gstm)[[1]][[1]]$to_vector(), mal_sev$to_vector())
  expect_equal(mockery::mock_args(go)[[1]][[1]]$to_vector(), dia_non_sev$to_vector())
  expect_equal(mockery::mock_args(ga)[[1]][[1]]$to_vector(), mal_non_sev$to_vector())
  mockery::expect_args(renderer$render, 2, "hf_severe_diarrhoea_tx", 1, 1)
  mockery::expect_args(renderer$render, 3, "hf_severe_malaria_tx", 1, 1)
  mockery::expect_args(renderer$render, 4, "hf_ors", 1, 1)
  mockery::expect_args(renderer$render, 5, "hf_act", 1, 1)
})

test_that("CHW works", {
  timesteps <- 10
  parameters <- get_parameters()
  parameters$population <- 5
  parameters$chw$efficacy <- 1
  variables <- create_variables(parameters)
  variables$malaria_fever <- mock_integer(c(0, 1, 0, 1, 0))
  variables$awaiting_followup <- mock_integer(c(0, 1, 0, 1, 0))
  events <- create_events(variables, parameters)
  renderer <- mock_render(timesteps)
  initialise_events(events, variables, parameters)
  create_event_listeners(events, variables, parameters, renderer)
  processes <- create_processes(parameters, variables, renderer, events)

  chw1 <- chw_treat(variables, parameters, renderer, events)

  # Five individuals to treat
  to_treat <- individual::Bitset$new(5)
  to_treat <- to_treat$insert(1:5)

  dia_sev <- individual::Bitset$new(5)
  dia_sev <- dia_sev$insert(1)
  mal_sev <- individual::Bitset$new(5)
  mal_sev <- mal_sev$insert(2)
  dia_non_sev <- individual::Bitset$new(5)
  dia_non_sev <- dia_non_sev$insert(3)
  mal_non_sev <- individual::Bitset$new(5)
  mal_non_sev <- mal_non_sev$insert(4)

  go <- mockery::mock()
  mockery::stub(chw1, "give_ors", go)
  ga <- mockery::mock()
  mockery::stub(chw1, "give_act", ga)

  mockery::stub(chw1, "dx", mockery::mock(dia_sev, mal_sev, dia_non_sev, mal_non_sev))

  chw1(1, to_treat)

  mockery::expect_args(renderer$render, 1, "chw_patients", 5, 1)
  expect_equal(mockery::mock_args(variables$awaiting_followup$queue_update)[[1]][[2]]$to_vector(), c(1, 3, 5))
  expect_equal(mockery::mock_args(variables$awaiting_followup$queue_update)[[2]][[2]]$to_vector(), c(2, 4))
  expect_equal(mockery::mock_args(go)[[1]][[1]]$to_vector(), dia_sev$to_vector())
  expect_equal(mockery::mock_args(ga)[[1]][[1]]$to_vector(), mal_sev$to_vector())
  mockery::expect_args(renderer$render, 2, "chw_referral", 2, 1)
  expect_equal(mockery::mock_args(go)[[2]][[1]]$to_vector(), dia_non_sev$to_vector())
  expect_equal(mockery::mock_args(ga)[[2]][[1]]$to_vector(), mal_non_sev$to_vector())
  mockery::expect_args(renderer$render, 3, "chw_ors", 2, 1)
  mockery::expect_args(renderer$render, 4, "chw_act", 2, 1)
  mockery::expect_args(renderer$render, 5, "chw_followup", 2, 1)
})

test_that("Private works", {
  timesteps <- 10
  parameters <- get_parameters()
  parameters$population <- 5
  parameters$private$efficacy <- 1
  variables <- create_variables(parameters)
  variables$malaria_fever <- mock_integer(c(0, 1, 0, 1, 0))
  events <- create_events(variables, parameters)
  renderer <- mock_render(timesteps)
  initialise_events(events, variables, parameters)
  create_event_listeners(events, variables, parameters, renderer)
  processes <- create_processes(parameters, variables, renderer, events)

  private1 <- private_treat(variables, parameters, renderer, events)

  # Five individuals to treat
  to_treat <- individual::Bitset$new(5)
  to_treat <- to_treat$insert(1:5)

  dia_sev <- individual::Bitset$new(5)
  dia_sev <- dia_sev$insert(1)
  mal_sev <- individual::Bitset$new(5)
  mal_sev <- mal_sev$insert(2)
  dia_non_sev <- individual::Bitset$new(5)
  dia_non_sev <- dia_non_sev$insert(3)
  mal_non_sev <- individual::Bitset$new(5)
  mal_non_sev <- mal_non_sev$insert(4)

  go <- mockery::mock()
  mockery::stub(private1, "give_ors", go)
  ga <- mockery::mock()
  mockery::stub(private1, "give_act", ga)

  mockery::stub(private1, "dx", mockery::mock(dia_non_sev, mal_non_sev))

  private1(1, to_treat)

  mockery::expect_args(renderer$render, 1, "private_patients", 5, 1)
  expect_equal(mockery::mock_args(go)[[1]][[1]]$to_vector(), dia_non_sev$to_vector())
  expect_equal(mockery::mock_args(ga)[[1]][[1]]$to_vector(), mal_non_sev$to_vector())
  mockery::expect_args(renderer$render, 2, "private_ors", 1, 1)
  mockery::expect_args(renderer$render, 3, "private_act", 1, 1)
})
