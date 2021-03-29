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
  parameters <- get_parameters(list(population = 4))
  variables <- create_variables(parameters)
  events <- create_events(variables, parameters)
  renderer <- individual::Render$new(timesteps)
  initialise_events(events, variables, parameters)
  create_event_listeners(events, variables, parameters, renderer)
  processes <- create_processes(parameters, variables, renderer, events)

  hf1 <- hf_treat(variables, parameters, renderer, events)




})
