test_that("sample preference works", {
  parameters <- get_parameters()

  # Provider availability
  parameters$hf$hf = 0
  parameters$chw$chw = 0
  parameters$private$private = 0
  expect_equal(sample_preference(100, parameters), rep("none", 100))

  parameters$hf$hf = 1
  parameters$chw$chw = 0
  parameters$private$private = 0
  expect_equal(sample_preference(100, parameters), rep("hf", 100))

  parameters$hf$hf = 0
  parameters$chw$chw = 1
  parameters$private$private = 0
  expect_equal(sample_preference(100, parameters), rep("chw", 100))

  parameters$hf$hf = 0
  parameters$chw$chw = 0
  parameters$private$private = 1
  expect_equal(sample_preference(100, parameters), rep("private", 100))


  # Preference weighting
  parameters$hf$hf = 1
  parameters$chw$chw = 1
  parameters$private$private = 1
  parameters$treatment_seeking$provider_preference_weights = c(1, 0, 0)
  expect_equal(sample_preference(100, parameters), rep("hf", 100))

  parameters$treatment_seeking$provider_preference_weights = c(0, 1, 0)
  expect_equal(sample_preference(100, parameters), rep("chw", 100))

  parameters$treatment_seeking$provider_preference_weights = c(0, 0, 1)
  expect_equal(sample_preference(100, parameters), rep("private", 100))
})

