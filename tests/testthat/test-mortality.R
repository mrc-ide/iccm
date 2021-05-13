test_that("mortality from disease", {
  parameters <- get_parameters()
  parameters$population <- 2
  renderer <- mock_render(1)

  # Set all individuals to severe for all diseases
  variables <- list()
  for(disease in names(parameters$disease)){
    variables[[paste0(disease, "_status")]] <- mock_category(c("uninfected", "asymptomatic", "symptomatic", "severe"), rep("severe", parameters$population))
  }
  events <- list()

  # No natural mortality:
  parameters$average_age <- Inf
  # Set probability of death = 1 for only 1 disease
  for(disease in names(parameters$disease)){
    parameters$disease[[disease]]$daily_probability_death <- 0
  }
  parameters$disease[[1]]$daily_probability_death <- 1

  mf <- mortality(parameters, variables, events, renderer)
  replace_child_mock <- mockery::mock()
  mockery::stub(mf, "replace_child", replace_child_mock)
  mf(1)

  # Check - rendering
  mockery::expect_args(renderer$render, 1, paste0(names(parameters$disease)[1], "_death"), 2, 1)
  mockery::expect_args(renderer$render, 2, "other_death", 0, 1)

  # Check - replace_child
  expect_equal(mockery::mock_args(replace_child_mock)[[1]][[1]]$to_vector(), 1:2)
  expect_equal(mockery::mock_args(replace_child_mock)[[1]][[2]], 1)
  expect_equal(mockery::mock_args(replace_child_mock)[[1]][[3]], variables)
  expect_equal(mockery::mock_args(replace_child_mock)[[1]][[4]], parameters)
  expect_equal(mockery::mock_args(replace_child_mock)[[1]][[5]], events)
})

test_that("mortality from other cause", {
  parameters <- get_parameters()
  parameters$population <- 2
  renderer <- mock_render(1)

  # Set all individuals to severe for all diseases
  variables <- list()
  for(disease in names(parameters$disease)){
    variables[[paste0(disease, "_status")]] <- mock_category(c("uninfected", "asymptomatic", "symptomatic", "severe"), rep("severe", parameters$population))
  }
  events <- list()

  # No natural mortality:
  parameters$average_age <- 0
  # Set probability of death = 0 for all diseases
  for(disease in names(parameters$disease)){
    parameters$disease[[disease]]$daily_probability_death <- 0
  }

  mf <- mortality(parameters, variables, events, renderer)
  replace_child_mock <- mockery::mock()
  mockery::stub(mf, "replace_child", replace_child_mock)
  mf(1)

  # Check - rendering
  mockery::expect_args(renderer$render, 1, "other_death", 2, 1)

  # Check - replace_child
  expect_equal(mockery::mock_args(replace_child_mock)[[1]][[1]]$to_vector(), 1:2)
  expect_equal(mockery::mock_args(replace_child_mock)[[1]][[2]], 1)
  expect_equal(mockery::mock_args(replace_child_mock)[[1]][[3]], variables)
  expect_equal(mockery::mock_args(replace_child_mock)[[1]][[4]], parameters)
  expect_equal(mockery::mock_args(replace_child_mock)[[1]][[5]], events)
})
