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

test_that("replace child", {
  timestep <- 151
  p <- get_parameters()
  p$population <- 3
  # Set coverages of interventions to 1 so we can check the new child is assigned them all
  p$rotavirus_vx_coverage = 1
  p$pneumococcal_vx_coverage = 1
  p$hib_vx_coverage = 1
  p$llin_coverage = 1

  # Mock up events
  events <- list()
  events$graduate <- mock_event()

  # Mock up variables
  variables <- list()
  variables$birth_t <- mock_integer(rep(100, p$population))
  for(disease in names(p$disease)){
    variables[[paste0(disease, "_prior_exposure")]] <- mock_integer(rep(1, p$population))
    variables[[paste0(disease, "_status")]] <- mock_category(c("uninfected", "asymptomatic", "symptomatic", "severe"), rep("symptomatic", p$population))
    variables[[paste0(disease, "_fever")]] <- mock_category(c("nonfebrile", "febrile"), rep("febrile", p$population))
    variables[[paste0(disease, "_symptom_onset")]] <- mock_integer(rep(150, p$population))
  }
  variables$het <- mock_double(rep(1.5, p$population))
  variables$time_of_last_act <- mock_integer(rep(150, p$population))
  variables$time_of_last_amoxicillin <- mock_integer(rep(150, p$population))
  variables$awaiting_followup <- mock_integer(rep(1, p$population))
  variables$llin <- mock_integer(rep(0, p$population))
  variables$rotavirus_vx <- mock_integer(rep(0, p$population))
  variables$pneumococcal_vx <- mock_integer(rep(0, p$population))
  variables$hib_vx <- mock_integer(rep(0, p$population))

  # Stub out the random het draw
  mockery::stub(replace_child, "heterogeneity", c(0.5, 1.5))

  # Replace the first 2 children
  to_replace <- individual::Bitset$new(p$population)
  to_replace <- to_replace$insert(1:2)
  replace_child(to_replace, timestep, variables, p, events)

  # Checks - variable updates
  expect_bitset_update(variables$birth_t$queue_update, timestep - p$age_lower, 1:2)
  for(disease in names(p$disease)){
    expect_bitset_update(variables[[paste0(disease, "_prior_exposure")]]$queue_update, 0, 1:2)
    expect_bitset_update(variables[[paste0(disease, "_status")]]$queue_update, "uninfected", 1:2)
    expect_bitset_update(variables[[paste0(disease, "_fever")]]$queue_update, "nonfebrile", 1:2)
    expect_bitset_update(variables[[paste0(disease, "_symptom_onset")]]$queue_update, NA, 1:2)
  }
  expect_bitset_update(variables$het$queue_update, c(0.5, 1.5), 1:2)
  expect_bitset_update(variables$llin$queue_update, c(1,1), 1:2)
  expect_bitset_update(variables$time_of_last_act$queue_update, NA, 1:2)
  expect_bitset_update(variables$time_of_last_amoxicillin$queue_update, NA, 1:2)
  expect_bitset_update(variables$awaiting_followup$queue_update, 0, 1:2)
  expect_bitset_update(variables$rotavirus_vx$queue_update, c(1, 1), 1:2)
  expect_bitset_update(variables$hib_vx$queue_update, c(1, 1), 1:2)
  expect_bitset_update(variables$pneumococcal_vx$queue_update, c(1, 1), 1:2)
  # Checks - events
  mockery::expect_args(events$graduate$schedule, n = 1, to_replace, c(p$age_upper - p$age_lower, p$age_upper - p$age_lower))
  mockery::expect_called(events$graduate$clear_schedule, n = 1)
})

test_that("get age", {
  variables <- list()
  variables$birth_t <- mock_integer(c(1, 100))

  expect_equal(get_age(0, variables), -variables$birth_t$get_values())
  expect_equal(get_age(100, variables), -variables$birth_t$get_values() + 100)
  expect_equal(get_age(0, variables, 1), -variables$birth_t$get_values()[1])
  expect_equal(get_age(0, variables, 2), -variables$birth_t$get_values()[2])
})

