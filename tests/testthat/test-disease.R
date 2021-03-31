timesteps <- 10
parameters = get_parameters()
parameters$population <- 4
variables <- create_variables(parameters)
events <- create_events(variables, parameters)
create_event_listeners(events, variables)
renderer <- individual::Render$new(timesteps)
processes <- create_processes(parameters, variables, renderer, events)
pop_bitset <- individual::Bitset$new(4)$insert(1:4)

test_that("prevalence rendering works", {
  renderer <- mock_render(2)
  variables$dia_status <- mock_integer( c(2, 2, 2, 1))
  variables$dia_disease <- mock_integer(c(2, 2, 3, 0))
  variables$dia_prior_bacteria <- mock_integer(rep(0,4))
  variables$dia_prior_virus <- mock_integer(rep(0,4))
  variables$dia_prior_parasite <- mock_integer(rep(0,4))
  variables$dia_prior_rotavirus <- mock_integer(rep(0,4))

  prevalence_renderer <- render_prevalence("dia", variables, parameters, renderer)
  prevalence_renderer(timestep  = 1)

  mockery::expect_args(renderer$render, 1, "dia_prevalence", 0.75, 1)
  mockery::expect_args(renderer$render, 2, "dia_bacteria_prevalence", 0, 1)
  mockery::expect_args(renderer$render, 3, "dia_virus_prevalence", 0.5, 1)
  mockery::expect_args(renderer$render, 4, "dia_parasite_prevalence", 0.25, 1)
  mockery::expect_args(renderer$render, 5, "dia_rotavirus_prevalence", 0, 1)
})

test_that("recover event works", {
  variables$dia_status <- mock_integer(c(2, 2, 2, 1))
  variables$dia_disease <- mock_integer(c(2, 2, 3, 0))
  recovery_event <- recover_event(variables, "dia")
  recovery_event(1, 1)

  mockery::expect_args(variables$dia_status$queue_update, 1, 0, 1)
  mockery::expect_args(variables$dia_disease$queue_update, 1, 0, 1)
})

test_that("time since onset estimated correctly", {
  symptom_start <- c(10, 20, 30, 40)
  variables$dia_symptom_start <- mock_integer(symptom_start)

  expect_equal(time_since_onset(target = pop_bitset, condition = "dia", variables = variables, timestep = 100), 100 - symptom_start)
})

test_that("Prior exposures incremented correctly", {
  variables$dia_prior_bacteria <- mock_integer(rep(0, 4))
  variables$dia_prior_virus <- mock_integer(rep(1, 4))
  variables$dia_prior_parasite <- mock_integer(rep(2, 4))
  variables$dia_prior_rotavirus <- mock_integer(rep(3, 4))

  condition_prior_disease <- paste0("dia_prior_", parameters$dia$disease)
  new_infections <- c(1, 2, 3, 4)

  increment_prior_exposure_counter(new_infections = new_infections, target = pop_bitset,
                                   condition_prior_disease = condition_prior_disease,
                                   variables = variables)

  # First person, first disease 0->1
  expect_equal(mockery::mock_args(variables$dia_prior_bacteria$queue_update)[[1]][[1]], 1)
  expect_equal(mockery::mock_args(variables$dia_prior_bacteria$queue_update)[[1]][[2]]$to_vector(), 1)
  # Second person, second disease 1->2
  expect_equal(mockery::mock_args(variables$dia_prior_virus$queue_update)[[1]][[1]], 2)
  expect_equal(mockery::mock_args(variables$dia_prior_virus$queue_update)[[1]][[2]]$to_vector(), 2)
  # Third person, thirs disease 2->3
  expect_equal(mockery::mock_args(variables$dia_prior_parasite$queue_update)[[1]][[1]], 3)
  expect_equal(mockery::mock_args(variables$dia_prior_parasite$queue_update)[[1]][[2]]$to_vector(), 3)
  # Fourth person, fourth disease 3->4
  expect_equal(mockery::mock_args(variables$dia_prior_rotavirus$queue_update)[[1]][[1]], 4)
  expect_equal(mockery::mock_args(variables$dia_prior_rotavirus$queue_update)[[1]][[2]]$to_vector(), 4)
})

test_that("Death due to disease works correctly - 100% probability of dieing", {
  # Two individuals severe (3)
  variables$dia_status <- mock_integer(c(0, 3, 2, 3))
  variables$dia_disease <- mock_integer(c(0, 1, 1, 1))


  # 100% chance of death
  renderer <- mock_render(timesteps)
  parameters$dia$daily_prob_death <- rep(1, 4)
  df <- die(condition = "dia", parameters = parameters, variables = variables, events = events, renderer = renderer)
  rc <- mockery::mock()
  mockery::stub(df, "replace_child", rc)
  df(1)

  mockery::expect_args(renderer$render, 1, "dia_bacteria_mortality", 2, 1)
  mockery::expect_args(renderer$render, 2, "dia_virus_mortality", 0, 1)
  mockery::expect_args(renderer$render, 3, "dia_parasite_mortality", 0, 1)
  mockery::expect_args(renderer$render, 4, "dia_rotavirus_mortality", 0, 1)

  expect_equal(mockery::mock_args(rc)[[1]][[1]]$to_vector(), c(2, 4))
})

test_that("Death due to disease works correctly - 0% probability of dieing", {
  # Two individuals severe (3)
  variables$dia_status <- mock_integer(c(0, 3, 2, 3))
  variables$dia_disease <- mock_integer(c(0, 1, 1, 1))

  # 0% chance of death
  renderer <- mock_render(timesteps)
  parameters$dia$daily_prob_death <- rep(0, 4)
  df <- die(condition = "dia", parameters = parameters, variables = variables, events = events, renderer = renderer)
  rc <- mockery::mock()
  mockery::stub(df, "replace_child", rc)
  df(1)

  mockery::expect_called(renderer$render, 0)
  mockery::expect_called(rc, 0)
})

test_that("Progression to severe disease works correctly - 100% probability of progressing", {
  # Two individuals clinical (2)
  variables$dia_status <- mock_integer(c(0, 2, 3, 2))
  variables$dia_disease <- mock_integer(c(0, 1, 1, 1))
  variables$dia_fever <- mock_integer(c(0, 0, 0, 0))
  variables$provider_preference <- mock_category(c("None", "HF", "CHW", "Private"), c("HF", "HF", "CHW", "CHW"))

  events$chw_treatment <- mock_event()
  events$hf_treatment <- mock_event()
  events$private_treatment <- mock_event()

  renderer <- mock_render(timesteps)
  parameters$dia$daily_prob_severe <- rep(1, 4)

  pf <- progress_severe(condition = "dia", parameters = parameters, variables = variables, events = events)
  pf(1)

  # Check status and fever updates
  expect_equal(mockery::mock_args(variables$dia_status$queue_update)[[1]][[1]], 3)
  expect_equal(mockery::mock_args(variables$dia_status$queue_update)[[1]][[2]]$to_vector(), c(2, 4))
  expect_equal(mockery::mock_args(variables$dia_fever$queue_update)[[1]][[1]], 1)
  expect_equal(mockery::mock_args(variables$dia_fever$queue_update)[[1]][[2]]$to_vector(), c(2, 4))

  # Individual 2 gets HF treatment scheduled
  expect_equal(mockery::mock_args(events$hf_treatment$schedule)[[1]][[1]]$to_vector(), 2)
  # Individual 4 gets CHW treatment scheduled
  expect_equal(mockery::mock_args(events$chw_treatment$schedule)[[1]][[1]]$to_vector(), 4)
  # None get private treatment scheduled
  expect_equal(length(mockery::mock_args(events$private_treatment$schedule)[[1]][[1]]$to_vector()), 0)
})

test_that("Progression to severe disease works correctly - 0% probability of progressing", {
  # Two individuals clinical (2)
  variables$dia_status <- mock_integer(c(0, 2, 3, 2))
  variables$dia_disease <- mock_integer(c(0, 1, 1, 1))
  variables$dia_fever <- mock_integer(c(0, 0, 0, 0))
  variables$provider_preference <- mock_category(c("None", "HF", "CHW", "Private"), c("HF", "HF", "CHW", "CHW"))

  events$chw_treatment <- mock_event()
  events$hf_treatment <- mock_event()
  events$private_treatment <- mock_event()

  renderer <- mock_render(timesteps)
  parameters$dia$daily_prob_severe <- rep(0, 4)

  pf <- progress_severe(condition = "dia", parameters = parameters, variables = variables, events = events)
  pf(1)

  # Check status and fever updates
  mockery::expect_called(variables$dia_status$queue_update, 0)
  mockery::expect_called(variables$dia_fever$queue_update, 0)
  mockery::expect_called(events$hf_treatment$schedule, 0)
  mockery::expect_called(events$chw_treatment$schedule, 0)
  mockery::expect_called(events$private_treatment$schedule, 0)
})

test_that("Condition exposure works correctly", {
  variables$dia_status <- mock_integer(c(0, 0, 0, 2))
  variables$dia_disease <- mock_integer(c(0, 0, 0, 1))
  variables$dia_symptom_start <- mock_integer(rep(NA, 4))
  variables$dia_fever <- mock_integer(c(0, 0, 0, 0))
  events$dia_recover <- mock_event()
  variables$provider_preference <- mock_category(c("None", "HF", "CHW", "Private"), c("HF", "HF", "CHW", "CHW"))
  events$chw_treatment <- mock_event()
  events$hf_treatment <- mock_event()
  events$private_treatment <- mock_event()

  # All get fever
  parameters$dia$prob_fever <- rep(1, 4)
  # All seek treatment
  parameters$treatment_seeking$prob_seek_treatment <- 1

  cf <- condition_exposure(condition = "dia", variables = variables, parameters = parameters, events = events, renderer = renderer)
  # Stub so that individuals 1 and 3 are infected
  mockery::stub(cf, "stats::runif", mockery::mock(c(0, 1, 0), c(0, 0)))
  # With disease index 1 and 2
  mockery::stub(cf, "sample_disease", mockery::mock(1, 2))
  # Clinical durations, asymptomatic durations time to treatment seeking for HF, CHW, private
  mockery::stub(cf, "stats::rpois", mockery::mock(c(5, 10), c(0, 0), 1, 3, 0, 0))
  ipec <- mockery::mock()
  mockery::stub(cf, "increment_prior_exposure_counter", ipec)
  ri <- mockery::mock()
  mockery::stub(cf, "render_incidence", ri)

  cf(1)

  # Work through all of the actions call in cf
  expect_equal(mockery::mock_args(variables$dia_disease$queue_update)[[1]][[1]], c(1, 2))
  expect_equal(mockery::mock_args(variables$dia_disease$queue_update)[[1]][[2]]$to_vector(), c(1, 3))
  mockery::expect_called(ipec, 1)
  expect_equal(mockery::mock_args(variables$dia_status$queue_update)[[1]][[1]], 2)
  expect_equal(mockery::mock_args(variables$dia_status$queue_update)[[1]][[2]]$to_vector(), c(1, 3))
  expect_equal(mockery::mock_args(variables$dia_symptom_start$queue_update)[[1]][[1]], 1)
  expect_equal(mockery::mock_args(variables$dia_symptom_start$queue_update)[[1]][[2]]$to_vector(), c(1, 3))
  expect_equal(mockery::mock_args(variables$dia_fever$queue_update)[[1]][[1]], 1)
  expect_equal(mockery::mock_args(variables$dia_fever$queue_update)[[1]][[2]]$to_vector(), c(1, 3))
  expect_equal(mockery::mock_args(events$dia_recover$schedule)[[1]][[1]]$to_vector(), c(1, 3))
  expect_equal(mockery::mock_args(events$dia_recover$schedule)[[1]][[2]], c(5, 10))
  mockery::expect_called(ri, 1)
  expect_equal(mockery::mock_args(events$hf_treatment$schedule)[[1]][[1]]$to_vector(), 1)
  expect_equal(mockery::mock_args(events$hf_treatment$schedule)[[1]][[2]], parameters$hf$travel_time + 1 + 1)
  expect_equal(mockery::mock_args(events$chw_treatment$schedule)[[1]][[1]]$to_vector(), 3)
  expect_equal(mockery::mock_args(events$chw_treatment$schedule)[[1]][[2]], parameters$chw$travel_time + 1 + 3)
  expect_equal(length(mockery::mock_args(events$private_treatment$schedule)[[1]][[1]]$to_vector()), 0)
})
