test_that("disease types are indexed correctly", {
  parameters <- get_parameters()
  disease_types <- sapply(parameters$disease, "[[", "type")
  for(type in c("diarrhoea", "malaria", "pneumonia")){
    expect_equal(type_index(parameters, type), which(disease_types == type))
  }
})


test_that("diagnosis works", {
  disease_index <- 1
  parameters <- list(
    population = 10
  )
  target <- individual::Bitset$new(10)$insert(1:10)

  # Non-severe
  variables <- list(
    infection_status = list(
      individual::CategoricalVariable$new(
        c("uninfected", "asymptomatic", "symptomatic", "severe"),
        c(rep("symptomatic", 2), rep("uninfected", 8))
      )
    )
  )
  # No false positives, no false negatives
  perfect_diagnostic <- diagnosis(target, sens = 1, spec = 1, parameters, variables, disease_index)
  expect_equal(perfect_diagnostic$to_vector(), c(1, 2))
  # All negative or false negative
  bad_diagnostic1 <- diagnosis(target, sens = 0, spec = 1, parameters, variables, disease_index)
  expect_equal(bad_diagnostic1$to_vector(), double())
  # All positive or false positive
  bad_diagnostic2 <- diagnosis(target, sens = 1, spec = 0, parameters, variables, disease_index)
  expect_equal(bad_diagnostic2$to_vector(), 1:10)
  # All positive -> negative, all negative -> positive
  bad_diagnostic3 <- diagnosis(target, sens = 0, spec = 0, parameters, variables, disease_index)
  expect_equal(bad_diagnostic3$to_vector(), 3:10)

  # Severe
  variables$infection_status = list(
    individual::CategoricalVariable$new(
      c("uninfected", "asymptomatic", "symptomatic", "severe"),
      c(rep("severe", 2), rep("uninfected", 8))
    )
  )
  # No false positives, no false negatives
  perfect_diagnostic <- severe_diagnosis(target, sens = 1, spec = 1, parameters, variables, disease_index)
  expect_equal(perfect_diagnostic$to_vector(), c(1, 2))
  # All negative or false negative
  bad_diagnostic1 <- severe_diagnosis(target, sens = 0, spec = 1, parameters, variables, disease_index)
  expect_equal(bad_diagnostic1$to_vector(), double())
  # All positive or false positive
  bad_diagnostic2 <- severe_diagnosis(target, sens = 1, spec = 0, parameters, variables, disease_index)
  expect_equal(bad_diagnostic2$to_vector(), 1:10)
  # All positive -> negative, all negative -> positive
  bad_diagnostic3 <- severe_diagnosis(target, sens = 0, spec = 0, parameters, variables, disease_index)
  expect_equal(bad_diagnostic3$to_vector(), 3:10)
})

test_that("children exceeding threshold symptom duration are identified", {
  disease_index <- 1
  parameters <- list(
    population = 10
  )
  target <- individual::Bitset$new(10)$insert(1:10)

  variables <- list(
    symptom_onset = list(
      individual::IntegerVariable$new(
        c(1, 1, 100, rep(NA, 7))
      )
    )
  )

  expect_equal(
    long_symptoms(
      target,
      disease_index,
      threshold = 14,
      timestep = 1,
      variables
    )$to_vector(),
    double())

  expect_equal(
    long_symptoms(
      target,
      disease_index,
      threshold = 14,
      timestep = 1 + 15,
      variables
    )$to_vector(),
    c(1, 2))
})


test_that("give ORS", {
  parameters <- get_parameters()
  target <- individual::Bitset$new(4)$insert(1:4)
  variables <- list()
  events <- list()
  scheduled <- individual::Bitset$new(4)$insert(3)
  scheduled2 <- individual::Bitset$new(4)

  for(disease in 1:length(parameters$disease)){
    variables$infection_status[[disease]] <- mock_category(c("uninfected", "asymptomatic", "symptomatic", "severe"),
                                                           c("uninfected", "asymptomatic", "symptomatic", "severe"))
    variables$fever[[disease]] <- mock_category(c("nonfebrile", "febrile"),
                                                c("nonfebrile", "nonfebrile", "febrile", "febrile"))
    events$severe[[disease]] <- mock_event(scheduled)
    events$susceptible[[disease]] <- mock_event(scheduled2)
  }
  parameters <- get_parameters()
  parameters$dx_tx$ors_efficacy <- 1
  parameters$dx_tx$ors_efficacy_severe <- 1
  timestep <- 1
  mockery::stub(give_ors, "stats::rexp", 10)

  give_ors(target, parameters, variables, events, timestep)

  for(disease in 1:length(parameters$disease)){
    if(parameters$disease[[disease]]$type == "diarrhoea"){
      expect_bitset_update(variables$infection_status[[disease]]$queue_update, "symptomatic", 4)
      expect_bitset_update(variables$fever[[disease]]$queue_update, "nonfebrile", 4)

      expect_equal(mockery::mock_args(events$severe[[disease]]$clear_schedule)[[1]][[1]]$to_vector(), 3)
      expect_equal(mockery::mock_args(events$susceptible[[disease]]$schedule)[[1]][[1]]$to_vector(), 3)
      expect_equal(mockery::mock_args(events$susceptible[[disease]]$schedule)[[1]][[2]], 10)
    }
  }
})

test_that("give ACT", {
  target <- individual::Bitset$new(4)$insert(1:4)
  variables <- list()
  variables$infection_status =
    list(
      individual::CategoricalVariable$new(c("uninfected", "asymptomatic", "symptomatic", "severe"),
                                          c("uninfected", "asymptomatic", "symptomatic", "severe"))
    )
  variables$time_of_last_act <- mock_integer(rep(NA, 4))
  parameters <- list()
  parameters$dx_tx$act_efficacy <- 1
  parameters$disease = list(
    plasmodium_falciparum = list(
      type = "malaria"
    )
  )
  events <- list()
  timestep <- 1
  cure_stub <- mockery::mock()
  mockery::stub(give_act, "cure", cure_stub)
  give_act(target, parameters, variables, events, timestep)

  expect_bitset_update(variables$time_of_last_act$queue_update, 1, 1:4)
  expect_equal(mockery::mock_args(cure_stub)[[1]][[1]]$to_vector(), 2:4)
  expect_equal(mockery::mock_args(cure_stub)[[1]][[2]], 1)
  expect_equal(mockery::mock_args(cure_stub)[[1]][[3]], variables)
  expect_equal(mockery::mock_args(cure_stub)[[1]][[4]], events)
})


test_that("give amoxicillin", {
  target <- individual::Bitset$new(4)$insert(1:4)
  variables <- list()
  variables$infection_status =
    list(
      individual::CategoricalVariable$new(c("uninfected", "asymptomatic", "symptomatic", "severe"),
                                          c("uninfected", "asymptomatic", "symptomatic", "severe"))
    )
  variables$time_of_last_amoxicillin <- mock_integer(rep(NA, 4))
  parameters <- list()
  parameters$disease = list(
    bacterial_pneumonia = list(
      type = "pneumonia",
      amoxicillin_efficacy = 1
    )
  )
  events <- list()
  timestep <- 1
  cure_stub <- mockery::mock()
  mockery::stub(give_amoxicillin, "cure", cure_stub)
  give_amoxicillin(target, parameters, variables, events, timestep)

  expect_bitset_update(variables$time_of_last_amoxicillin$queue_update, 1, 1:4)
  expect_equal(mockery::mock_args(cure_stub)[[1]][[1]]$to_vector(), 2:4)
  expect_equal(mockery::mock_args(cure_stub)[[1]][[2]], 1)
  expect_equal(mockery::mock_args(cure_stub)[[1]][[3]], variables)
  expect_equal(mockery::mock_args(cure_stub)[[1]][[4]], events)
})


test_that("cure",{
  target <- individual::Bitset$new(1)$insert(1)
  disease <- 1
  variables <- list()
  variables$infection_status = list(
    mock_category(c("uninfected", "asymptomatic", "symptomatic", "severe"), "symptomatic")
  )
  variables$fever = list(
    mock_category(c("nonfebrile", "febrile"), "febrile")
  )
  variables$symptom_onset = list(
    mock_integer(10)
  )
  events <- list()
  events$asymptomatic = list(
    mock_event(individual::Bitset$new(1))
  )
  events$clinical = list(
    mock_event(individual::Bitset$new(1))
  )
  events$severe = list(
    mock_event(individual::Bitset$new(1))
  )
  events$susceptible = list(
    mock_event(individual::Bitset$new(1))
  )

  cure(target, disease, variables, events)
  expect_bitset_update(variables$infection_status[[1]]$queue_update, "uninfected", 1)
  expect_bitset_update(variables$fever[[1]]$queue_update, "nonfebrile", 1)
  expect_bitset_update(variables$symptom_onset[[1]]$queue_update, as.numeric(NA), 1)

  expect_equal(mockery::mock_args(events$asymptomatic[[1]]$clear_schedule)[[1]][[1]]$to_vector(), 1)
  expect_equal(mockery::mock_args(events$clinical[[1]]$clear_schedule)[[1]][[1]]$to_vector(), 1)
  expect_equal(mockery::mock_args(events$severe[[1]]$clear_schedule)[[1]][[1]]$to_vector(), 1)
  expect_equal(mockery::mock_args(events$susceptible[[1]]$clear_schedule)[[1]][[1]]$to_vector(), 1)
})


test_that("treatment prophylaxis", {
  expect_equal(treatment_prophylaxis(c(0, NA, 100), 100), c(0, 1, 1 - exp(-100 * (1/100))))
  expect_gt(treatment_prophylaxis(5, 100), treatment_prophylaxis(1, 100))
})

test_that("has fever", {
  variables <- list()
  parameters <- get_parameters()
  parameters$population <- 2
  for(disease in 1:length(parameters$disease)){
    variables$fever[[disease]] <- mock_category(c("nonfebrile", "febrile"), c("febrile", "nonfebrile"))
  }
  expect_equal(any_fever(parameters, variables)$to_vector(), 1)
})
