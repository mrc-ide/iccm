test_that("disease types are indexed correctly", {
  parameters <- get_parameters()
  disease_types <- sapply(parameters$disease, "[[", "type")
  for(type in c("diarrhoea", "malaria", "pneumonia")){
    expect_equal(type_index(parameters,type), which(disease_types == type))
  }
})


test_that("diagnosis works", {
  disease_index <- 1
  parameters <- list(
    population = 10
  )
  variables <- list(
    infection_status = list(
      individual::CategoricalVariable$new(
        c("uninfected", "asymptomatic", "symptomatic", "severe"),
        c(rep("symptomatic", 2), rep("uninfected", 8))
      )
    )
  )

  target <- individual::Bitset$new(10)$insert(1:10)
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
})

















test_that("diagnosis", {
  parameters <- get_parameters()
  target <- individual::Bitset$new(4)$insert(1:4)
  disease_type <- "diarrhoea"
  sens <- 1
  spec <- 1
  variables <- list()
  for(disease in names(parameters$disease)){
    variables[[paste0(disease, "_status")]] <- mock_category(c("uninfected", "asymptomatic", "symptomatic", "severe"), c("uninfected", "asymptomatic", "symptomatic", "severe"))
  }
  parameters <- get_parameters()
  parameters$population <- 4

  diagnosed <- diagnosis(target, disease_type, sens, spec, variables, parameters)
  expect_equal(diagnosed$to_vector(), 2:4)

  spec <- 0
  diagnosed <- diagnosis(target, disease_type, sens, spec, variables, parameters)
  expect_equal(diagnosed$to_vector(), 1:4)

  sens <- 0
  diagnosed <- diagnosis(target, disease_type, sens, spec, variables, parameters)
  expect_equal(diagnosed$to_vector(), 1)
})

test_that("severe diagnosis", {
  parameters <- get_parameters()
  target <- individual::Bitset$new(4)$insert(1:4)
  disease_type <- "diarrhoea"
  sens <- 1
  spec <- 1
  variables <- list()
  for(disease in names(parameters$disease)){
    variables[[paste0(disease, "_status")]] <- mock_category(c("uninfected", "asymptomatic", "symptomatic", "severe"), c("uninfected", "asymptomatic", "symptomatic", "severe"))
  }
  parameters <- get_parameters()
  parameters$population <- 4

  diagnosed <- severe_diagnosis(target, disease_type, sens, spec, variables, parameters)
  expect_equal(diagnosed$to_vector(), 4)

  spec <- 0
  diagnosed <- severe_diagnosis(target, disease_type, sens, spec, variables, parameters)
  expect_equal(diagnosed$to_vector(), 1:4)

  sens <- 0
  diagnosed <- severe_diagnosis(target, disease_type, sens, spec, variables, parameters)
  expect_equal(diagnosed$to_vector(), 1:3)
})

test_that("long symptoms", {
  parameters <- get_parameters()
  target <- individual::Bitset$new(4)$insert(1:4)
  disease_type <- "diarrhoea"
  variables <- list()
  for(disease in names(parameters$disease)){
    variables[[paste0(disease, "_symptom_onset")]] <- mock_integer(rep(0, 4))
  }
  parameters <- get_parameters()

  with_long <- long_symptoms(target, disease_type, threshold = 14, timestep = 15, variables, parameters)
  expect_equal(with_long$to_vector(), 1:4)

  with_long <- long_symptoms(target, disease_type, threshold = 14, timestep = 13, variables, parameters)
  expect_length(with_long$to_vector(), 0)
})

test_that("give ORS", {
  parameters <- get_parameters()
  target <- individual::Bitset$new(4)$insert(1:4)
  variables <- list()
  events <- list()
  scheduled <- individual::Bitset$new(4)$insert(3)
  scheduled2 <- individual::Bitset$new(4)
  for(disease in names(parameters$disease)){
    variables[[paste0(disease, "_status")]] <- mock_category(c("uninfected", "asymptomatic", "symptomatic", "severe"), c("uninfected", "asymptomatic", "symptomatic", "severe"))
    variables[[paste0(disease, "_fever")]] <- mock_category(c("nonfebrile", "febrile"), c("nonfebrile", "nonfebrile", "febrile", "febrile"))
    events[[paste0(disease, "_progress_to_severe_infection")]] <- mock_event(scheduled)
    events[[paste0(disease, "_progress_to_uninfected")]] <- mock_event(scheduled2)
  }
  parameters <- get_parameters()
  parameters$dx_tx$ors_efficacy <- 1
  parameters$dx_tx$ors_efficacy_severe <- 1
  timestep <- 1
  mockery::stub(give_ors, "rexp", 10)
  give_ors(target, parameters, variables, events, timestep)

  for(disease in names(parameters$disease)){
    if(parameters$disease[[disease]]$type == "diarrhoea"){
      expect_bitset_update(variables[[paste0(disease, "_status")]]$queue_update, "symptomatic", 4)
      expect_bitset_update(variables[[paste0(disease, "_fever")]]$queue_update, "nonfebrile", 4)

      expect_equal(mockery::mock_args(events[[paste0(disease, "_progress_to_severe_infection")]]$clear_schedule)[[1]][[1]]$to_vector(), 3)
      expect_equal(mockery::mock_args(events[[paste0(disease, "_progress_to_uninfected")]]$schedule)[[1]][[1]]$to_vector(), 3)
      expect_equal(mockery::mock_args(events[[paste0(disease, "_progress_to_uninfected")]]$schedule)[[1]][[2]], 10)
    }
  }
})

test_that("give ACT", {
  target <- individual::Bitset$new(4)$insert(1:4)
  variables <- list()
  variables$plasmodium_falciparum_status <- mock_category(c("uninfected", "asymptomatic", "symptomatic", "severe"), c("uninfected", "asymptomatic", "symptomatic", "severe"))
  variables$time_of_last_act <- mock_integer(rep(NA, 4))
  parameters <- get_parameters()
  parameters$dx_tx$act_efficacy <- 1
  events <- list()
  timestep <- 1
  cure_stub <- mockery::mock()
  mockery::stub(give_act, "cure", cure_stub)
  give_act(target, parameters, variables, events, timestep)

  expect_bitset_update(variables$time_of_last_act$queue_update, 1, 1:4)
  expect_equal(mockery::mock_args(cure_stub)[[1]][[1]]$to_vector(), 2:4)
  expect_equal(mockery::mock_args(cure_stub)[[1]][[2]], "plasmodium_falciparum")
  expect_equal(mockery::mock_args(cure_stub)[[1]][[3]], variables)
  expect_equal(mockery::mock_args(cure_stub)[[1]][[4]], events)
})


test_that("give amoxicillin", {
  target <- individual::Bitset$new(4)$insert(1:4)
  variables <- list()
  parameters <- get_parameters()
  for(disease in names(parameters$disease)){
    variables[[paste0(disease, "_status")]] <- mock_category(c("uninfected", "asymptomatic", "symptomatic", "severe"), c("uninfected", "asymptomatic", "symptomatic", "severe"))
    parameters$disease[[disease]]$amoxicillin_efficacy <- 1
  }
  variables$time_of_last_amoxicillin <- mock_integer(rep(NA, 4))

  events <- list()
  timestep <- 1
  cure_stub <- mockery::mock()
  mockery::stub(give_amoxicillin, "cure", cure_stub)
  give_amoxicillin(target, parameters, variables, events, timestep)

  expect_bitset_update(variables$time_of_last_amoxicillin$queue_update, 1, 1:4)
  for(disease in names(parameters$disease)){
    if(grepl("bacterial", disease)){
      expect_equal(mockery::mock_args(cure_stub)[[1]][[1]]$to_vector(), 2:4)
      expect_equal(mockery::mock_args(cure_stub)[[1]][[2]], disease)
      expect_equal(mockery::mock_args(cure_stub)[[1]][[3]], variables)
      expect_equal(mockery::mock_args(cure_stub)[[1]][[4]], events)
    }
  }
})

test_that("cure",{
  target <- individual::Bitset$new(1)$insert(1)
  disease <- "bacterial_diarrhoea"
  variables <- list()
  variables$bacterial_diarrhoea_status <- mock_category(c("uninfected", "asymptomatic", "symptomatic", "severe"), "symptomatic")
  variables$bacterial_diarrhoea_fever <- mock_category(c("nonfebrile", "febrile"), "febrile")
  variables$bacterial_diarrhoea_symptom_onset <- mock_integer(10)
  events <- list()
  events$bacterial_diarrhoea_progress_to_asymptomatic_infection <- mock_event(individual::Bitset$new(1))
  events$bacterial_diarrhoea_progress_to_clinical_infection <- mock_event(individual::Bitset$new(1))
  events$bacterial_diarrhoea_progress_to_severe_infection <- mock_event(individual::Bitset$new(1))
  events$bacterial_diarrhoea_progress_to_uninfected <- mock_event(individual::Bitset$new(1))

  cure(target, disease, variables, events)
  expect_bitset_update(variables$bacterial_diarrhoea_status$queue_update, "uninfected", 1)
  expect_bitset_update(variables$bacterial_diarrhoea_fever$queue_update, "nonfebrile", 1)
  expect_bitset_update(variables$bacterial_diarrhoea_symptom_onset$queue_update, NA, 1)

  expect_equal(mockery::mock_args(events$bacterial_diarrhoea_progress_to_asymptomatic_infection$clear_schedule)[[1]][[1]]$to_vector(), 1)
  expect_equal(mockery::mock_args(events$bacterial_diarrhoea_progress_to_clinical_infection$clear_schedule)[[1]][[1]]$to_vector(), 1)
  expect_equal(mockery::mock_args(events$bacterial_diarrhoea_progress_to_severe_infection$clear_schedule)[[1]][[1]]$to_vector(), 1)
  expect_equal(mockery::mock_args(events$bacterial_diarrhoea_progress_to_uninfected$clear_schedule)[[1]][[1]]$to_vector(), 1)
})

test_that("treatment prophylaxis", {
  expect_equal(treatment_prophylaxis(c(0, NA, 100), 100), c(0, 1, 1 - exp(-100 * (1/100))))
  expect_gt(treatment_prophylaxis(5, 100), treatment_prophylaxis(1, 100))
})

test_that("has fever", {
  variables <- list()
  parameters <- get_parameters()
  parameters$population <- 2
  for(disease in names(parameters$disease)){
    variables[[paste0(disease, "_fever")]] <- mock_category(c("nonfebrile", "febrile"), c("febrile", "nonfebrile"))
  }
  expect_equal(any_fever(parameters, variables)$to_vector(), 1)
})
