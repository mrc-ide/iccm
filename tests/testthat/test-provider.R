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

test_that("Treatment, diarrhoea non-severe", {

  # Single individual seeking treatment


  # Set efficacies and diagnostics to all be perfect
  parameters <- get_parameters()
  parameters$population <- 1
  for(i in seq_along(parameters$hf)){
    if(grepl("sensitivity", names(parameters$hf)[i])){
      parameters$hf[i] <- 1
    }
    if(grepl("specificity", names(parameters$hf)[i])){
      parameters$hf[i] <- 1
    }
  }
  parameters$hf$efficacy <- 1
  parameters$chw$efficacy <- 1
  parameters$private$efficacy <- 1
  parameters$dx_tx$rdt_sensitivity <- 1
  parameters$dx_tx$rdt_specificity <- 1

  events <- list()
  timestep <- 1

  variables <- list()
  status_cat <- c("uninfected", "asymptomatic", "symptomatic", "severe")
  for(disease in names(parameters$disease)){
    variables[[paste0(disease, "_status")]] <- mock_category(status_cat, "uninfected")
    variables[[paste0(disease, "_symptom_onset")]] <- mock_integer(NA)
  }
  variables$bacterial_diarrhoea_status <-  mock_category(status_cat, "symptomatic")
  variables$bacterial_diarrhoea_symptom_onset <- mock_integer(1)
  variables$awaiting_followup <- mock_integer(1)

  # Health facility
  target <- individual::Bitset$new(1)$insert(1)
  renderer <- individual::Render$new(1)
  hf <- hf_treat(variables, parameters, renderer, events)
  cstv <- mockery::mock()
  mockery::stub(hf, "clear_scheduled_treatment_visits", cstv)
  gors <- mockery::mock()
  mockery::stub(hf, "give_ors", gors)
  hf(timestep, target)

  mockery::expect_called(cstv, 1)
  mockery::expect_called(gors, 1)
  output <- renderer$to_dataframe()
  expect_equal(output$hf_patients, 1)
  expect_equal(output$hf_severe_diarrhoea_tx, 0)
  expect_equal(output$hf_severe_malaria_tx, 0)
  expect_equal(output$hf_severe_pneumonia_tx, 0)
  expect_equal(output$hf_ors, 1)
  expect_equal(output$hf_act, 0)
  expect_equal(output$hf_amoxicillin, 0)

  # CHW
  target <- individual::Bitset$new(1)$insert(1)
  renderer <- individual::Render$new(1)
  chw <- chw_treat(variables, parameters, renderer, events)
  cstv <- mockery::mock()
  mockery::stub(chw, "clear_scheduled_treatment_visits", cstv)
  gors <- mockery::mock()
  mockery::stub(chw, "give_ors", gors)
  chw(timestep, target)

  mockery::expect_called(cstv, 1)
  mockery::expect_called(gors, 1)
  output <- renderer$to_dataframe()
  expect_equal(output$chw_patients, 1)
  expect_equal(output$chw_followup, 1)
  expect_equal(output$chw_referral, 0)
  expect_equal(output$chw_ors, 1)
  expect_equal(output$chw_act, 0)
  expect_equal(output$chw_amoxicillin, 0)

  # Private
  target <- individual::Bitset$new(1)$insert(1)
  renderer <- individual::Render$new(1)
  private <- private_treat(variables, parameters, renderer, events)
  cstv <- mockery::mock()
  mockery::stub(private, "clear_scheduled_treatment_visits", cstv)
  gors <- mockery::mock()
  mockery::stub(private, "give_ors", gors)
  private(timestep, target)

  mockery::expect_called(cstv, 1)
  mockery::expect_called(gors, 1)
  output <- renderer$to_dataframe()
  expect_equal(output$private_patients, 1)
  expect_equal(output$private_ors, 1)
  expect_equal(output$private_act, 0)
  expect_equal(output$private_amoxicillin, 0)
})








test_that("Treatment, diarrhoea severe", {

  # Single individual seeking treatment
  # Set efficacies and diagnostics to all be perfect
  parameters <- get_parameters()
  parameters$population <- 1
  for(i in seq_along(parameters$hf)){
    if(grepl("sensitivity", names(parameters$hf)[i])){
      parameters$hf[i] <- 1
    }
    if(grepl("specificity", names(parameters$hf)[i])){
      parameters$hf[i] <- 1
    }
  }
  parameters$hf$efficacy <- 1
  parameters$chw$efficacy <- 1
  parameters$private$efficacy <- 1
  parameters$dx_tx$rdt_sensitivity <- 1
  parameters$dx_tx$rdt_specificity <- 1

  events <- list()
  events$hf_treatment <- mock_event()
  timestep <- 1

  variables <- list()
  status_cat <- c("uninfected", "asymptomatic", "symptomatic", "severe")
  for(disease in names(parameters$disease)){
    variables[[paste0(disease, "_status")]] <- mock_category(status_cat, "uninfected")
    variables[[paste0(disease, "_symptom_onset")]] <- mock_integer(NA)
  }
  variables$bacterial_diarrhoea_status <-  mock_category(status_cat, "severe")
  variables$bacterial_diarrhoea_symptom_onset <- mock_integer(1)
  variables$awaiting_followup <- mock_integer(1)

  # Health facility
  target <- individual::Bitset$new(1)$insert(1)
  renderer <- individual::Render$new(1)
  hf <- hf_treat(variables, parameters, renderer, events)
  cstv <- mockery::mock()
  mockery::stub(hf, "clear_scheduled_treatment_visits", cstv)
  #gors <- mockery::mock()
  #mockery::stub(hf, "give_ors", gors)
  hf(timestep, target)

  mockery::expect_called(cstv, 1)
  output <- renderer$to_dataframe()
  expect_equal(output$hf_patients, 1)
  expect_equal(output$hf_severe_diarrhoea_tx, 1)
  expect_equal(output$hf_severe_malaria_tx, 0)
  expect_equal(output$hf_severe_pneumonia_tx, 0)
  expect_equal(output$hf_ors, 0)
  expect_equal(output$hf_act, 0)
  expect_equal(output$hf_amoxicillin, 0)

  # CHW
  target <- individual::Bitset$new(1)$insert(1)
  renderer <- individual::Render$new(1)
  chw <- chw_treat(variables, parameters, renderer, events)
  cstv <- mockery::mock()
  mockery::stub(chw, "clear_scheduled_treatment_visits", cstv)
  gors <- mockery::mock()
  mockery::stub(chw, "give_ors", gors)
  chw(timestep, target)

  mockery::expect_called(cstv, 1)
  mockery::expect_called(gors, 1)
  output <- renderer$to_dataframe()
  expect_equal(output$chw_patients, 1)
  expect_equal(output$chw_followup, 1)
  expect_equal(output$chw_referral, 1)
  expect_equal(output$chw_ors, 1)
  expect_equal(output$chw_act, 0)
  expect_equal(output$chw_amoxicillin, 0)

})
