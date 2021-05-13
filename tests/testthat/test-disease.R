test_that("prevalence rendering", {
  renderer <- mock_render(1)
  variables <- list()
  parameters <- get_parameters()
  parameters$population <- 8

  status_cat <- c("uninfected", "asymptomatic", "symptomatic", "severe")

  for(disease in names(parameters$disease)){
    variables[[paste0(disease, "_status")]] <- mock_category(status_cat, rep(status_cat, 2))
  }
  pr <- render_prevalence(variables, renderer, parameters)
  pr(1)

  call <- 1
  for(disease in names(parameters$disease)){
    mockery::expect_args(renderer$render, call, paste0(disease, "_prevalence"), 0.75, 1)
    call <- call + 1
  }
})

test_that("infection probability", {
  disease <- "bacterial_diarrhoea"
  parameters <- get_parameters()
  parameters$population <- 5
  p <- parameters$disease[[disease]]
  timestep <- 1
  variables <- list()
  variables$het <- mock_double(rep(1, parameters$population))
  variables$bacterial_diarrhoea_prior_exposure <- mock_integer(1:5)
  variables$birth_t <- mock_integer(rep(-364, parameters$population))

  target <- individual::Bitset$new(5)$insert(1:5)
  ip <- infection_probability(disease, target, p, timestep, variables)
  expect_equal(ip, c(9.703971e-05, 9.226818e-05, 7.940421e-05, 6.124740e-05, 4.290352e-05), tolerance = 0.0000001)
})

test_that("infection - no asymptopmatic pathway", {
  disease <- "bacterial_diarrhoea"
  parameters <- get_parameters()
  parameters$population <- 5
  p <- parameters$disease[[disease]]
  timestep <- 1
  variables <- list()
  variables$het <- mock_double(rep(1, parameters$population))
  variables$bacterial_diarrhoea_prior_exposure <- mock_integer(1:5)
  variables$birth_t <- mock_integer(rep(-364, parameters$population))
  renderer <- mock_render(1)
  events <- list()
  events$bacterial_diarrhoea_progress_to_clinical_infection <- mock_event()
  infected <- individual::Bitset$new(5)$insert(1:5)

  infection(infected, disease, parameters, variables, events, renderer, timestep)

  expect_equal(mockery::mock_args(events$bacterial_diarrhoea_progress_to_clinical_infection$schedule)[[1]][[1]]$to_vector(), 1:5)
  expect_equal(mockery::mock_args(events$bacterial_diarrhoea_progress_to_clinical_infection$schedule)[[1]][[2]], 0)

  mockery::expect_args(renderer$render, 1, paste0(disease, "_clinical_infection"), 5, 1)
})

test_that("infection - including asymptopmatic pathway", {
  disease <- "plasmodium_falciparum"
  parameters <- get_parameters()
  parameters$population <- 5
  p <- parameters$disease[[disease]]
  timestep <- 1
  variables <- list()
  variables$het <- mock_double(rep(1, parameters$population))
  variables$plasmodium_falciparum_prior_exposure <- mock_integer(1:5)
  variables$plasmodium_falciparum_status <- mock_category(c("uninfected", "asymptomatic", "symptomatic", "severe"), rep("uninfected", 5))
  variables$birth_t <- mock_integer(rep(-364, parameters$population))
  renderer <- mock_render(1)
  events <- list()
  events$plasmodium_falciparum_progress_to_clinical_infection <- mock_event()
  events$plasmodium_falciparum_progress_to_asymptomatic_infection <- mock_event()
  infected <- individual::Bitset$new(5)$insert(1:5)
  mockery::stub(infection, "stats::runif", how = c(0, 0, 1, 1, 1))

  infection(infected, disease, parameters, variables, events, renderer, timestep)

  expect_equal(mockery::mock_args(events$plasmodium_falciparum_progress_to_clinical_infection$schedule)[[1]][[1]]$to_vector(), 1:2)
  expect_equal(mockery::mock_args(events$plasmodium_falciparum_progress_to_clinical_infection$schedule)[[1]][[2]], 0)

  expect_equal(mockery::mock_args(events$plasmodium_falciparum_progress_to_asymptomatic_infection$schedule)[[1]][[1]]$to_vector(), 3:5)
  expect_equal(mockery::mock_args(events$plasmodium_falciparum_progress_to_asymptomatic_infection$schedule)[[1]][[2]], 0)

  mockery::expect_args(renderer$render, 1, paste0(disease, "_clinical_infection"), 2, 1)
})

test_that("increment counter", {
  variable <- mock_integer(1:5)
  target <- individual::Bitset$new(5)$insert(1:3)
  increment_counter(target, variable)
  expect_bitset_update(variable$queue_update, c(1:3) + 1, 1:3)
})

