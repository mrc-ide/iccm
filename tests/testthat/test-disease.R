test_that("prevalence rendering", {
  renderer <- mock_render(1)
  variables <- list()
  parameters <- get_parameters()
  parameters$population <- 8

  status_cat <- c("uninfected", "asymptomatic", "symptomatic", "severe")

  for(disease in 1:length(parameters$disease)){
    variables$infection_status[[disease]] <- mock_category(status_cat, rep(status_cat, 2))
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
  timestep <- 1
  disease <- 1
  parameters <- get_parameters()
  parameters$population <- 2
  parameters$disease = list(
    disease1 = list(
      type = "test",
      sigma = 1,
      maternal_immunity_halflife = 0,
      infection_immunity_shape = 0,
      infection_immunity_rate = 0,
      vaccine_coverage = 0
    )
  )

  variables <- list()
  variables$heterogeneity <- individual::DoubleVariable$new(c(1.0, 1.0))
  variables$prior_exposure[[1]] <- individual::IntegerVariable$new(1:2)
  variables$birth_t <- individual::IntegerVariable$new(c(-364, -364))

  target <- individual::Bitset$new(2)$insert(1:2)

  ip <- infection_probability(target,
                              disease,
                              parameters,
                              variables,
                              timestep)

  expect_equal(ip, c(rate_to_prob(1),  rate_to_prob(1)))
})

test_that("infection - no asymptopmatic pathway", {
  disease <- 1
  parameters <- get_parameters()
  parameters$population <- 5
  parameters$disease <- parameters$disease[1]
  parameters$disease[[1]]$asymptomatic_pathway <- FALSE
  timestep <- 1
  variables <- list()
  variables$het <- mock_double(rep(1, parameters$population))
  variables$prior_exposure <- list(
    mock_integer(1:5)
  )
  variables$birth_t <- mock_integer(rep(-364, parameters$population))
  renderer <- mock_render(1)
  events <- list()
  events$clinical <- list(
    mock_event()
  )
  infected <- individual::Bitset$new(5)$insert(1:5)

  infection(infected,
            disease,
            parameters,
            variables,
            events,
            renderer,
            timestep)

  expect_equal(mockery::mock_args(events$clinical[[disease]]$schedule)[[1]][[1]]$to_vector(), 1:5)
  expect_equal(mockery::mock_args(events$clinical[[disease]]$schedule)[[1]][[2]], 0)

  mockery::expect_args(renderer$render, 1, paste0(names(parameters$disease)[disease], "_clinical_infection"), 5, 1)
})

test_that("infection - including asymptopmatic pathway", {
  disease <- 1
  parameters <- get_parameters()
  parameters$population <- 5
  parameters$disease <- parameters$disease[1]
  parameters$disease[[1]]$asymptomatic_pathway <- FALSE
  timestep <- 1
  variables <- list()
  variables$het <- mock_double(rep(1, parameters$population))
  variables$prior_exposure <- list(
    mock_integer(1:5)
  )
  variables$birth_t <- mock_integer(rep(-364, parameters$population))
  renderer <- mock_render(1)
  events <- list()
  events$clinical <- list(
    mock_event()
  )
  events$asymptomatic <- list(
    mock_event()
  )
  infected <- individual::Bitset$new(5)$insert(1:5)



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

