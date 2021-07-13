test_that("mortality probability matrix estimated correctly",{
  parameters <- list(
    population = 4,
    average_age = 50,
    disease = list(
      disease1 = list(
        daily_probability_death = 0.01
      ),
      disease2 = list(
        daily_probability_death = 0.05
      )
    )
  )

  disease_states <- c("uninfected", "asymptomatic", "symptomatic", "severe")
  variables <- list(
    infection_status = list(
      individual::CategoricalVariable$new(disease_states, disease_states),
      individual::CategoricalVariable$new(disease_states, disease_states)
    )
  )

  expect_equal(
    mortality_probability(
      parameters,
      variables
    ),
    matrix(
      c(
        0, 0, 0, parameters$disease[[1]]$daily_probability_death,
        0, 0, 0, parameters$disease[[2]]$daily_probability_death,
        rep(rate_to_prob(1 / 50), 4)
      ),
      ncol = 3
    )
  )
})


test_that("competing hazard sampling works",{
  prob_matrix <- matrix(c(1, 1, 0, 0), ncol = 2)
  expect_equal(competing_hazard(prob_matrix), c(1, 1))

  prob_matrix <- matrix(c(0, 0, 0, 0), ncol = 2)
  expect_equal(competing_hazard(prob_matrix), c(0, 0))

  prob_matrix <- matrix(c(1, 1, 1, 1), ncol = 2)
  expect_gt(sum(competing_hazard(prob_matrix)), 1)
})

test_that("death from disease works",{
  death_index = c(1, 0, 0, 0)
  parameters <- get_parameters()
  variables <- list()
  events <- list()
  renderer <- mock_render(1)
  timestep <- 1
  replace_child_stub <- mockery::mock()
  mockery::stub(death_from_diseases, "replace_child", replace_child_stub)

  death_from_diseases(death_index, parameters, variables, events, renderer, timestep)

  expect_equal(mockery::mock_args(renderer$render)[[1]][[1]], paste0(names(parameters$disease[1]), "_death"))
  expect_equal(mockery::mock_args(renderer$render)[[1]][[2]], 1)
  expect_equal(mockery::mock_args(renderer$render)[[1]][[3]], timestep)

  expect_equal(mockery::mock_args(replace_child_stub)[[1]][[1]]$to_vector(), 1)
  expect_equal(mockery::mock_args(replace_child_stub)[[1]][[2]], timestep)
  expect_equal(mockery::mock_args(replace_child_stub)[[1]][[3]], variables)
  expect_equal(mockery::mock_args(replace_child_stub)[[1]][[4]], parameters)
  expect_equal( mockery::mock_args(replace_child_stub)[[1]][[5]], events)
})

test_that("death from other causes works",{
  parameters <- get_parameters()
  death_index = c(length(parameters$disease) + 1, 0, 0, 0)
  variables <- list()
  events <- list()
  renderer <- mock_render(1)
  timestep <- 1
  replace_child_stub <- mockery::mock()
  mockery::stub(death_from_other_causes, "replace_child", replace_child_stub)

  death_from_other_causes(death_index, parameters, variables, events, renderer, timestep)

  expect_equal(mockery::mock_args(renderer$render)[[1]][[1]], "other_death")
  expect_equal(mockery::mock_args(renderer$render)[[1]][[2]], 1)
  expect_equal(mockery::mock_args(renderer$render)[[1]][[3]], timestep)

  expect_equal(mockery::mock_args(replace_child_stub)[[1]][[1]]$to_vector(), 1)
  expect_equal(mockery::mock_args(replace_child_stub)[[1]][[2]], timestep)
  expect_equal(mockery::mock_args(replace_child_stub)[[1]][[3]], variables)
  expect_equal(mockery::mock_args(replace_child_stub)[[1]][[4]], parameters)
  expect_equal( mockery::mock_args(replace_child_stub)[[1]][[5]], events)
})

