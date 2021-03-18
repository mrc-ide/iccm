test_that("vaccine effect works", {
  expect_equal(vaccine_effect(ages = 0, vx_start = 1, vx_initial_efficacy = 1, vx_hl = 100), 0)
  expect_equal(vaccine_effect(ages = 0, vx_start = 0, vx_initial_efficacy = 1, vx_hl = 100), 1)
  expect_equal(vaccine_effect(ages = 0, vx_start = 0, vx_initial_efficacy = 0.9, vx_hl = 100), 0.9)
  expect_equal(vaccine_effect(ages = 0:100, vx_start = 0, vx_initial_efficacy = 0.9, vx_hl = Inf), rep(0.9, 101))
})

timesteps <- 100
parameters = get_parameters()
parameters$population <- 3
variables <- create_variables(parameters)

test_that("vaccine impact works", {
  variables$rotavirus_vx <- mock_integer(rep(1, 4))
  vi <- vaccine_impact(disease = "rotavirus", index = 4, target = individual::filter_bitset(variables$dia_status$get_index_of(values = "S"), 1), ages = 200, parameters$dia, variables)
  expect_equal(vi, 1 - vaccine_effect(ages = 200, vx_start = parameters$dia$vx_start[4],
                                       vx_initial_efficacy = parameters$dia$vx_initial_efficacy[4],
                                       vx_hl = parameters$dia$vx_hl[4]))
})

test_that("community impact works", {
  expect_equal(community_impact("rotavirus", 4, parameters$dia), 1 - parameters$dia$vx_ci[4])
})

