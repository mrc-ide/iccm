test_that("vaccine effect works", {
  expect_equal(vaccine_effect(ages = 0, vx_start = 1, vx_initial_efficacy = 1, vx_hl = 100), 0)
  expect_equal(vaccine_effect(ages = 0, vx_start = 0, vx_initial_efficacy = 1, vx_hl = 100), 1)
  expect_equal(vaccine_effect(ages = 0, vx_start = 0, vx_initial_efficacy = 0.9, vx_hl = 100), 0.9)
  expect_equal(vaccine_effect(ages = 0:100, vx_start = 0, vx_initial_efficacy = 0.9, vx_hl = Inf), rep(0.9, 101))
})

p <- get_parameters()
p$population <- 2
states <- create_states(p)
variables <- create_variables(p)
events <- create_events(individuals, variables)
individuals <- create_individuals(states, variables, events)
processes <- create_processes(p, individuals, variables, events)

test_that("vaccine impact works", {
  api <- mock_api(list(child = list(
    NULLSTATE = c(1, 2),
    rotavirus_vx = c(0, 1))),
    parameters = p, timestep = 0)

  vi <- vaccine_impact("rotavirus", 4, 1:2, c(100, 100), p$diarrhoea, individuals, variables, api)
  expect_equal(vi, 1 - c(0, vaccine_effect(ages = 100, vx_start = p$diarrhoea$vx_start[4],
                                       vx_initial_efficacy = p$diarrhoea$vx_initial_efficacy[4],
                                       vx_hl = p$diarrhoea$vx_hl[4])))
})

test_that("community impact works", {
  expect_equal(community_impact("rotavirus", 4, p$diarrhoea), 1 - p$diarrhoea$vx_ci[4])
})

#test_that("llin impact works", {
#})
