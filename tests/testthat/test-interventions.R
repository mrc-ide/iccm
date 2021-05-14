test_that("vaccine effect", {
  expect_equal(vaccine_effect(ages = 0, vx_start = 1, vx_initial_efficacy = 1, vx_hl = 100), 0)
  expect_equal(vaccine_effect(ages = 0, vx_start = 0, vx_initial_efficacy = 1, vx_hl = 100), 1)
  expect_equal(vaccine_effect(ages = 0, vx_start = 0, vx_initial_efficacy = 0.9, vx_hl = 100), 0.9)
  expect_equal(vaccine_effect(ages = 0:100, vx_start = 0, vx_initial_efficacy = 0.9, vx_hl = Inf), rep(0.9, 101))
})

test_that("vaccine impact", {
  parameters <- get_parameters()
  p <- parameters$disease$rotavirus
  variables <- list()
  variables$rotavirus_vx <- mock_integer(c(0, 0, 1, 1))
  target <- individual::Bitset$new(4)$insert(1:4)
  ages <- c(10, 100, 500, 1000)

  expect_equal(vaccine_impact("rotavirus", target, ages, p, variables), c(1.0000000, 1.0000000, 0.7326065, 0.9320441), tolerance = 0.0000001)
})

test_that("llin impacy",{
  parameters <- get_parameters()
  p <- parameters$disease$plasmodium_falciparum
  variables <- list()
  variables$llin <- mock_integer(c(0, 0, 1, 1))
  target <- individual::Bitset$new(4)$insert(1:4)

  expect_equal(llin_impact("plasmodium_falciparum", target, p, variables), c(1, 1,  1 - p$llin_efficacy, 1 - p$llin_efficacy))
})
