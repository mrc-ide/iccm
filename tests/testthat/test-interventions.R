test_that("vaccine effect", {
  expect_equal(vaccine_effect(ages = 0, vaccine_start = 1, vaccine_initial_efficacy = 1, vaccine_hl = 100), 0)
  expect_equal(vaccine_effect(ages = 0, vaccine_start = 0, vaccine_initial_efficacy = 1, vaccine_hl = 100), 1)
  expect_equal(vaccine_effect(ages = 0, vaccine_start = 0, vaccine_initial_efficacy = 0.9, vaccine_hl = 100), 0.9)
  expect_equal(vaccine_effect(ages = 0:100, vaccine_start = 0, vaccine_initial_efficacy = 0.9, vaccine_hl = Inf), rep(0.9, 101))
})

test_that("vaccine impact", {
  parameters <- get_parameters()
  parameters$disease[[1]]$vaccine_coverage = 1
  parameters$disease[[1]]$vaccine_start = 0
  parameters$disease[[1]]$vaccine_initial_efficacy = 1
  parameters$disease[[1]]$vaccine_hl = 365
  variables <- list()
  variables$vaccine =
    list(
      mock_integer(
        c(0, 0, 1, 1)
      )
    )
  target <- individual::Bitset$new(4)$insert(1:4)
  ages <- c(10, 100, 500, 1000)

  expect_equal(vaccine_impact(target, 1, ages, parameters, variables), c(1.0000000, 1.0000000, 0.7458582, 0.9354120), tolerance = 0.0000001)
})

test_that("llin impacy",{
  parameters <- get_parameters()
  disease <- which(names(parameters$disease) == "plasmodium_falciparum")
  variables <- list()
  variables$llin <- mock_integer(c(0, 0, 1, 1))
  target <- individual::Bitset$new(4)$insert(1:4)

  expect_equal(llin_impact(target, disease, parameters, variables), c(1, 1,  1 - parameters$disease$plasmodium_falciparum$llin_efficacy, 1 - parameters$disease$plasmodium_falciparum$llin_efficacy))
})
