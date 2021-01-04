test_that("parameter defaults work", {
  params <- get_parameters()
  expect_type(params, "list")
  expect_equal(params$lifespan, 80 * 365)
})


test_that("override parameter defaults work", {
  params_change <- get_parameters(overrides = list(lifespan = 1))
  expect_equal(params_change$lifespan, 1)

  expect_error(get_parameters(overrides = c(lifespan = 1)), "overrides must be a list")
  expect_error(get_parameters(overrides = list(bad_param = 1)), "Unknown parameter bad_param")
})
