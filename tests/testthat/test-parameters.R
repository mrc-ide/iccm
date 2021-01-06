test_that("parameter defaults work", {
  params <- get_parameters()
  expect_type(params, "list")
  expect_equal(params$average_age, 60 * 365)
})


test_that("override parameter defaults work", {
  params_change <- get_parameters(overrides = list(average_age = 1))
  expect_equal(params_change$average_age, 1)

  expect_error(get_parameters(overrides = c(average_age = 1)), "overrides must be a list")
  expect_error(get_parameters(overrides = list(bad_param = 1)), "Unknown parameter bad_param")
})
