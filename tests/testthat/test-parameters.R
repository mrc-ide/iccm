test_that("parameter defaults work", {
  params <- get_parameters()
  expect_type(params, "list")
  expect_equal(params$average_age, 60 * 365)
})


