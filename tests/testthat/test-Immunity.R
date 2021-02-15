test_that("immunity works", {
  expect_equal(maternal_immunity(100, 100), 1 - exp(-100 * (1/100)))
  expect_equal(exposure_immunity(2, 1, 1), 1 - stats::pgamma(q = 2, shape = 1, rate = 1))
})
