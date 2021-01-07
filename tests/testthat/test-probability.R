test_that("random draws from truncated exponential", {
  expect_error(rtexp(n = 0), "n >= 1 is not TRUE", fixed = TRUE)
  expect_error(rtexp(rate = 0), "rate > 0 is not TRUE", fixed = TRUE)
  expect_error(rtexp(upper = 0), "lower < upper is not TRUE", fixed = TRUE)
  expect_error(rtexp(lower = -1), "lower >= 0 is not TRUE", fixed = TRUE)

  draw <- rtexp()
  expect_equal(length(draw), 1)
  expect_type(draw, "double")
})

test_that("rate probability conversions",{
  expect_equal(rate_to_prob(0), 0)
  expect_equal(rate_to_prob(Inf), 1)

  expect_equal(prob_to_rate(0), 0)
  expect_equal(prob_to_rate(1), Inf)

  expect_equal(rate_to_prob(prob_to_rate(0.75)), 0.75)
  expect_equal(prob_to_rate(rate_to_prob(1.23)), 1.23)
})
