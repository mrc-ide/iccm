test_that("random draws from truncated exponential", {
  expect_error(rtexp(n = 0), "n >= 1 is not TRUE", fixed = TRUE)
  expect_error(rtexp(rate = 0), "rate > 0 is not TRUE", fixed = TRUE)
  expect_error(rtexp(upper = 0), "upper > 0 is not TRUE", fixed = TRUE)
  expect_error(rtexp(n = "A"), "is.numeric(n) is not TRUE", fixed = TRUE)
  expect_error(rtexp(rate = "A"), "is.numeric(rate) is not TRUE", fixed = TRUE)
  expect_error(rtexp(upper = "A"), "is.numeric(upper) is not TRUE", fixed = TRUE)

  draw <- rtexp()
  expect_equal(length(draw), 1)
  expect_type(draw, "double")
})

test_that("rate to probability",{
  expect_error(rate_to_prob(r = -1), "r >= 0 is not TRUE", fixed = TRUE)
  expect_error(rate_to_prob(r = 1, t = 0), "t > 0 is not TRUE", fixed = TRUE)
  expect_error(rate_to_prob(r = "A"), "is.numeric(r) is not TRUE", fixed = TRUE)
  expect_error(rate_to_prob(r = 1, t = "A"), "is.numeric(t) is not TRUE", fixed = TRUE)

  expect_equal(rate_to_prob(0), 0)
  expect_equal(rate_to_prob(Inf), 1)
})
