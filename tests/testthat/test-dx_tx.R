test_that("diagnostic works", {
  status <- c("S", "A", "I", "V")
  mockery::stub(diagnostic, "runif", rep(0.5, 4))

  expect_equal(diagnostic(status, sens = 1, spec = 1, positive = c("A", "I", "V")), c(FALSE, TRUE, TRUE, TRUE))
  expect_equal(diagnostic(status, sens = 0.51, spec = 1, positive = c("A", "I", "V")), c(FALSE, TRUE, TRUE, TRUE))
  expect_equal(diagnostic(status, sens = 0.5, spec = 1, positive = c("A", "I", "V")), rep(FALSE, 4))
  expect_equal(diagnostic(status, sens = 1, spec = 0.49, positive = c("A", "I", "V")), c(TRUE, TRUE, TRUE, TRUE))
  expect_equal(diagnostic(status, sens = 1, spec = 0.5, positive = c("A", "I", "V")), c(FALSE, TRUE, TRUE, TRUE))
  expect_equal(diagnostic(status, sens = 1, spec = 1, positive = c("V")), c(FALSE, FALSE, FALSE, TRUE))
})
