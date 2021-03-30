test_that("diagnostic works", {
  status <- c(0, 1, 2, 3)
  mockery::stub(dx, "stats::runif", rep(0.5, 4))

  expect_equal(dx(status, sens = 1, spec = 1, positive = 1:3), c(FALSE, TRUE, TRUE, TRUE))
  expect_equal(dx(status, sens = 0.51, spec = 1, positive = 1:3), c(FALSE, TRUE, TRUE, TRUE))
  expect_equal(dx(status, sens = 0.5, spec = 1, positive = 1:3), rep(FALSE, 4))
  expect_equal(dx(status, sens = 1, spec = 0.49, positive = 1:3), c(TRUE, TRUE, TRUE, TRUE))
  expect_equal(dx(status, sens = 1, spec = 0.5, positive = 1:3), c(FALSE, TRUE, TRUE, TRUE))
  expect_equal(dx(status, sens = 1, spec = 1, positive = 3), c(FALSE, FALSE, FALSE, TRUE))
})

test_that("provider works", {
  disease <- c(1, 2, 3, 4)
  mockery::stub(px, "stats::runif", rep(0.5, 4))

  expect_equal(px(disease, 1), c(TRUE, TRUE, TRUE, TRUE))
  expect_equal(px(disease, 0.51), c(TRUE, TRUE, TRUE, TRUE))
  expect_equal(px(disease, 0.5), c(FALSE, FALSE, FALSE, FALSE))
})
