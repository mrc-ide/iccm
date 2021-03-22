test_that("diagnostic works", {
  status <- c("S", "A", "I", "V")
  mockery::stub(dx, "runif", rep(0.5, 4))

  expect_equal(dx(status, sens = 1, spec = 1, positive = c("A", "I", "V")), c(FALSE, TRUE, TRUE, TRUE))
  expect_equal(dx(status, sens = 0.51, spec = 1, positive = c("A", "I", "V")), c(FALSE, TRUE, TRUE, TRUE))
  expect_equal(dx(status, sens = 0.5, spec = 1, positive = c("A", "I", "V")), rep(FALSE, 4))
  expect_equal(dx(status, sens = 1, spec = 0.49, positive = c("A", "I", "V")), c(TRUE, TRUE, TRUE, TRUE))
  expect_equal(dx(status, sens = 1, spec = 0.5, positive = c("A", "I", "V")), c(FALSE, TRUE, TRUE, TRUE))
  expect_equal(dx(status, sens = 1, spec = 1, positive = c("V")), c(FALSE, FALSE, FALSE, TRUE))
})

test_that("treatment works", {
  disease <- c(1, 2, 3, 4)
  mockery::stub(tx, "runif", rep(0.5, 4))

  expect_equal(tx(disease, 1, c(2, 4)), c(FALSE, TRUE, FALSE, TRUE))
  expect_equal(tx(disease, 0.51, c(2, 4)), c(FALSE, TRUE, FALSE, TRUE))
  expect_equal(tx(disease, 0.5, c(2, 4)), c(FALSE, FALSE, FALSE, FALSE))
  expect_equal(tx(disease, 1, c(1, 3)), c(TRUE, FALSE, TRUE, FALSE))
})


test_that("provider works", {
  disease <- c(1, 2, 3, 4)
  mockery::stub(px, "runif", rep(0.5, 4))

  expect_equal(px(disease, 1), c(TRUE, TRUE, TRUE, TRUE))
  expect_equal(px(disease, 0.51), c(TRUE, TRUE, TRUE, TRUE))
  expect_equal(px(disease, 0.5), c(FALSE, FALSE, FALSE, FALSE))
})
