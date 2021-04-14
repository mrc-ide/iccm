test_that("diagnostic works", {
  target <- individual::Bitset$new(8)
  target <- target$insert(c(1, 3, 5, 7))
  status <- individual::IntegerVariable$new(c(0, 0, 1, 1, 2, 2, 3, 3))
  expect_equal(dx(target, status, sens = 1, spec = 1, positive = 3, negative = 0:2)$to_vector(), 7)
  expect_equal(dx(target, status, sens = 0, spec = 0, positive = 3, negative = 0:2)$to_vector(), c(1, 3, 5))
  expect_equal(dx(target, status, sens = 1, spec = 1, positive = 2:3, negative = 0:1)$to_vector(), c(5, 7))
  expect_equal(dx(target, status, sens = 0, spec = 0, positive = 2:3, negative = 0:1)$to_vector(), c(1, 3))
})

test_that("treatment prophylaxis works", {
  expect_equal(treatment_prophylaxis(c(0, NA, 100), 100), c(0, 1, 1 - exp(-100 * (1/100))))
  expect_gt(treatment_prophylaxis(5, 100), treatment_prophylaxis(1, 100))
})
