test_that("plotting works", {
  # Mock up some test output
  test_output <- data.frame(timestep = 1:10, var1 = 10, var2 = 20)
  test_output_long <- convert_to_long(test_output)

  # All variables
  p1 <- plot_sim(test_output_long)
  expect_equal(is(p1), "patchwork")
})
