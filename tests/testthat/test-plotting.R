test_that("plotting works", {
  # Mock up some test output
  test_output <- data.frame(timestep = 1:10, var1 = 10, var2 = 20)
  test_output_long <- convert_to_long(test_output)

  # All variables
  p1 <- plot_sim(test_output_long)
  expect_equal(is(p1), "gg")
  expect_equal(length(unique(ggplot2::ggplot_build(p1)$data[[1]]$PANEL)), 2)

  # Single variable selected
  p2 <- plot_sim(test_output_long, vars = "var1")
  expect_equal(is(p2), "gg")
  expect_equal(length(unique(ggplot2::ggplot_build(p2)$data[[1]]$PANEL)), 1)
})
