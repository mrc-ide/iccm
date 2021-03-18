test_that("format works", {
  # Mock up some test output
  test_output <- data.frame(timestep = 1:10, var1 = 10, var2 = 20)

  # Mock up so when we call run_simulation, the internal call to individual::simulate
    # returns the test output (2x for the 2 calls)
  m <- mockery::mock(test_output, test_output)
  mockery::stub(run_simulation, "renderer$to_dataframe", m)

  # Run tests with output options for wide and long
  output_wide <- run_simulation(10, parameters = list(population = 5), long = FALSE)
  expect_equal(dim(output_wide), dim(test_output))

  output_long <- run_simulation(10, parameters = list(population = 5), long = TRUE)
  expect_equal(dim(output_long), c(20, 3))
})
