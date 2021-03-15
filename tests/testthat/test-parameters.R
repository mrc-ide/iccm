test_that("parameter defaults work", {
  params <- get_parameters()
  expect_type(params, "list")
  expect_equal(params$average_age, 60 * 365)
})

test_that("parameter overrides work", {
  tl <- list(a = 1, b = list(c = 2))
  expect_equal(nested_list_names(tl), c("a", "b.c"))
  params <- get_parameters(user_overwrite = list(dia = list(sigma = rep(100, 4))))
  expect_equal(params$dia$sigma, rep(100, 4))
  expect_error(get_parameters(user_overwrite = list(wrongname = 1)),
               "User input parameter(s) not recognised: wrongname", fixed = TRUE)
})


