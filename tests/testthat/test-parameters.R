test_that("parameter defaults work", {
  parameters <- get_parameters()
  expect_type(parameters, "list")
  expect_equal(parameters$average_age, 60 * 365)
})

test_that("parameter overrides work", {
  tl <- list(a = 1, b = list(c = 2))
  expect_equal(nested_list_names(tl), c("a", "b.c"))
  parameters <- get_parameters(user_overwrite = list(disease = list(plasmodium_falciparum = list(sigma = 0.123))))
  expect_equal(parameters$disease$plasmodium_falciparum$sigma, 0.123)
  expect_error(get_parameters(user_overwrite = list(wrongname = 1)),
               "User input parameter(s) not recognised: wrongname", fixed = TRUE)
})


