source(here::here("tests/testthat/test-data.R"))

test_that("corr() for one data frame", {
  expect_true(exists("corr")) # Prevents "Empty Test" warning

  f <- suppressMessages(corr)

  test_that("Valid inputs do not throw errors",{
    expect_no_error(f(test_data, x_join = "a"))
  })

  test_that("Invalid inputs throw errors",{
    expect_error(f(1))
    expect_error(f("a"))
    expect_error(f(c("a","b","c")))
  })
})
