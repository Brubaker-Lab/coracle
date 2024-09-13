indexes <- c("a", "b", "c", "d", "e")
indexes_alt <- c("c", "d", "e", "f", "g")
values_up <- c(1, 2, 3, 4, 5)
values_down <- c(5, 4, 3, 2, 1)
values_constant <- c(1, 1, 1, 1, 1)
values_up_na <- c(1, 2, 3, NA, 5)
values_dn_na <- c(5, NA, 3, 2, 1)
values_all_na <- as.numeric(c(NA, NA, NA, NA, NA))
values_single <- c(NA, NA, 3, NA, NA)
values_nonnumeric <- c("a", "b", "c", "d", "e")
values_random <- c(runif(5))

# Single data frame `x` ------------------------------------------

test_that("Argument checking for single data frame",{

  # `x` is not a data frame ------------------------------------------

  expect_error(corr_col(x = "1")) # not a data frame
  expect_error(corr_col(x = 1)) # not a data frame
  expect_error(corr_col(x = c(1,2))) # not a data frame

  # `x` has less than two numeric columns ------------------------------------------

  expect_error(corr_col(x = data.frame()))
  expect_error(corr_col(x = data.frame(values_up)))
  expect_error(corr_col(x = data.frame(values_random)))
  expect_error(corr_col(x = data.frame(values_nonnumeric)))
  expect_error(corr_col(x = data.frame(values_up, values_nonnumeric)))

  # `x` arguments as expected ------------------------------------------

  expect_no_error(corr_col(x = data.frame(values_up, values_down, values_nonnumeric)))
  expect_no_error(corr_col(x = data.frame(values_up, values_down)))
  expect_no_error(corr_col(x = data.frame(values_up, values_up)))
  expect_no_error(corr_col(x = data.frame(values_up, values_up, values_random)))
  expect_no_error(corr_col(x = tibble(values_up, values_down)))

})

test_that("Result checking for single data frame",{
  expect_equal(corr_col(x = data.frame(values_up, values_up))$rho,1)
  expect_equal(corr_col(x = data.frame(values_down, values_down))$rho,1)
  expect_equal(corr_col(x = data.frame(values_up, values_up_na))$rho,1)
  expect_equal(corr_col(x = data.frame(values_up, values_single))$rho,NA)
  expect_equal(corr_col(x = data.frame(values_up, values_all_na))$rho,NA)
})

# Two data frames `x`, `y` ------------------------------------------

test_that("Argument checking for two data frames",{

  # `y` is not a data frame ------------------------------------------

  expect_error(corr_col(x = data.frame(values_up), y = data.frame(values_up))) # not a data frame
  expect_error(corr_col(x = 1)) # not a data frame
  expect_error(corr_col(x = c(1,2))) # not a data frame
})
