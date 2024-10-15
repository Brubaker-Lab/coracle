set.seed(123)

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

# Three data frames `x`, `y`, `z` ------------------------------------------

test_that("Argument checking for three data frames", {

  # Require all three data frame inputs ------------------------------------------

  expect_error(pcor_col(x = data.frame(values_up),
                        y = data.frame(values_up)))
  expect_error(pcor_col(x = data.frame(values_up),
                        z = data.frame(values_up)))
  expect_error(pcor_col(y = data.frame(values_up),
                        z = data.frame(values_up)))

  # Inputs are not a data frame ------------------------------------------

  expect_error(pcor_col(x = 1, y = data.frame(values_up), z = data.frame(values_up)))

  # Join specification ------------------------------------------

  expect_error(suppressMessages(corr_col(
    x = data.frame(values_up), y = data.frame(values_up)
  ))) # Only column used for joining
  expect_error(suppressMessages(corr_col(
    x = data.frame(indexes, values_up),
    y = data.frame(indexes, values_up)
  ))) # All columns used for joining
  expect_no_error(suppressMessages(corr_col(
    x = data.frame(indexes, values_up),
    y = data.frame(indexes, values_up),
    xy_join = join_by(indexes)
  ))) # Join specification for columns with the same name
  expect_no_error(suppressMessages(corr_col(
    x = data.frame(i1 = indexes, values_up),
    y = data.frame(i2 = indexes, values_up),
    xy_join = join_by(i1 == i2)
  ))) # Join specification for columns which don't have the same name
  expect_no_error(suppressMessages(corr_col(
    x = data.frame(i1 = indexes, indexes, values_up),
    y = data.frame(i2 = indexes, indexes, values_up),
    xy_join = join_by(i1 == i2)
  ))) # Join specification for columns which don't have the same name, excluding columns with the same name
})
