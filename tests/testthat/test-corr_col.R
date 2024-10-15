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

# `corr()` ------------

# corr(var_x, var_y, corr_data, method)

## inputs ------------

### basic ------------

test_that("Basic `corr()` inputs",{
  expect_no_error(corr(var_x = "x",
                  var_y = "y",
                  corr_data = data.frame(x = 1:5, y = 1:5),
                  method = "spearman"))
})

### advanced ------------

test_that("Advanced `corr()` inputs",{})

### errors ------------

test_that("Error `corr()` inputs",{})

## results ------------

### basic ------------

### advanced ------------

### errors ------------


# `corr_x()` ------------

## inputs ------------

### basic ------------

### advanced ------------

### errors ------------

## results ------------

### basic ------------

### advanced ------------

### errors ------------

# `corr_xy()` ------------

## inputs ------------

### basic ------------

### advanced ------------

### errors ------------

## results ------------

### basic ------------

### advanced ------------

### errors ------------

# `corr_col()` ------------

## inputs ------------

### basic ------------

### advanced ------------

### errors ------------

## results ------------

### basic ------------

### advanced ------------

### errors ------------










# Single data frame `x` case ------------------------------------------

test_that("Argument checking for single data frame", {
  # `x` is not a data frame ------------------------------------------

  expect_error(corr_col(x = "1")) # not a data frame
  expect_error(corr_col(x = 1)) # not a data frame
  expect_error(corr_col(x = c(1, 2))) # not a data frame

  # `x` has less than two numeric columns ------------------------------------------

  expect_error(corr_col(x = data.frame()))
  expect_error(corr_col(x = data.frame(values_up)))
  expect_error(corr_col(x = data.frame(values_random)))
  expect_error(corr_col(x = data.frame(values_nonnumeric)))
  expect_error(corr_col(x = data.frame(values_up, values_nonnumeric)))

  # `x` arguments as expected ------------------------------------------

  expect_no_error(corr_col(x = data.frame(
    values_up, values_down, values_nonnumeric
  )))
  expect_no_error(corr_col(x = data.frame(values_up, values_down)))
  expect_no_error(corr_col(x = data.frame(values_up, values_up)))
  expect_no_error(corr_col(x = data.frame(values_up, values_up, values_random)))
  expect_no_error(corr_col(x = tibble(values_up, values_down)))

})

test_that("Result checking for single data frame", {
  expect_equal(corr_col(x = data.frame(values_up, values_up))$rho, 1)
  expect_equal(corr_col(x = data.frame(values_down, values_down))$rho, 1)
  expect_equal(corr_col(x = data.frame(values_up, values_up_na))$rho, 1)
  expect_equal(corr_col(x = data.frame(values_up, values_single))$rho, NA)
  expect_equal(corr_col(x = data.frame(values_up, values_all_na))$rho, NA)
})

# Two data frames `x`, `y` ------------------------------------------

test_that("Argument checking for two data frames", {
  # `y` is not a data frame ------------------------------------------

  expect_error(corr_col(x = data.frame(values_up), y = values_up)) # not a data frame

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

test_that("Result checking for two data frames", {

  # One column ------------------------------------------

  expect_equal(suppressMessages(corr_col(
    x = data.frame(indexes, u1 = values_up),
    y = data.frame(indexes, u2 = values_up)
  )$rho), 1)
  expect_equal(suppressMessages(corr_col(
    x = data.frame(indexes, d1 = values_down),
    y = data.frame(indexes, d2 = values_down)
  )$rho), 1)
  expect_equal(suppressMessages(corr_col(
    x = data.frame(indexes, values_up),
    y = data.frame(indexes, values_down)
  )$rho), -1)
  expect_equal(suppressMessages(corr_col(
    x = data.frame(indexes, values_down),
    y = data.frame(indexes, values_up)
  )$rho), -1)

  # Two columns ------------------------------------------

  # expect_true(suppressMessages(all(
  #   corr_col(
  #     x = data.frame(indexes, u1 = values_up, d1 = values_down),
  #     y = data.frame(indexes, u2 = values_up, d2 = values_down)
  #   )$rho == c(1, -1, -1, 1)
  # )))

  # Missing values ------------------------------------------

  expect_equal(suppressMessages(corr_col(
    x = data.frame(indexes, values_up),
    y = data.frame(indexes, values_up_na)
  )$rho), 1)
  expect_equal(suppressMessages(corr_col(
    x = data.frame(indexes, values_up),
    y = data.frame(indexes, values_dn_na)
  )$rho), -1)
})
