set.seed(123)

index <- c("a", "b", "c", "d", "e")
up <- c(1, 2, 3, 4, 5)
down <- c(5, 4, 3, 2, 1)
constant <- c(1, 1, 1, 1, 1)
up_with_na <- c(1, 2, 3, NA, 5)
down_with_na <- c(5, NA, 3, 2, 1)
all_na <- as.numeric(c(NA, NA, NA, NA, NA))
all_na_but_one <- c(NA, NA, 3, NA, NA)
nonnumeric <- c("a", "b", "c", "d", "e")
random <- c(runif(5))

test_that("corr_col() for one data frame", {
  expect_true(exists("corr_col")) # Prevents "Empty Test" warning

  f <- corr_col

  test_that("expected inputs throw no errors", {
    expect_no_error(suppressMessages(f(x = data.frame(
      up, down, nonnumeric
    ))))
    expect_no_error(suppressMessages(f(x = data.frame(up, down))))
    expect_no_error(suppressMessages(f(x = data.frame(up, up))))
    expect_no_error(suppressMessages(f(x = data.frame(up, up, random))))
    expect_no_error(suppressMessages(f(x = tibble(up, down))))
  })
  test_that("expected inputs give correct results", {
    expect_equal(suppressMessages(f(x = data.frame(up, up)))$rho, 1)
    expect_equal(suppressMessages(f(x = data.frame(down, down)))$rho, 1)
    expect_equal(suppressMessages(f(x = data.frame(up, up_with_na)))$rho, 1)
    expect_equal(suppressMessages(f(x = data.frame(up, up_with_na)))$n, 4)
    expect_equal(suppressMessages(f(x = data.frame(
      up, all_na_but_one
    )))$rho, NA)
    expect_equal(suppressMessages(f(x = data.frame(up, all_na)))$rho, NA)
  })
  test_that("expected inputs snapshots", {
    expect_snapshot(suppressMessages(
      f(
        x = data.frame(index, up, down, random, constant, nonnumeric, all_na),
        y = data.frame(index, up, down, random, constant, nonnumeric, all_na),
        x_name = "first",
        y_name = "second",
        xy_join = join_by(index)
      )
    ))

  })
  test_that("erroneous inputs", {
    # Not data frames

    expect_error(suppressMessages(f(x = "1"))) # not a data frame
    expect_error(suppressMessages(f(x = 1))) # not a data frame
    expect_error(suppressMessages(f(x = c(1, 2)))) # not a data frame

    # Less than two numeric columns

    expect_error(suppressMessages(f(x = data.frame())))
    expect_error(suppressMessages(f(x = data.frame(up))))
    expect_error(suppressMessages(f(x = data.frame(random))))
    expect_error(suppressMessages(f(x = data.frame(nonnumeric))))
    expect_error(suppressMessages(f(x = data.frame(up, nonnumeric))))
  })
})

test_that("corr_col() for two data frames", {
  expect_true(exists("corr_col")) # Prevents "Empty Test" warning

  f <- corr_col

  test_that("expected inputs for joining", {
    expect_no_error(suppressMessages(f(
      x = data.frame(index, up),
      y = data.frame(index, up),
      xy_join = join_by(index)
    ))) # Join specification for columns with the same name
    expect_no_error(suppressMessages(f(
      x = data.frame(i1 = index, up),
      y = data.frame(i2 = index, up),
      xy_join = join_by(i1 == i2)
    ))) # Join specification for columns which don't have the same name
    expect_no_error(suppressMessages(f(
      x = data.frame(i1 = index, index, up),
      y = data.frame(i2 = index, index, up),
      xy_join = join_by(i1 == i2)
    ))) # Join specification for columns which don't have the same name, excluding columns with the same name

  })

  test_that("expected inputs give correct results", {
    expect_equal(suppressMessages(f(
      x = data.frame(index, u1 = up), y = data.frame(index, u2 = up)
    ))$rho, 1)
    expect_equal(suppressMessages(f(
      x = data.frame(index, d1 = down),
      y = data.frame(index, d2 = down)
    ))$rho, 1)
    expect_equal(suppressMessages(f(
      x = data.frame(index, up), y = data.frame(index, down)
    ))$rho, -1)
    expect_equal(suppressMessages(f(
      x = data.frame(index, down), y = data.frame(index, up)
    ))$rho, -1)
    expect_equal(suppressMessages(f(
      x = data.frame(index, up), y = data.frame(index, up_with_na)
    ))$rho, 1)
    expect_equal(suppressMessages(f(
      x = data.frame(index, up),
      y = data.frame(index, down_with_na)
    ))$rho, -1)
  })

  test_that("expected inputs snapshots", {
    expect_snapshot(suppressMessages(f(
      x = data.frame(index, up, down, random, nonnumeric)
    )))
  })

  test_that("erroneous inputs for joining", {
    expect_error(suppressMessages(f(x = data.frame(up), y = data.frame(up)))) # Only column used for joining
    expect_error(suppressMessages(f(
      x = data.frame(indexes, up), y = data.frame(indexes, up, down)
    ))) # All columns in x used for joining
    expect_error(suppressMessages(f(
      x = data.frame(indexes, up, down), y = data.frame(indexes, up)
    ))) # All columns in y used for joining
  })

  test_that("erroneous inputs", {
    expect_error(suppressMessages(f(x = data.frame(up), y = up))) # not a data frame

  })

})
