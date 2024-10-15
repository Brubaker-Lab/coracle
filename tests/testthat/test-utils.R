set.seed(123)

test_that("check_data()", {
  expect_true(exists("check_data")) # Prevents "Empty Test" warning

  f <- check_data

  test_that("basic inputs", {
    expect_no_error(f(data.frame(x = 1:5)))
    expect_no_error(f(tibble(x = 1:5)))
    expect_no_error(f(data.table::data.table(x = 1:5)))
    expect_no_error(f(mtcars))
  })

  test_that("non-data frame input", {
    expect_error(f_df("a"))
    expect_error(f(1))
    expect_error(f(list("a", "b")))
    expect_error(f(list(1, 2)))
  })

  test_that("no numeric data in input", {
    expect_error(f(data.frame(
      x = c("a"), y = as.character(1:5)
    )))
    expect_error(f(tibble(
      x = c("a"), y = as.character(1:5)
    )))
    expect_error(f(data.table(
      x = c("a"), y = as.character(1:5)
    )))
    expect_error(f(data.frame(
      x = c(TRUE), y = as.character(1:5)
    )))
    expect_error(f(data.frame(x = c(NA), y = as.character(1:5))))
  })
  test_that("too few observations/rows in input", {
    expect_error(f(data.frame(x = c(1))))
    expect_error(f(data.frame(x = 1:2)))
    expect_no_error(f(data.frame(x = 1:3)))
    expect_no_error(f(data.frame(x = 1:100)))
  })
})


test_that("statistic()", {
  expect_true(exists("statistic")) # Prevents "Empty Test" warning

  f <- statistic

  test_that("basic inputs", {
    expect_equal(statistic(1, "spearman") %>% nrow, 1)
    expect_equal(statistic(1, "spearman") %>% ncol, 1)
    expect_equal(statistic(1, "spearman") %>% names, "rho")
    expect_equal(statistic(1, "pearson") %>% names, "cor")
    expect_equal(statistic(1, "kendall") %>% names, "tau")
    expect_equal(statistic(1, "spearman")[[1]], 1)
    expect_equal(statistic(0.123, "spearman")[[1]], 0.123)
    expect_equal(statistic(NA, "spearman")[[1]], NA)
  })

    test_that("incomplete `method` argument", {
      expect_error(statistic(1, "s"))
      expect_error(statistic(1, "p"))
      expect_error(statistic(1, "k"))

    })
    test_that("`value` argument is not a numeric of length 1", {
      expect_error(statistic("a", "spearman"))
      expect_error(statistic(NA, "spearman"))
      expect_error(statistic(list(1, 2), "spearman"))
    })
})
