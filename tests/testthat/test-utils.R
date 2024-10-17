set.seed(123)

test_that("check_data()", {
  expect_true(exists("check_data")) # Prevents "Empty Test" warning

  f <- check_data

  test_that("expected inputs", {
    expect_no_error(f(data.frame(x = 1:5)))
    expect_no_error(f(tibble(x = 1:5)))
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

test_that("check_numeric_length", {
  expect_true(exists("check_numeric_length")) # Prevents "Empty Test" warning

  f <- check_numeric_length


  test_that("expected inputs", {
    expect_no_error(f(1))
    expect_no_error(f(123))
  })

  test_that("non-numeric inputs", {
    expect_error(f("a"))
    expect_error(f(TRUE))
  })

  test_that("inputs of length > 1", {
    expect_error(f(c(1, 2, 3)))
  })

  test_that("non-numeric inputs of length > 1", {
    expect_error(f(c(T, T, F)))
    expect_error(f(c("a", "b")))
    expect_error(f(c("a", "b", 1)))
  })

})

test_that("check_name", {
  expect_true(exists("check_name")) # Prevents "Empty Test" warning

  f <- check_name


  test_that("expected inputs", {
    expect_no_error(f("a"))
    expect_no_error(f("ABC"))
  })

  test_that("non-string inputs", {
    expect_error(f(1))
    expect_error(f(T))
    expect_error(f(NA))
    expect_error(f(c(1, 2)))
    expect_error(f(c("a", "b")))
    expect_error(f(c("a", 1)))
  })

})

test_that("check_join()", {
  expect_true(exists("check_join")) # Prevents "Empty Test" warning

  f <- check_join

  i <- as.character(1:5)
  i1 <- as.character(1:5)
  i2 <- as.character(1:5)
  a <- 1:5
  b <- 1:5
  c <- 1:5

  test_that("expected inputs", {
    expect_no_error(f(
      x = data.frame(i, a),
      y = data.frame(i, b),
      join = NULL
    ))
    expect_no_error(f(
      x = data.frame(i, a, b),
      y = data.frame(i, a, c),
      join = NULL
    ))
    expect_no_error(f(
      x = data.frame(i, a),
      y = data.frame(i, a),
      join = join_by(i)
    ))
    expect_no_error(f(
      x = data.frame(i1, a),
      y = data.frame(i2, a),
      join = join_by(i1 == i2)
    ))
  })

  test_that("erroneous inputs without join specification", {
    expect_error(f(
      x = data.frame(a),
      y = data.frame(b),
      join = NULL
    ))
    expect_error(f(
      x = data.frame(i, a),
      y = data.frame(i, a, b),
      join = NULL
    ))
    expect_error(f(
      x = data.frame(i, a, b),
      y = data.frame(i, a),
      join = NULL
    ))
    expect_error(f(
      x = data.frame(i, a, b),
      y = data.frame(i, a, b),
      join = NULL
    ))
  })

  test_that("erroneous inputs with join specification", {
    expect_error(f(
      x = data.frame(i, a, c),
      y = data.frame(i, a, b),
      join = join_by(z)
    ))
    expect_error(f(
      x = data.frame(i, a, c),
      y = data.frame(i, a, b),
      join = join_by(b)
    ))
    expect_error(f(
      x = data.frame(i, a, c),
      y = data.frame(i, a, b),
      join = join_by(c)
    ))
    expect_error(f(
      x = data.frame(i, a, c),
      y = data.frame(i, a, b),
      join = join_by(b == b)
    ))
    expect_error(f(
      x = data.frame(i, a, c),
      y = data.frame(i, a, b),
      join = join_by(c == c)
    ))
    expect_error(f(
      x = data.frame(i, a, c),
      y = data.frame(i, a, b),
      join = join_by(i == z)
    ))
    expect_error(f(
      x = data.frame(i, a, c),
      y = data.frame(i, a, b),
      join = join_by(z == i)
    ))
    expect_error(f(
      x = data.frame(i, a, c),
      y = data.frame(i, a, b),
      join = "a"
    ))
  })
})


test_that("statistic_column()", {
  expect_true(exists("statistic_column")) # Prevents "Empty Test" warning

  f <- statistic_column

  test_that("expected inputs", {
    expect_equal(f(1, "spearman") %>% nrow, 1)
    expect_equal(f(1, "spearman") %>% ncol, 1)
    expect_equal(f(1, "spearman") %>% names, "rho")
    expect_equal(f(1, "pearson") %>% names, "cor")
    expect_equal(f(1, "kendall") %>% names, "tau")
    expect_equal(f(1, "spearman")[[1]], 1)
    expect_equal(f(0.123, "spearman")[[1]], 0.123)
    expect_equal(f(NA, "spearman")[[1]], NA)
  })

  test_that("incomplete `method` argument", {
    expect_error(f(1, "s"))
    expect_error(f(1, "p"))
    expect_error(f(1, "k"))

  })

  test_that("`value` argument is not a numeric of length 1", {
    expect_error(f("a", "spearman"))
    expect_error(f(list(1, 2), "spearman"))
    expect_error(f(list(1, 2, "a"), "spearman"))
    expect_error(f(list(1, 2, T), "spearman"))
    expect_error(f(T, "spearman"))
  })
})
