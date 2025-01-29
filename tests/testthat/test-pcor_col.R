set.seed(123)

index <- c("a", "b", "c", "d", "e")
index_alt <- c("d", "e", "f", "g", "h")
up <- c(1, 2, 3, 4, 5)
down <- c(5, 4, 3, 2, 1)
constant <- c(1, 1, 1, 1, 1)
up_with_na <- c(1, 2, 3, NA, 5)
down_with_na <- c(5, NA, 3, 2, 1)
all_na <- as.numeric(c(NA, NA, NA, NA, NA))
all_na_but_one <- c(NA, NA, 3, NA, NA)
nonnumeric <- c("a", "b", "c", "d", "e")
random <- c(runif(5))


run_tests <- function(use_future = NULL) {
  test_that("pcor_col() for three data frames", {
    expect_true(exists("pcor_col")) # Prevents "Empty Test" warning

    f <- pcor_col

    if (use_future) {
      future::plan(future::multisession, workers = 2)
    } else {
      future::plan(future::sequential)
    }

    test_that("expected inputs", {
      expect_no_error(suppressMessages(f(
        x = data.frame(index, up),
        y = data.frame(index, down),
        z = data.frame(index, random)
      )))
    })

    test_that("expected inputs with joining", {
      expect_no_error(suppressMessages(
        f(
          x = data.frame(index, up),
          y = data.frame(index, up),
          z = data.frame(index, up),
          xy_join = join_by(index),
          xz_join = join_by(index)
        )
      )) # Join specification for columns with the same name
      expect_no_error(suppressMessages(
        f(
          x = data.frame(i1 = index, up),
          y = data.frame(i2 = index, up),
          z = data.frame(i3 = index, up),
          xy_join = join_by(i1 == i2),
          xz_join = join_by(i1 == i3)
        )
      )) # Join specification for columns which don't have the same name
      expect_no_error(suppressMessages(
        f(
          x = data.frame(i1 = index, index, up),
          y = data.frame(i2 = index, index, up),
          z = data.frame(i3 = index, up),
          xy_join = join_by(i1 == i2),
          xz_join = join_by(i1 == i3)
        )
      )) # Join specification for columns which don't have the same name, excluding columns with the same name
    })

    test_that("expected outputs snapshots", {
      data <- mtcars %>% rownames_to_column("name")

      expect_snapshot(suppressMessages(f(
        x = data[, c("name", "mpg")], y = data[, c("name", "disp", "hp")], z = data[, c("name", "wt", "cyl")]
      )))
      expect_snapshot(ppcor::pcor.test(
        x = data$mpg,
        y = data$disp,
        z = data[, c("wt", "cyl")],
        method = "spearman"
      ))
      expect_snapshot(ppcor::pcor.test(
        x = data$mpg,
        y = data$hp,
        z = data[, c("wt", "cyl")],
        method = "spearman"
      ))


    })



    test_that("erroneous inputs", {
      # Missing input data

      expect_error(suppressMessages(f(
        y = data.frame(index, up), z = data.frame(index, up)
      )))
      expect_error(suppressMessages(f(
        x = data.frame(index, up), z = data.frame(index, up)
      )))
      expect_error(suppressMessages(f(
        x = data.frame(index, up), y = data.frame(index, up)
      )))

      # Non-data frame inputs

      expect_error(suppressMessages(f(
        x = "a",
        y = data.frame(index, up),
        z = data.frame(index, down)
      )))
      expect_error(suppressMessages(f(
        x = data.frame(index, up),
        y = "a",
        z = data.frame(index, down)
      )))
      expect_error(suppressMessages(f(
        x = data.frame(index, up),
        y = data.frame(index, down),
        z = "a"
      )))

      # Non-string name inputs

      expect_error(suppressMessages(
        f(
          x = data.frame(index, up),
          y = data.frame(index, down),
          z = data.frame(index, random),
          x_name = 1
        )
      ))
      expect_error(suppressMessages(
        f(
          x = data.frame(index, up),
          y = data.frame(index, down),
          z = data.frame(index, random),
          y_name = 1
        )
      ))
      expect_error(suppressMessages(
        f(
          x = data.frame(index, up),
          y = data.frame(index, down),
          z = data.frame(index, random),
          z_name = 1
        )
      ))

      # Non-join_by inputs

      expect_error(suppressMessages(
        f(
          x = data.frame(index, up),
          y = data.frame(index, down),
          z = data.frame(index, random),
          xy_join = 1
        )
      ))
      expect_error(suppressMessages(
        f(
          x = data.frame(index, up),
          y = data.frame(index, down),
          z = data.frame(index, random),
          xz_join = 1
        )
      ))

      # Incorrect method inputs

      expect_error(suppressMessages(
        f(
          x = data.frame(index, up),
          y = data.frame(index, down),
          z = data.frame(index, random),
          method = 1
        )
      ))
      expect_error(suppressMessages(
        f(
          x = data.frame(index, up),
          y = data.frame(index, down),
          z = data.frame(index, random),
          method = "s"
        )
      ))
    })

    test_that("erroneous inputs for joining", {
      expect_error(suppressMessages(f(
        x = data.frame(up),
        y = data.frame(up),
        z = data.frame(up)
      ))) # All columns used for joining
      expect_error(suppressMessages(f(
        x = data.frame(index, up),
        y = data.frame(index, up, down),
        z = data.frame(index, up, random)
      ))) # All columns in `x` are used for joining
      expect_error(suppressMessages(f(
        x = data.frame(index, up, down),
        y = data.frame(index, up),
        z = data.frame(index, up, random)
      ))) # All columns in `y` are used for joining
      expect_error(suppressMessages(f(
        x = data.frame(index, up, down),
        y = data.frame(index, up, random),
        z = data.frame(index)
      ))) # All columns in `z` are used for joining
      expect_error(suppressMessages(
        f(
          x = data.frame(index, up),
          y = data.frame(index, down),
          z = data.frame(index, random),
          xy_join = join_by(index, not_a_col),
          xz_join = join_by(index)
        )
      )) # Join refers to non-existent column(s)
      expect_error(suppressMessages(
        f(
          x = data.frame(index, up),
          y = data.frame(index, down),
          z = data.frame(index, random),
          xy_join = join_by(index),
          xz_join = join_by(index, not_a_col)
        )
      )) # Join refers to non-existent column(s)
      expect_error(suppressMessages(
        f(
          x = data.frame(index, up),
          y = data.frame(index, down),
          z = data.frame(index, random),
          xy_join = join_by(index == not_a_col),
          xz_join = join_by(index)
        )
      )) # Join refers to non-existent column(s)
      expect_error(suppressMessages(
        f(
          x = data.frame(index, up),
          y = data.frame(index, down),
          z = data.frame(index, random),
          xy_join = join_by(index),
          xz_join = join_by(index == not_a_col)
        )
      )) # Join refers to non-existent column(s)
    })
  })

}

run_tests(use_future = FALSE)
run_tests(use_future = TRUE)
