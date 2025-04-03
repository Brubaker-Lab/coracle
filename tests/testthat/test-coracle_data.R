df1 <- tidyr::expand_grid(mbio = LETTERS[1:5], gene = letters[1:26]) %>%
  dplyr::mutate(
    up = dplyr::row_number(),
    down = nrow(.) - dplyr::row_number(),
    random = runif(nrow(.)),
    const = 3
  ) |>
  dplyr::rowwise() |>
  dplyr::mutate(random_up = rnorm(1, mean = up),
         random_down = rnorm(1, mean = down)) |>
  dplyr::ungroup()


df2 <- tidyr::expand_grid(drug = LETTERS[22:26], gene = letters[1:26]) %>%
  dplyr::mutate(
    up = dplyr::row_number(),
    down = nrow(.) - dplyr::row_number(),
    random = runif(nrow(.)),
    const = 3
  ) |>
  dplyr::rowwise() |>
  dplyr::mutate(random_up = runif(1, max = up),
         random_down = runif(1, max = down)) |>
  dplyr::ungroup()

test_that("coracle_data.R", {
  expect_true(exists("coracle_data")) # Prevent empty test warning

  f <- suppressMessages(coracle_data)

  test_that("Initializaton", {
    expect_true(exists("f")) # Prevent empty test warning

    test_that("Erroneous Inputs", {
      expect_true(exists("f")) # Prevent empty test warning

      test_that("Arg `data` is a data.frame", {
        expect_snapshot(f$new(NULL), error = T)
        expect_snapshot(f$new(1), error = T)
        expect_snapshot(f$new(list(1, 2, 3)), error = T)

      })

      test_that("Arg `join` selects exactly one column", {
        expect_snapshot(f$new(mtcars, join = NULL, disp, mpg), error = T) # No join
        expect_snapshot(f$new(mtcars, join = c(cyl, drat), disp, mpg), error = T) # Multiple joins
        expect_snapshot(f$new(mtcars, join = abcd, disp, mpg), error = T) # join not in data

      })


      test_that("Arg `vals` selects one or more column(s)", {
        expect_snapshot(f$new(mtcars, cyl, vals = NULL, mpg), error = T) # No vals
        expect_snapshot(f$new(mtcars, cyl, vals = abcd, mpg), error = T) # vals not in data
      })

      test_that("Arg `grps` selects one or more column(s)", {
        expect_snapshot(f$new(mtcars, cyl, disp, grps = NULL), error = T) # No grps
        expect_snapshot(f$new(mtcars, cyl, disp, grps = abcd), error = T) # grps not in data
      })



    })

  })

})
