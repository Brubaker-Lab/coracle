library(tidyverse)

df1 <- expand_grid(mbio = LETTERS[1:5], gene = letters[1:26]) %>%
  mutate(
    up = row_number(),
    down = nrow(.) - row_number(),
    random = runif(nrow(.))
  ) |>
  rowwise() |>
  mutate(random_up = runif(1, max = up),
         random_down = runif(1, max = down)) |>
  ungroup()


df2 <- expand_grid(drug = LETTERS[22:26], gene = letters[1:26]) %>%
  mutate(
    up = row_number(),
    down = nrow(.) - row_number(),
    random = runif(nrow(.))
  ) |>
  rowwise() |>
  mutate(random_up = runif(1, max = up),
         random_down = runif(1, max = down)) |>
  ungroup()

test_that("coracle_data.R", {
  expect_true(exists("coracle_data"))

  f <- coracle_data

  test_that("Initializaton", {
      expect_error(f$new(NULL))
      expect_error(f$new(1))
      expect_error(f$new(list(1, 2, 3)))
      expect_error(f$new(mtcars, join = NULL, disp, mpg))
      expect_error(f$new(mtcars, cyl, vars = NULL, mpg))
      expect_error(f$new(mtcars, cyl, disp, NULL))
  })

})
