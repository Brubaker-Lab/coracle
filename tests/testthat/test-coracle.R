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

test_that("coracle", {
  expect_true(exists("coracle")) # Prevents "Empty Test" warning

  f <- suppressMessages(coracle)

  cdo1 <- coracle_data$new(df1, grps = mbio, join = gene, vals = -c(mbio,gene), labl_cols = "column1")
  cdo2 <- coracle_data$new(df2, grps = drug, join = gene, vals = -c(drug,gene), labl_cols = "column2")

  coracle(cdo1,cdo2)


})
