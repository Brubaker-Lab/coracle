library(tidyverse)

df <- expand_grid(loc = c("gut", "vagina", "brain"), mbio = c("lac. cr.", "lac. in.", "gard. va."), gene = letters[1:26]) |>
  rowwise() |>
  mutate(rho = rnorm(1)) |>
  ungroup()

df_long <- expand_grid(g1 = LETTERS[1:3], g2 = LETTERS[24:26], j = letters[1:10]) |>
  mutate(v = row_number())

df_long_grp <- df_long |> group_by(g1,g2)

df_wide <- df_long |> pivot_wider(names_from = g2,values_from = v)

df_wide_grp <- df_wide |> group_by(g1)

test_that("coracle_obj.R", {

  expect_true(exists("coracle_obj")) # Prevents "Empty Test" warning

  f <- coracle_obj

  test_that("Expected inputs", {

    expect_no_error(f$new(df_long, "g1", "j", "v"))
    expect_no_error(f$new(df_long, c("g1", "g2"), "j", "v"))

  })
})

