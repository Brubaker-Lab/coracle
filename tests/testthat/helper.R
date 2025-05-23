# Packages ------------

library(future)

# Test data ------------

set.seed(123)

df_wide <- data.frame(j = c(letters[1:26], NA)) %>%
  dplyr::mutate(
    up = dplyr::row_number(),
    down = nrow(.) - dplyr::row_number(),
    random = runif(nrow(.)),
    const = 3
  ) |>
  dplyr::rowwise() |>
  dplyr::mutate(random_up = rnorm(1, mean = up),
                random_down = rnorm(1, mean = down)) |>
  dplyr::ungroup() %>%
  dplyr::mutate(across(up:random_down, \(x) ifelse(runif(length(
    x
  )) > 0.1, x, NA)))

df_long <- tidyr::expand_grid(g1 = c(LETTERS[1:26], NA), g2 = c(LETTERS[1:26], NA), j = c(letters[1:26], NA)) %>%
  dplyr::mutate(
    up = dplyr::row_number(),
    down = nrow(.) - dplyr::row_number(),
    random = runif(nrow(.)),
    const = 3
  ) |>
  dplyr::rowwise() |>
  dplyr::mutate(random_up = rnorm(1, mean = up),
                random_down = rnorm(1, mean = down)) |>
  dplyr::ungroup() %>%
  dplyr::mutate(across(everything(), \(x) ifelse(runif(length(
    x
  )) > 0.1, x, NA)))

# df_big <- tidyr::expand_grid(g = 1:1000, j = 1:10000)%>%
#   dplyr::mutate(
#     up = dplyr::row_number(),
#     down = nrow(.) - dplyr::row_number(),
#     random = runif(nrow(.)),
#     const = 3
#   ) |>
#   dplyr::rowwise() |>
#   dplyr::mutate(random_up = rnorm(1, mean = up),
#                 random_down = rnorm(1, mean = down)) |>
#   dplyr::ungroup() %>%
#   dplyr::mutate(across(up:random_down, \(x) ifelse(runif(length(
#     x
#   )) > 0.1, x, NA)))

