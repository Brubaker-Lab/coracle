i <- 60

df_wide <- tibble(
  n = sample(LETTERS[1:5], i, replace = T),
  j = sample(letters[1:5], i, replace = T),
  v1 = 1:i,
  v2 = i:1
)

df_wide_n <- df_wide |> mutate(n = as_name(n))
df_wide_j <- df_wide |> mutate(j = as_join(j))
df_wide_v <- df_wide |> mutate(v1 = as_vals(v1), v2 = as_vals(v2))
df_wide_nj <- df_wide |> mutate(n = as_name(n)) |> mutate(j = as_join(j))
df_wide_n_v <- df_wide |> mutate(n = as_name(n)) |> mutate(v1 = as_vals(v1), v2 = as_vals(v2))
df_wide_jv <- df_wide |> mutate(j = as_join(j)) |> mutate(v1 = as_vals(v1), v2 = as_vals(v2))
df_wide_njv <- df_wide |> mutate(n = as_name(n)) |> mutate(j = as_join(j)) |> mutate(v1 = as_vals(v1), v2 = as_vals(v2))

df_wide_2n <- df_wide |> mutate(n = as_name(n)) |> mutate(j = as_name(j))
df_wide_2j <- df_wide |> mutate(n = as_join(n)) |> mutate(j = as_join(j))

df_long <- tibble(n = sample(LETTERS[1:5], i, replace = T),
                  j = sample(letters[1:5], i, replace = T),
                  v = 1:i,)

df_long_n <- df_long |> mutate(n = as_name(n))
df_long_j <- df_long |> mutate(j = as_join(j))
df_long_v <- df_long |> mutate(v = as_vals(v)) |> mutate(v = as_vals(v))
df_long_nj <- df_long |> mutate(n = as_name(n)) |> mutate(j = as_join(j))
df_long_nv <- df_long |> mutate(n = as_name(n)) |> mutate(v = as_vals(v))
df_long_jv <- df_long |> mutate(j = as_join(j)) |> mutate(v = as_vals(v))
df_long_njv <- df_long |> mutate(n = as_name(n)) |> mutate(j = as_join(j)) |> mutate(v = as_vals(v))

test_that("validate.R", {
  # Prevents "Empty Test" warning
  expect_true(exists("validate_inputs"))

  test_that("validate_inputs", {

    # Prevents "Empty Test" warning
    expect_true(exists("validate_inputs"))

        f <- function(d = NULL,
                  n = NULL,
                  j = NULL,
                  v = NULL) {
      validate_inputs(d, n, j, v)
    }

    test_that("wide inputs", {

      # No join input

      expect_error(df_wide |> f())
      expect_error(df_wide_n |> f())
      expect_error(df_wide_n_v |> f())
      expect_error(df_wide_v |> f())

      # Ambiguous annotations

      expect_error(df_wide_2n |> f())
      expect_error(df_wide_2j |> f())

      # Correct inputs

      expect_no_error(df_wide_j |> f())
      expect_no_error(df_wide_nj |> f())
      expect_no_error(df_wide_jv |> f())
      expect_no_error(df_wide_njv |> f())

    })

    test_that("long inputs", {
      # No join input

      expect_error(df_long |> f())
      expect_error(df_long_n |> f())
      expect_error(df_long_n_v |> f())
      expect_error(df_long_v |> f())

      # No name input

      expect_error(df_long_j |> f())
      expect_error(df_long_jv |> f())

      # Ambiguous annotations

      expect_error(df_long_2n |> f())
      expect_error(df_long_2j |> f())

      # Correct inputs

      expect_no_error(df_long_nj |> f())
      expect_no_error(df_long_njv |> f())

    })
  })
})

