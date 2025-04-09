# Test function ------------

run_tests <- function(use_future = FALSE) {
  test_that("coracle_data.R", {
    expect_true(exists("coracle_data")) # Prevent empty test warning

    f <- coracle_data

    if(use_future){
      plan(multisession, workers = 4)
    } else {
      plan(sequential)
    }

    expect_no_error(f$new(
      data = df_long,
      grps = g,
      join = j,
      vals = random_up
    ))
    expect_no_error(
      f$new(
        data = df_wide,
        grps = NULL,
        join = j,
        vals = where(is.numeric),
        labl_cols = "g",
        labl_vals = "vals"
      )
    )

    expect_error(f$new(
      data = c(1, 2, 3),
      # not a data.frame
      grps = g,
      join = j,
      vals = random_up
    ))
    expect_error(f$new(
      data = df_long,
      grps = abcd,
      # not in data
      join = j,
      vals = random_up
    ))
    expect_error(f$new(
      data = df_long,
      grps = g,
      join = abcd,
      # not in data
      vals = random_up
    ))
    expect_error(f$new(
      data = df_wide,
      grps = g,
      join = c(j, up),
      # more than one
      vals = random_up
    ))
    expect_error(f$new(
      data = df_long,
      grps = g,
      join = j,
      vals = abcd # not in data
    ))
    expect_error(f$new(
      data = df_long,
      grps = g,
      # overlap
      join = g,
      # overlap
      vals = up
    ))
    expect_error(f$new(
      data = df_long,
      grps = g,
      join = j,
      # overlap
      vals = j # overlap
    ))
    expect_error(f$new(
      data = df_long,
      grps = g,
      # overlap
      join = j,
      vals = g # overlap
    ))

    cdo_long <- f$new(
      data = df_long,
      grps = g,
      join = j,
      vals = up
    )

    expect_equal(nrow(cdo_long$data), nrow(df_long))
    expect_equal(ncol(cdo_long$data), ncol(df_long))
    expect_equal(cdo_long$data, df_long)
    expect_equal(length(cdo_long$leaves), 27) # LETTERS[1:26] and NA
    expect_equal(length(cdo_long$children), 27) # LETTERS[1:26] and NA
    expect_equal(length(cdo_long$leaves_valid), 27)
    expect_equal(length(cdo_long$leaves_invalid), 0)

    input_labl_cols <- "g"
    input_labl_vals <- "v"

    cdo_wide <- coracle_data$new(
      data = df_wide,
      grps = NULL,
      join = j,
      vals = where(is.numeric)
    )

    expect_equal(
      nrow(cdo_wide$data),
      length(unique(df_wide$j)) *
        df_wide |> select(where(is.numeric)) |> names() |> length()
    )
    expect_equal(ncol(cdo_wide$data), 3)
    expect_equal(length(cdo_wide$leaves), 6) # up:random_up
    expect_equal(length(cdo_wide$leaves_valid), 5)
    expect_equal(length(cdo_wide$leaves_invalid), 1) # const

  })

  plan(sequential)
}

run_tests(use_future = F)
run_tests(use_future = T)

