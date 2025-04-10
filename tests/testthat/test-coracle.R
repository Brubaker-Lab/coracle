
test_that("coracle", {
  expect_true(exists("coracle")) # Prevents "Empty Test" warning

  f <- suppressMessages(coracle)

  cdo_1 <- coracle_data$new(df_wide, join = j, vals = where(is.numeric), labl_cols = "cols_1")
  cdo_2 <- coracle_data$new(df_wide, join = j, vals = where(is.numeric), labl_cols = "cols_2")

  f(cdo_1, cdo_2)

  expect_no_error(f(cdo_1, cdo_2))
  expect_snapshot(f(cdo_1, cdo_2)$data)
  expect_snapshot(f(cdo_1, cdo_2)$data |> str())

})
