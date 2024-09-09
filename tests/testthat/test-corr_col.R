indexes <- c("a", "b", "c", "d", "e")
indexes_long <- c("c", "d", "e", "f", "g")
values_up <- c(1, 2, 3, 4, 5)
values_down <- c(5, 4, 3, 2, 1)
values_constant <- c(1, 1, 1, 1, 1)
values_up_na <- c(1, 2, 3, NA, 5)
values_dn_na <- c(5, NA, 3, 2, 1)
values_all_na <- as.numeric(c(NA, NA, NA, NA, NA))
values_single <- c(NA, NA, 3, NA, NA)
values_nonnumeric <- c("a", "b", "c", "d", "e")
values_random <- c(runif(5))


test_that("Arguments checked for validity",{
  expect_error(corr_col(x = "Not a data frame",
                        y = tibble(i = indexes, v = values_up),
                        x_name = "x_name",
                        y_name = "y_name"))
  expect_error(corr_col(x = tibble(i = indexes, v = values_up),
                        y = "Not a data frame",
                        x_name = "x_name",
                        y_name = "y_name"))
  expect_error(corr_col(x = tibble(i = indexes, v = values_up),
                        x = tibble(i = indexes, v = values_up),
                        #x_name = "x_name",
                        y_name = "y_name"))
  expect_error(corr_col(x = tibble(i = indexes, v = values_up),
                        x = tibble(i = indexes, v = values_up),
                        x_name = "x_name"#,
                        #y_name = "y_name"
                        ))
  expect_error(corr_col(x = tibble(i = indexes, n = values_nonnumeric),
                        x = tibble(i = indexes, v = values_up),
                        x_name = "x_name",
                        y_name = "y_name"
  ))
  expect_error(corr_col(x = tibble(i = indexes, v = values_up),
                        x = tibble(i = indexes, n = values_nonnumeric),
                        x_name = "x_name",
                        y_name = "y_name"
  ))
})
