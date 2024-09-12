#' Correlation of columns for tidy data.
#'
#' @param x A tidy data.frame containing numeric data columns.
#' @param y (Optional) A tidy data frame containing numeric data columns for pairwaise correlation with `x`.
#' @param ... Unused.
#' @param xy_join_vars (Optional) A character vector of variables to join `x` and `y`. Refer to the `by` argument of `dplyr::inner_join` for details.
#' @param x_name (Optional) A character of length 1 to be used as the first column name.
#' @param y_name (Optional) A character of length 1 to be used as the second column name.
#' @param method (Optional) A character of length 1 indicating which correlation coefficient is to be used: `"spearman"` (the default), `"pearson"`, or `"kendall"`.
#'
#' @return A data frame of correlation results.
#' @export
#'
#' @importFrom stringr str_replace
corr_col <- function(x,
                     y = NULL,
                     ...,
                     xy_join_vars = NULL,
                     x_name = "x",
                     y_name = "y",
                     method = "spearman") {
  arg_match(method, c("pearson", "kendall", "spearman"))

  # Two cases: x, xy

  if (is.null(y)) {
    result <- corr_col_x(x = x,
                         x_name = x_name,
                         method = method) %>%
      mutate(q = p.adjust(p, method = "fdr"))
  } else {
    cli_abort(c(
      "Correlation with a second {.cls data.frame} {.var y} has not been implemented yet!"
    ))

    result <- corr_col_xy(
      x = x,
      y = y,
      xy_join_vars = xy_join_vars,
      x_name = x_name,
      y_name = y_name,
      method = method
    ) %>%
      mutate(q = p.adjust(p, method = "fdr")) %>%
      group_by(x) %>%
      mutate(q_x = p.adjust(p, method = "fdr")) %>%
      ungroup() %>%
      group_by(y) %>%
      mutate(q_y = p.adjust(p, method = "fdr")) %>%
      ungroup()
  }

  result %>%
    rename_with(
      .fn = str_replace,
      .cols = contains("x"),
      pattern = "x$",
      replacement = x_name
    ) %>%
    rename_with(
      .fn = str_replace,
      .cols = contains("y"),
      pattern = "y$",
      replacement = y_name
    ) %>%
    remove_rownames()

}

#' (Internal) Single data frame (`x`) case of `corr_col()`
#'
#' @param x A tidy data frame containing numeric data columns.
#' @param x_name (Optional) A character of length 1 to be used as the first column name.
#' @param ... Unused.
#' @param method (Optional) A character of length 1 indicating which correlation coefficient is to be used: `"spearman"` (the default), `"pearson"`, or `"kendall"`.
#'
#' @return A data frame of correlation results.
corr_col_x <- function(x, x_name, ..., method = "spearman") {
  arg_match(method, c("pearson", "kendall", "spearman"))
  arg_check_data(x, "x")

  corr_data <- x %>%
    select(where(is.numeric))

  if (ncol(corr_data) < 2)
    cli_abort(c("{.var x} must have at least two numeric columns."))

  corr_vars <- corr_data %>%
    names() %>%
    combn(x = ., m = 2) %>%
    t() %>%
    as_tibble(.name_repair = ~ vctrs::vec_as_names(..., repair = "unique", quiet = TRUE)) %>% # `vec_as_names` fn suppresses messages from `as_tibble()`
    rename(var_1 = ...1, var_2 = ...2)

  map2(.x = corr_vars$var_1,
       .y = corr_vars$var_2,
       .f = possibly(\(x, y) corr_x(x, y, corr_data, method))) %>%
    list_rbind()

}

#' (Internal)Mapped correlation function for single data frame (`x`) case of `corr_col()`
#'
#' @param x A column name of `x` as a character of length 1
#' @param y A column name of `x` as a character of length 1
#' @param corr_data The data frame prepared from `x` for correlation
#' @param method (Optional) A character of length 1 indicating which correlation coefficient is to be used: `"spearman"` (the default), `"pearson"`, or `"kendall"`.
#'
#' @return The results of a single correlation as a data frame row.
corr_x <- function(x, y, corr_data, method) {
  arg_match(method, c("pearson", "kendall", "spearman"))

  tryCatch({
    cor_result <- cor.test(
      x = corr_data[[x]],
      y = corr_data[[y]],
      method = method,
      exact = FALSE
    )

    return(cbind(
      data.frame(x = x, y = y),
      statistic(cor_result$estimate, method),
      data.frame(p = cor_result$p.value, error = NA)
    ))

  }, warning = function(cond) {
    return(cbind(
      data.frame(x = x, y = y),
      statistic(NA, method),
      data.frame(p = NA, error = conditionMessage(cond))
    ))
  }, error = function(cond) {
    return(cbind(
      data.frame(x = x, y = y),
      statistic(NA, method),
      data.frame(p = NA, error = conditionMessage(cond))
    ))
  })
}


#' (Internal) Two data frame (`x`, `y`) case of `corr_col()`
#'
#' @param x A tidy data frame containing numeric data columns.
#' @param y A tidy data frame containing numeric data columns.
#' @param ... Unused.
#' @param xy_join_vars (Optional) A character vector of variables to join `x` and `y`. Refer to the `by` argument of `dplyr::inner_join` for details.
#' @param x_name (Optional) A character of length 1 to be used as the first column name.
#' @param y_name (Optional) A character of length 1 to be used as the second column name.
#' @param method (Optional) A character of length 1 indicating which correlation coefficient is to be used: `"spearman"` (the default), `"pearson"`, or `"kendall"`.
#'
#' @return A data frame of correlation results.
corr_col_xy <- function(x,
                        y,
                        ...,
                        xy_join_vars = NULL,
                        x_name = "x",
                        y_name = "y",
                        method = "spearman") {
  arg_match(method, c("pearson", "kendall", "spearman"))
  arg_check_data(x, "x")
  arg_check_data(y, "y")
}
