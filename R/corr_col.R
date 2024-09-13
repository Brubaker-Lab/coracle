#' Correlation of columns for tidy data.
#'
#' @param x A tidy data.frame containing numeric data columns.
#' @param y (Optional) A tidy data frame containing numeric data columns for pairwise correlation with `x`.
#' @param ... Unused.
#' @param xy_join (Optional) A join specification for `x` and `y` constructed by \link[dplyr]{join_by}.
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
                     xy_join = NULL,
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
    result <- corr_col_xy(
      x = x,
      y = y,
      xy_join = xy_join,
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
      ungroup() %>%
      as.data.frame()
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
    rename(var_x = ...1, var_y = ...2)

  map2(.x = corr_vars$var_x,
       .y = corr_vars$var_y,
       .f = possibly(\(var_x, var_y) corr_x(var_x, var_y, corr_data, method))) %>%
    list_rbind()

}

#' (Internal)Mapped correlation function for single data frame (`x`) case of `corr_col()`
#'
#' @param var_x A column name of `x` as a character of length 1
#' @param var_y A column name of `x` as a character of length 1
#' @param corr_data The data frame prepared from `x` for correlation
#' @param method (Optional) A character of length 1 indicating which correlation coefficient is to be used: `"spearman"` (the default), `"pearson"`, or `"kendall"`.
#'
#' @return The results of a single correlation as a data frame row.
corr_x <- function(var_x, var_y, corr_data, method) {
  arg_match(method, c("pearson", "kendall", "spearman"))

  tryCatch({
    cor_result <- cor.test(
      x = corr_data[[var_x]],
      y = corr_data[[var_y]],
      method = method,
      exact = FALSE
    )

    return(cbind(
      data.frame(x = var_x, y = var_y),
      statistic(cor_result$estimate, method),
      data.frame(p = cor_result$p.value, message = NA)
    ))

  }, warning = function(cond) {
    return(cbind(
      data.frame(x = var_x, y = var_y),
      statistic(NA, method),
      data.frame(p = NA, message = conditionMessage(cond))
    ))
  }, error = function(cond) {
    return(cbind(
      data.frame(x = var_x, y = var_y),
      statistic(NA, method),
      data.frame(p = NA, message = conditionMessage(cond))
    ))
  })
}


#' (Internal) Two data frame (`x`, `y`) case of `corr_col()`
#'
#' @param x A tidy data frame containing numeric data columns.
#' @param y A tidy data frame containing numeric data columns.
#' @param ... Unused.
#' @param xy_join (Optional) A join specification for `x` and `y` constructed by \link[dplyr]{join_by}.
#' @param x_name (Optional) A character of length 1 to be used as the first column name.
#' @param y_name (Optional) A character of length 1 to be used as the second column name.
#' @param method (Optional) A character of length 1 indicating which correlation coefficient is to be used: `"spearman"` (the default), `"pearson"`, or `"kendall"`.
#'
#' @return A data frame of correlation results.
corr_col_xy <- function(x,
                        y,
                        ...,
                        xy_join = NULL,
                        x_name = "x",
                        y_name = "y",
                        method = "spearman") {
  arg_match(method, c("pearson", "kendall", "spearman"))
  arg_check_data(x, "x")
  arg_check_data(y, "y")

  if (is.null(xy_join)) {
    xy_join$x <- intersect(names(x), names(y))
    xy_join$y <- intersect(names(x), names(y))
  }

  if (is_empty(xy_join$x) | is_empty(xy_join$y))
    cli_abort(
      c("{.arg x} and {.arg y} do not share any columns.", "i" = "Provide a join specification with {.fn dplyr::join_by}.")
    )

  cli({
    cli_alert_info("Joining {.arg x} and {.arg y} by the following columns:")
    if (is.list(xy_join)) {
      cli_ul(paste(dQuote(xy_join$x), dQuote(xy_join$y), sep = " = "))
    } else {
      cli_ul(xy_join$expr)
    }
    cli_alert_warning("Override by providing {.arg xy_join} argument.")
    cli_alert_info(
      "Refer to {.href [documentation for the {.arg by} argument of {.pkg dplyr} mutating joins](https://dplyr.tidyverse.org/reference/mutate-joins.html)}."
    )
  })

  x_data <- x %>%
    select(all_of(xy_join$x), where(is.numeric)) %>%
    nest(.by = xy_join$x, .key = "x_data")
  y_data <- y %>%
    select(all_of(xy_join$y), where(is.numeric)) %>%
    nest(.by = xy_join$y, .key = "y_data")

  corr_data <- inner_join(x_data, y_data, by = xy_join)

  if (nrow(corr_data) == 0)
    cli_abort(c("No observations remain after joining `x` and `y`."))

  x_vars <- names(corr_data$x_data[[1]])
  y_vars <- names(corr_data$y_data[[1]])

  if (length(x_vars) == 0)
    cli_abort(c("No numeric columns from `x` remain after joining `x` and `y`."))
  if (length(y_vars) == 0)
    cli_abort(c("No numeric columns from `y` remain after joining `x` and `y`."))

  corr_vars <- expand_grid(var_x = x_vars, var_y = y_vars)

  map2(.x = corr_vars$var_x,
       .y = corr_vars$var_y,
       .f = possibly(\(var_x, var_y) corr_xy(var_x, var_y, corr_data, method))) %>%
    list_rbind()

}

#' (Internal)Mapped correlation function fort two data frame (`x`, `y`) case of `corr_col()`
#'
#' @param var_x A column name of `x` as a character of length 1
#' @param var_y A column name of `x` as a character of length 1
#' @param corr_data The data frame prepared from `x` for correlation
#' @param method (Optional) A character of length 1 indicating which correlation coefficient is to be used: `"spearman"` (the default), `"pearson"`, or `"kendall"`.
#'
#' @return The results of a single correlation as a data frame row.
corr_xy <- function(var_x, var_y, corr_data, method) {
  arg_match(method, c("pearson", "kendall", "spearman"))

  temp_data <- corr_data %>%
    hoist(x_data, x_col = var_x) %>%
    hoist(y_data, y_col = var_y) %>%
    select(x_col, y_col)

  tryCatch({
    cor_result <- cor.test(
      x = temp_data[["x_col"]],
      y = temp_data[["y_col"]],
      method = method,
      exact = FALSE
    )

    return(cbind(
      data.frame(x = var_x, y = var_y),
      statistic(cor_result$estimate, method),
      data.frame(p = cor_result$p.value, message = NA)
    ))

  }, warning = function(cond) {
    return(cbind(
      data.frame(x = var_x, y = var_y),
      statistic(NA, method),
      data.frame(p = NA, message = conditionMessage(cond))
    ))
  }, error = function(cond) {
    return(cbind(
      data.frame(x = var_x, y = var_y),
      statistic(NA, method),
      data.frame(p = NA, message = conditionMessage(cond))
    ))
  })

}
