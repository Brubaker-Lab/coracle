#' Correlation of columns for tidy data.
#'
#' @param x A tidy data.frame containing numeric data columns.
#' @param y (Optional) A tidy data frame containing numeric data columns for pairwise correlation with `x`.
#' @param ... Unused.
#' @param xy_join (Optional) A \code{\link[dplyr]{join_by}} join specification for `x` and `y`.
#' @param x_name (Optional) A character of length 1 to be used as the first column name.
#' @param y_name (Optional) A character of length 1 to be used as the second column name.
#' @param method (Optional) A character of length 1 indicating which correlation coefficient is to be used: `"spearman"` (the default), `"pearson"`, or `"kendall"`.
#'
#' @return A data frame of correlation results.
#' @export
#' @examples
#'
#' # Single data frame
#' x <- data.frame(i = as.character(1:5), ux = 1:5, dx = 5:1)
#' corr_col(x)
#'
#' # Data frames with a shared column, "i"
#' x <- data.frame(i = as.character(1:5), ux = 1:5, dx = 5:1)
#' y <- data.frame(i = as.character(1:5), uy = 1:5, dy = 5:1)
#' corr_col(x, y)
#'
#' # Data frames without a shared column
#' x <- data.frame(ix= as.character(1:5), ux = 1:5, dx = 5:1)
#' y <- data.frame(iy = as.character(1:5), uy = 1:5, dy = 5:1)
#' corr_col(x, y, xy_join = dplyr::join_by(ix == iy))
#'
#' # Renaming the outputs
#' x <- data.frame(i= as.character(1:5), ux = 1:5, dx = 5:1)
#' y <- data.frame(i = as.character(1:5), uy = 1:5, dy = 5:1)
#' corr_col(x,y, x_name = "first", y_name = "second")
#'
corr_col <- function(x,
                     y = NULL,
                     ...,
                     xy_join = NULL,
                     x_name = "x",
                     y_name = "y",
                     method = "spearman") {

  # Check arguments ------------

  check_data(x)
  if (!is.null(y)) {
    check_data(y)
    check_join(x, y, xy_join)
  }
  check_name(x_name)
  check_name(y_name)
  arg_match(method, c("pearson", "kendall", "spearman"))


  if (!is.null(y)) {
    corr_result <- corr_col_xy(
      x = x,
      y = y,
      xy_join = xy_join,
      method = method
    )
  } else {
    corr_result <- corr_col_x(x = x, method = method)
  }

  corr_result %>%
    mutate(q = p.adjust(p, method = "fdr")) %>%
    rename_with(
      .fn = str_replace,
      .cols = matches("x$"),
      pattern = "x$",
      replacement = x_name
    ) %>%
    rename_with(
      .fn = str_replace,
      .cols = matches("y$"),
      pattern = "y$",
      replacement = y_name
    ) %>%
    remove_rownames()

}

#' (Internal) Single data frame (`x`) case of `corr_col()`
#'
#' @param x A tidy data frame containing numeric data columns.
#' @param method (Optional) A character of length 1 indicating which correlation coefficient is to be used: `"spearman"` (the default), `"pearson"`, or `"kendall"`.
#'
#' @return A data frame of correlation results.
corr_col_x <- function(x, method) {
  corr_data <- x %>%
    select(where(is.numeric))

  if (ncol(corr_data) < 2)
    cli_abort(c("x" = "{.arg x} must have at least two numeric columns."))

  corr_vars <- corr_data %>%
    names() %>%
    combn(x = ., m = 2) %>%
    t() %>%
    as_tibble(.name_repair = ~ vctrs::vec_as_names(..., repair = "unique", quiet = TRUE)) %>% # `vec_as_names` fn suppresses messages from `as_tibble()`
    rename(var_x = ...1, var_y = ...2)

  map2(.x = corr_vars$var_x,
       .y = corr_vars$var_y,
       .f = possibly(\(var_x, var_y) corr(var_x, var_y, corr_data, method))) %>%
    list_rbind()

}


#' (Internal) Two data frame (`x`, `y`) case of `corr_col()`
#'
#' @param x A tidy data frame containing numeric data columns.
#' @param y A tidy data frame containing numeric data columns.
#' @param xy_join (Optional) A join specification for `x` and `y` constructed by \link[dplyr]{join_by}.
#' @param method (Optional) A character of length 1 indicating which correlation coefficient is to be used: `"spearman"` (the default), `"pearson"`, or `"kendall"`.
#'
#' @return A data frame of correlation results.
corr_col_xy <- function(x, y, xy_join = NULL, method) {

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

  x_var_prefix = "x."
  y_var_prefix = "y."

  x_data <- x %>%
    select(any_of(xy_join$x), where(is.numeric)) %>%
    rename_with(
      .cols = -all_of(xy_join$x) & where(is.numeric),
      .fn = \(x) paste0(x_var_prefix, x)
    )
  y_data <- y %>%
    select(any_of(xy_join$y), where(is.numeric)) %>%
    rename_with(
      .cols = -all_of(xy_join$y) & where(is.numeric),
      .fn = \(x) paste0(y_var_prefix, x)
    )

  corr_data <- inner_join(x_data, y_data, by = xy_join)

  if (nrow(corr_data) < 3)
    cli_abort(c("Too few observations remain after joining `x` and `y`."))

  x_num_vars <- x_data %>%
    select(-any_of(xy_join$x) & where(is.numeric)) %>%
    names()
  y_num_vars <- y_data %>%
    select(-any_of(xy_join$y) & where(is.numeric)) %>%
    names()

  x_vars <- corr_data %>%
    select(-(any_of(xy_join$x) |
               any_of(xy_join$y))) %>% # Subtract join vars
    select(-any_of(y_num_vars)) %>% # Subtract other data frame vars
    #janitor::remove_constant(na.rm = T, quiet = F) %>% # Subtract constant data frame vars
    names()
  y_vars <- corr_data %>%
    select(-(any_of(xy_join$x) | any_of(xy_join$y))) %>%
    select(-any_of(x_num_vars)) %>%
    #janitor::remove_constant(na.rm = T, quiet = F) %>%
    names()

  if (length(x_vars) == 0)
    cli_abort(c("No non-constant numeric columns from {.arg x} remain after joining {.arg x} and {.arg y}."))
  if (length(y_vars) == 0)
    cli_abort(c("No non-constant numeric columns from {.arg y} remain after joining {.arg x} and {.arg y}."))

  corr_vars <- expand_grid(var_x = x_vars, var_y = y_vars)

  map2(.x = corr_vars$var_x,
       .y = corr_vars$var_y,
       .f = possibly(\(var_x, var_y) corr(var_x, var_y, corr_data, method))) %>%
    list_rbind() %>%
    mutate(x = str_remove(x, paste0("^", x_var_prefix)), y = str_remove(y, paste0("^", y_var_prefix))) %>%
    group_by(x) %>%
    mutate(q_x = p.adjust(p, method = "fdr")) %>%
    ungroup() %>%
    group_by(y) %>%
    mutate(q_y = p.adjust(p, method = "fdr")) %>%
    ungroup()
}

#' (Internal)Mapped correlation function for `corr_col()`
#'
#' @param var_x A column name of `corr_data` as a character of length 1
#' @param var_y A column name of `corr_data` as a character of length 1
#' @param corr_data The data frame prepared for correlation
#' @param method (Optional) A character of length 1 indicating which correlation coefficient is to be used: `"spearman"` (the default), `"pearson"`, or `"kendall"`.
#'
#' @return The results of a single correlation as a data frame row.
corr <- function(var_x, var_y, corr_data, method) {

  tryCatch({
    corr_result <- cor.test(
      x = corr_data[[var_x]],
      y = corr_data[[var_y]],
      method = method,
      exact = FALSE
    )

    return(cbind(
      data.frame(x = var_x, y = var_y),
      statistic_column(corr_result$estimate, method),
      data.frame(p = corr_result$p.value, message = NA)
    ))
  }, warning = function(cond) {
    return(cbind(
      data.frame(x = var_x, y = var_y),
      statistic_column(NA, method),
      data.frame(p = NA, message = conditionMessage(cond))
    ))
  }, error = function(cond) {
    return(cbind(
      data.frame(x = var_x, y = var_y),
      statistic_column(NA, method),
      data.frame(p = NA, message = conditionMessage(cond))
    ))
  })

}
