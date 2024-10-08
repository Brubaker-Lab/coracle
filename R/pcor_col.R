#' Partial correlation of columns for tidy data.
#'
#' @param x A tidy data.frame containing numeric data columns.
#' @param y (Optional) A tidy data frame containing numeric data columns for pairwise correlation with `x`.
#' @param z (Optional) A tidy data frame containing numeric data columns to use as covariates.
#' @param ... Unused
#' @param xy_join (Optional) A join specification for `x` and `y` constructed by \link[dplyr]{join_by}.
#' @param xz_join (Optional) A join specification for `x` and `z` constructed by \link[dplyr]{join_by}.
#' @param x_name (Optional) A character of length 1 to be used as the first column name.
#' @param y_name (Optional) A character of length 1 to be used as the second column name.
#' @param z_name (Optional) A character of length 1 to be used as the third column name.
#' @param method (Optional) A character of length 1 indicating which correlation coefficient is to be used: `"spearman"` (the default), `"pearson"`, or `"kendall"`.
#'
#' @return A data frame of partial correlation results.
#' @export
pcor_col <- function(x,
                     y = NULL,
                     z = NULL,
                     ...,
                     xy_join = NULL,
                     xz_join = NULL,
                     x_name = "x",
                     y_name = "y",
                     z_name = "z",
                     method = "spearman") {
  # Two cases: x, xyz


  if (is.null(y) || is.null(z)) {
    cli_abort(
      c(
        "{.fn coracle::pcor_col}` for a single data frame, `x`, is still under development."
      )
    )

    result <- pcor_col_x(x = x,
                         x_name = x_name,
                         method = method) %>%
      mutate(q = p.adjust(p, method = "fdr"))
  } else {
    result <- pcor_col_xyz(
      x = x,
      y = y,
      z = z,
      xy_join = xy_join,
      xz_join = xz_join,
      x_name = x_name,
      y_name = y_name,
      z_name = z_name,
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

#' (Internal) Single data frame (`x`) case of `pcor_col()`
#'
#' @param x A tidy data frame containing numeric data columns.
#' @param x_name (Optional) A character of length 1 to be used as the first column name.
#' @param ... Unused.
#' @param method (Optional) A character of length 1 indicating which correlation coefficient is to be used: `"spearman"` (the default), `"pearson"`, or `"kendall"`.
#'
#' @return A data frame of partial correlation results.
pcor_col_x <- function(x,
                       x_name = "x",
                       ...,
                       method = "spearman") {
  arg_match(method, c("pearson", "kendall", "spearman"))
  arg_check_data(x, "x")

  pcor_data <- x %>%
    select(where(is.numeric))

  if (ncol(pcor_data) < 3)
    cli_abort(c("{.var x} must have at least three numeric columns."))

  pcor_vars <- pcor_data %>%
    names()  %>%
    combn(x = ., m = 2) %>%
    t() %>%
    as_tibble(.name_repair = ~ vctrs::vec_as_names(..., repair = "unique", quiet = TRUE)) %>% # `vec_as_names` fn suppresses messages from `as_tibble()`
    rename(var_x = ...1, var_y = ...2) %>%
    rowwise() %>%
    mutate(var_z = list(setdiff(names(pcor_data), c(var_x, var_y)))) %>%
    ungroup()

  pmap(.l = pcor_vars, .f = possibly(\(var_x, var_y, var_z) pcor(var_x, var_y, var_z, pcor_data, method))) %>%
    list_rbind()

}


#' (Internal) Mapped partial correlation function for `pcor_col()`
#'
#' @param var_x A column name of `pcor_data` as a character of length 1.
#' @param var_y A column name of `pcor_data` as a character of length 1.
#' @param var_z A list of column names of `pcor_data`.
#' @param pcor_data The data prepare for partial correlation.
#' @param method (Optional) A character of length 1 indicating which correlation coefficient is to be used: `"spearman"` (the default), `"pearson"`, or `"kendall"`.
#'
#' @return The results of a single partial correlation as a data frame row.
pcor <- function(var_x, var_y, var_z, pcor_data, method) {
  arg_match(method, c("pearson", "kendall", "spearman"))

  temp_data <- pcor_data[, c(var_x, var_y, var_z)] %>%
    drop_na()

  tryCatch({
    pcor_result <- ppcor::pcor.test(
      x = temp_data[var_x],
      y = temp_data[var_y],
      z = temp_data[, var_z],
      method = method
    )

    return(cbind(
      tibble(
        x = var_x,
        y = var_y,
        z = list(var_z)
      ),
      statistic(pcor_result$estimate, method),
      tibble(p = pcor_result$p.value, message = NA)
    ))

  }, error = function(cond) {
    return(cbind(
      tibble(
        x = var_x,
        y = var_y,
        z = list(var_z)
      ),
      statistic(NA, method),
      tibble(p = NA, message = conditionMessage(cond))
    ))
  })

}

#' (Internal) Three data frame (`x`, `y`, `z`) case of `pcor_col()`
#'
#' @param x A tidy data frame containing numeric data columns.
#' @param y A tidy data frame containing numeric data columns.
#' @param z A tidy data frame containing numeric data columns.
#' @param ... Unused.
#' @param xy_join (Optional) A join specification for `x` and `y` constructed by \link[dplyr]{join_by}.
#' @param xz_join (Optional) A join specification for `x` and `z` constructed by \link[dplyr]{join_by}.
#' @param x_name (Optional) A character of length 1 to be used as the first column name.
#' @param y_name (Optional) A character of length 1 to be used as the second column name.
#' @param z_name (Optional) A character of length 1 to be used as the third column name.
#' @param method (Optional) A character of length 1 indicating which correlation coefficient is to be used: `"spearman"` (the default), `"pearson"`, or `"kendall"`.
#'
#' @return A data frame of partial correlation results.
pcor_col_xyz <- function(x,
                         y,
                         z,
                         ...,
                         xy_join = NULL,
                         xz_join = NULL,
                         x_name = "x",
                         y_name = "y",
                         z_name = "z",
                         method = "spearman") {
  arg_match(method, c("pearson", "kendall", "spearman"))
  arg_check_data(x, "x")
  arg_check_data(y, "y")
  arg_check_data(z, "z")

  if (is.null(xy_join)) {
    xy_join$x <- intersect(names(x), names(y))
    xy_join$y <- intersect(names(x), names(y))
  }

  if (is_empty(xy_join$x) | is_empty(xy_join$y))
    cli_abort(
      c("{.arg x} and {.arg y} do not share any columns.", "i" = "Provide a join specification for {.arg xy_join}  with {.fn dplyr::join_by}.")
    )

  cli({
    cli_alert_info("Joining {.arg x} and {.arg y} by the following columns:")
    if (is.list(xy_join)) {
      cli_ul(paste(dQuote(xy_join$x), dQuote(xy_join$y), sep = " = "))
    } else {
      cli_ul(xy_join$expr)
    }
    cli_alert_warning("Override by providing {.arg xy_join} argument.")
  })


  if (is.null(xz_join)) {
    xz_join$x <- intersect(names(x), names(z))
    xz_join$y <- intersect(names(x), names(z)) # The `dplyr::join_by()` object uses `x` and `y`
  }

  if (is_empty(xz_join$x) || is_empty(xz_join$y))
    cli_abort(
      c("{.arg x} and {.arg z} do not share any columns.", "i" = "Provide a join specification for {.arg xz_join} with {.fn dplyr::join_by}.")
    )

  cli({
    cli_alert_info("Joining {.arg x} and {.arg z} by the following columns:")
    if (is.list(xz_join)) {
      cli_ul(paste(dQuote(xz_join$x), dQuote(xz_join$y), sep = " = "))
    } else {
      cli_ul(xz_join$expr)
    }
    cli_alert_warning("Override by providing {.arg xz_join} argument.")
  })

  cli_alert(
    "Refer to {.href [documentation for the {.arg by} argument of {.pkg dplyr} mutating joins](https://dplyr.tidyverse.org/reference/mutate-joins.html)}."
  )

  x_var_prefix = "x."
  y_var_prefix = "y."
  z_var_prefix = "z."

  x_data <- x %>%
    select(any_of(c(xy_join$x, xz_join$x)), where(is.numeric)) %>%
    rename_with(
      .cols = -any_of(c(xy_join$x, xz_join$x)) & where(is.numeric),
      .fn = \(x) paste0(x_var_prefix, x)
    )
  y_data <- y %>%
    select(any_of(xy_join$y), where(is.numeric)) %>%
    rename_with(
      .cols = -any_of(xy_join$y) & where(is.numeric),
      .fn = \(x) paste0(y_var_prefix, x)
    )
  z_data <- z %>%
    select(any_of(xz_join$y), where(is.numeric)) %>%
    rename_with(
      .cols = -any_of(xz_join$y) & where(is.numeric),
      .fn = \(x) paste0(z_var_prefix, x)
    )

  #return(list(x_data, y_data, z_data))

  pcor_data <- x_data %>%
    inner_join(y_data, by = xy_join) %>%
    inner_join(z_data, by = xz_join)

  if (nrow(pcor_data) < 3)
    cli_abort(c("Too few observations remain after joining `x`, `y`, and `z`."))

  #return(pcor_data)

  x_num_vars <- x_data %>%
    select(-all_of(c(xy_join$x, xz_join$x)) & where(is.numeric)) %>%
    names()
  y_num_vars <- y_data %>%
    select(-all_of(xy_join$y) & where(is.numeric)) %>%
    names()
  z_num_vars <- z_data %>%
    select(-all_of(xz_join$y) & where(is.numeric)) %>%
    names()

  all_join_vars <- unique(c(xy_join$x, xy_join$y, xz_join$x, xz_join$y))

  x_vars <- pcor_data %>%
    select(-(any_of(all_join_vars))) %>% # Subtract join vars
    select(-any_of(c(y_num_vars, z_num_vars))) %>% # Subtract other data frame vars
    janitor::remove_constant(na.rm = T, quiet = F) %>% # Subtract constant data frame vars
    names()
  y_vars <- pcor_data %>%
    select(-(any_of(all_join_vars))) %>%
    select(-any_of(c(x_num_vars, z_num_vars))) %>%
    janitor::remove_constant(na.rm = T, quiet = F) %>%
    names()
  z_vars <- pcor_data %>%
    select(-(any_of(all_join_vars))) %>%
    select(-any_of(c(x_num_vars, y_num_vars))) %>%
    janitor::remove_constant(na.rm = T, quiet = F) %>%
    names()

  if (length(x_vars) == 0)
    cli_abort(c(
      "No numeric columns from {.arg x} remain after joining {.arg x} and {.arg y}."
    ))
  if (length(y_vars) == 0)
    cli_abort(c(
      "No numeric columns from {.arg y} remain after joining {.arg x} and {.arg y}."
    ))
  if (length(z_vars) == 0)
    cli_abort(c(
      "No numeric columns from {.arg z} remain after joining {.arg x} and {.arg z}."
    ))

  pcor_vars <- expand_grid(var_x = x_vars, var_y = y_vars) %>%
    mutate(var_z = list(z_vars))

  pmap(.l = pcor_vars, .f = possibly(\(var_x, var_y, var_z) pcor(var_x, var_y, var_z, pcor_data, method))) %>%
    list_rbind() %>%
    mutate(
      x = str_remove(x, paste0("^", x_var_prefix)),
      y = str_remove(y, paste0("^", y_var_prefix)),
      z = str_remove(z, paste0("^", z_var_prefix))
    )
}
