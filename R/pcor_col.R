#' Partial correlation of columns for tidy data.
#'
#' @param x A tidy data.frame containing numeric data columns.
#' @param y (Optional) A tidy data frame containing numeric data columns for pairwise correlation with `x`.
#' @param z (Optional) A tidy data frame containing numeric data columns to use as covariates.
#' @param ... Unused.
#' @param xy_join (Optional) A \code{\link[dplyr]{join_by}} join specification for `x` and `y`.
#' @param xz_join (Optional) A \code{\link[dplyr]{join_by}} join specification for `x` and `z`.
#' @param x_name (Optional) A character of length 1 to be used as the first column name.
#' @param y_name (Optional) A character of length 1 to be used as the second column name.
#' @param z_name (Optional) A character of length 1 to be used as the third column name.
#' @param method (Optional) A character of length 1 indicating which correlation coefficient is to be used: `"spearman"` (the default), `"pearson"`, or `"kendall"`.
#'
#' @return A data frame of partial correlation results.
#' @export
#' @examples
#'
#' # Data frames with a shared column, "i"
#' x <- data.frame(i = as.character(1:5), ux = 1:5, dx = 5:1)
#' y <- data.frame(i = as.character(1:5), uy = 1:5, dy = 5:1)
#' z <- data.frame(i = as.character(1:5), rz = runif(5))
#' corr_col(x, y, z)
#'
#' # Data frames without a shared column
#' x <- data.frame(ix= as.character(1:5), ux = 1:5, dx = 5:1)
#' y <- data.frame(iy = as.character(1:5), uy = 1:5, dy = 5:1)
#' z <- data.frame(iz = as.character(1:5), rz = runif(5))
#' corr_col(x, y,xy_join = dplyr::join_by(ix == iy), xz_join = dplyr::join_by(ix == iz))
#'
#' # Renaming the outputs
#' x <- data.frame(i = as.character(1:5), ux = 1:5, dx = 5:1)
#' y <- data.frame(i = as.character(1:5), uy = 1:5, dy = 5:1)
#' z <- data.frame(i = as.character(1:5), rz = runif(5))
#' pcor_col(x,y,z, x_name = "first", y_name = "second", z_name = "third")
#'
pcor_col <- function(x,
                     y,
                     z,
                     ...,
                     xy_join = NULL,
                     xz_join = NULL,
                     x_name = "x",
                     y_name = "y",
                     z_name = "z",
                     method = "spearman") {

  check_data(x)
  check_data(y)
  check_data(z)
  check_join(x, y, xy_join)
  check_join(x, z, xz_join)
  check_name(x_name)
  check_name(y_name)
  check_name(z_name)
  arg_match(method, c("pearson", "kendall", "spearman"))

  pcor_col_xyz(
    x = x,
    y = y,
    z = z,
    xy_join = xy_join,
    xz_join = xz_join,
    method = method
  ) %>%
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
    rename_with(
      .fn = str_replace,
      .cols = matches("z$"),
      pattern = "z$",
      replacement = z_name
    ) %>%
    remove_rownames()

}

#' (Internal) Three data frame (`x`, `y`, `z`) case of `pcor_col()`
#'
#' @param x A tidy data frame containing numeric data columns.
#' @param y A tidy data frame containing numeric data columns.
#' @param z A tidy data frame containing numeric data columns.
#' @param xy_join (Optional) A join specification for `x` and `y` constructed by \link[dplyr]{join_by}.
#' @param xz_join (Optional) A join specification for `x` and `z` constructed by \link[dplyr]{join_by}.
#' @param method (Optional) A character of length 1 indicating which correlation coefficient is to be used: `"spearman"` (the default), `"pearson"`, or `"kendall"`.
#'
#' @return A data frame of partial correlation results.
pcor_col_xyz <- function(x, y, z, xy_join, xz_join, method = "spearman") {

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

  pcor_data <- x_data %>%
    inner_join(y_data, by = xy_join) %>%
    inner_join(z_data, by = xz_join)

  if (nrow(pcor_data) < 3)
    cli_abort(c("Too few observations remain after joining `x`, `y`, and `z`."))

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
    #janitor::remove_constant(na.rm = T, quiet = F) %>% # Subtract constant data frame vars
    names()
  y_vars <- pcor_data %>%
    select(-(any_of(all_join_vars))) %>%
    select(-any_of(c(x_num_vars, z_num_vars))) %>%
    #janitor::remove_constant(na.rm = T, quiet = F) %>%
    names()
  z_vars <- pcor_data %>%
    select(-(any_of(all_join_vars))) %>%
    select(-any_of(c(x_num_vars, y_num_vars))) %>%
    #janitor::remove_constant(na.rm = T, quiet = F) %>%
    names()

  if (length(x_vars) == 0)
    cli_abort(
      c(
        "No non-constant numeric columns from {.arg x} remain after joining {.arg x} and {.arg y}."
      )
    )
  if (length(y_vars) == 0)
    cli_abort(
      c(
      )
    )
  if (length(z_vars) == 0)
    cli_abort(
      c(
        "No non-constant numeric columns from {.arg z} remain after joining {.arg x} and {.arg z}."
      )
    )

  pcor_vars <- expand_grid(var_x = x_vars, var_y = y_vars)

  map2(.x = pcor_vars$var_x,
       .y = pcor_vars$var_y,
       .f = possibly(\(var_x, var_y) pcor(var_x, var_y, z_vars, pcor_data, method))) %>%
    list_rbind() %>%
    mutate(
      x = str_remove(x, paste0("^", x_var_prefix)),
      y = str_remove(y, paste0("^", y_var_prefix)),
      z = toString(map(z_vars, \(l) str_remove(l, paste0("^", z_var_prefix))))
    ) %>%
    mutate(q = p.adjust(p, method = "fdr")) %>%
    group_by(x) %>%
    mutate(q_x = p.adjust(p, method = "fdr")) %>%
    ungroup() %>%
    group_by(y) %>%
    mutate(q_y = p.adjust(p, method = "fdr")) %>%
    ungroup()
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

  temp_data <- pcor_data[, c(var_x, var_y, var_z)] %>%
    drop_na()

  tryCatch({
    suppressWarnings(
    pcor_result <- ppcor::pcor.test(
      x = temp_data[, var_x],
      y = temp_data[, var_y],
      z = temp_data[, var_z],
      method = method
    ))

    return(cbind(
      data.frame(
        x = var_x,
        y = var_y,
        z = NA
      ),
      statistic_column(pcor_result$estimate, method),
      data.frame(p = pcor_result$p.value,
                 n = nrow(temp_data),
                 message = NA)
    ))

  }, warning = function(cond) {

    return(cbind(
      data.frame(
        x = var_x,
        y = var_y,
        z = NA
      ),
      statistic_column(NA, method),
      data.frame(p = NA,
                 n = nrow(temp_data),
                 message = conditionMessage(cond))
    ))
  }, error = function(cond) {

    return(cbind(
      data.frame(
        x = var_x,
        y = var_y,
        z = NA
      ),
      statistic_column(NA, method),
      data.frame(p = NA,
                 n = nrow(temp_data),
                 message = conditionMessage(cond))
    ))
  })

}
