#' Correlate all columns of two data frames
#'
#' @param x_data A data frame with at least one index column and at least one column of numeric data.
#' @param y_data A data frame with at least one index column and at least one column of numeric data.
#' @param ... Unused
#' @param x_name A string identifying the type of data in `x_data`
#' @param y_name A string identifying the type of data in `y_data`
#' @param join_vars A named list of column names for joining, passed to `dplyr::inner_join()`'s `by` argument. See `dplyr` reference for information.
#'
#' @return A data frame of correlation results.
#'
#' @export
corr_col <- function(x_data,
                     y_data,
                     ...,
                     x_name,
                     y_name,
                     join_vars = NULL) {
  stopifnot("Error: x is not a data frame." = is.data.frame(x_data))
  stopifnot("Error: y is not a data frame." = is.data.frame(y_data))

  stopifnot("Error: No string given for `x_name`." = is.character(x_name))
  stopifnot("Error: No string given for `y_name`." = is.character(y_name))

  stopifnot("Error: x has no numeric data." = x_data %>%
              dplyr::select(where(is.numeric)) %>%
              ncol() > 0)
  stopifnot("Error: y has no numeric data." = y_data %>%
              select(where(is.numeric)) %>%
              ncol() > 0)

  # find join variable(s) from x, y

  xy_intersect <- intersect(names(x_data), names(y_data))
  names(xy_intersect) <- intersect(names(x_data), names(y_data))

  join_vars <- join_vars %||% xy_intersect

  stopifnot(
    "Error: No shared column found in x_data and y_data. Provide `join_vars`." = length(join_vars) > 0
  )
  message("Joining data by these columns. Provide `join_vars` to override.")
  message(paste(
    names(join_vars),
    join_vars,
    sep = " = ",
    collapse = "\n"
  ))

  x_data_nested <- x_data %>%
    select(all_of(join_vars), where(is.numeric)) %>%
    nest(.by = names(join_vars), .key = "x_data")
  y_data_nested <- y_data %>%
    select(all_of(join_vars), where(is.numeric)) %>%
    nest(.by = unname(join_vars), .key = "y_data")

  data <- inner_join(x_data_nested, y_data_nested, by = join_vars)

  x_vars <- names(x_data_nested$x_data[[1]])
  y_vars <- names(y_data_nested$y_data[[1]])

  stopifnot("Error: No data columns in x_data after join."  = length(x_vars) > 0)
  stopifnot("Error: No data columns in y_data after join."  = length(y_vars) > 0)

  corr_vars <- expand_grid(x_var = x_vars, y_var = y_vars)

  map2(
    .x = corr_vars$x_var,
    .y = corr_vars$y_var,
    .f = possibly(~ pairwise_corr(.x, .y, data))
  ) %>%
    list_rbind() %>%
    mutate(q = p.adjust(p, method = "fdr")) %>%
    group_by(x) %>%
    mutate(q_x = p.adjust(p, method = "fdr")) %>%
    ungroup() %>%
    group_by(y) %>%
    mutate(q_y = p.adjust(p, method = "fdr")) %>%
    ungroup() %>%
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
    )

}

#' Internal function for calculation correlations
#'
#' @param x_var Column name string for correlation
#' @param y_var Column name string for correlation
#' @param data Data frame with join variables and nested data
#'
#' @return Data frame row of correlation results
pairwise_corr <- function(x_var, y_var, data) {
  cor_data <- data %>%
    hoist(x_data, x_col = x_var) %>%
    hoist(y_data, y_col = y_var) %>%
    select(x_col, y_col) %>%
    drop_na()

  if (nrow(cor_data) > 2) {
    suppressWarnings(corr <- cor.test(
      x = cor_data[["x_col"]],
      y = cor_data[["y_col"]],
      method = "spearman",
      exact = FALSE
    ))

    return(data.frame(
      x = x_var,
      y = y_var,
      rho = corr$estimate,
      p = corr$p.value,
      n = nrow(cor_data)
    ))

  } else {
    return(data.frame(
      x = x_var,
      y = y_var,
      rho = NA,
      p = NA,
      n = nrow(cor_data)
    ))
  }
}
