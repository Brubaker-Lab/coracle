stat_name <- list(spearman = "rho",
                  kendall = "tau",
                  pearson = "cor")

#' Correlation of Tidy Data
#'
#' @param x A `coracle_data` object for correlation.
#' @param y (Optional) A `coracle_data` object for correlation.
#' @param z (Optional) A `coracle_data` object for correlation.
#' @param ... Unused
#' @param method The correlation method ("spearman", "pearson", or "kendall")
#'
#' @returns A `coracle_data` object with correlation results.
#' @export
coracle <- function(x,
                    y = NULL,
                    z = NULL,
                    ...,
                    method = "spearman",
                    output = "coracle_data") {

  validate_coracle_input(x)
  validate_coracle_input(y)
  validate_coracle_input(z)

  arg_match(method, c("spearman", "pearson", "kendall"))
  arg_match(output, c("coracle_data", "tibble"))


  if (!is.null(x) && !is.null(y) && !is.null(z)) {

    cli_abort(c("x" = "Pairwise partial correlation has not been implemented yet"))

    result <- pairwise_partial_correlation(x, y, z, method)

  } else if (!is.null(x) && !is.null(y)) {

    result <- pairwise_correlation(x, y, method)



  } else if (!is.null(x)) {

    cli_abort(c("x" = "Autocorrelation has not been implemented yet"))

    result <- autocorrelation(x, method)

  } else {

    cli_abort(c("x" = "This case should be unreachable!"))

  }

  return(coracle_data$new(data = result,
                          grps = x$cols$grps,
                          join = y$cols$grps,
                          vals = stat_name[[method]]))

}

#' (Internal) Check non-null inputs for `coracle_data` objects
#'
#' @param input The input value.
#' @param arg The input argument.
#' @param call The caller environment.
#'
#' @returns Invisibly.
validate_coracle_input <- function(input,
                                   arg = caller_arg(input),
                                   call = caller_env()) {
  if (!is.null(input) && !("coracle_data" %in% class(input))) {
    cli_abort(c("x" = "{.arg {arg}} requires {.cls coracle_data}."))
  }

}

#' (Internal) assemble correlation results in data.frame.
#'
#' @param a A `coracle_data` object used in a correlation.
#' @param b A `coracle_data` object used in a correlation.
#' @param c A `coracle_data` object used in a correlation.
#' @param stat_value The estimated correlation result.
#' @param p The p-value of the correlation result.
#' @param n The number of observations used in a correlation.
#' @param message An informative message about the correlation result.
#' @param method The correlation method ("spearman", "pearson", or "kendall")
#'
#' @returns A data.frame of correlation results.
corr_result <- function(a = NULL,
                        b = NULL,
                        c = NULL,
                        stat_value = NA,
                        p = NA,
                        n = NA_integer_,
                        message = NA_character_,
                        method) {



  statistic <- stat_value
  names(statistic) <- stat_name[[method]]

  as.data.frame(
    c(
      a$grps_vals,
      b$grps_vals,
      c$grps_vals,
      statistic,
      p = as.numeric(p),
      n = as.integer(n),
      message = message
    )
  )

}



#' (Internal) Pairwise correlation of between two `coracle_data` objects.
#'
#' @param x A `coracle_data` object for correlation.
#' @param y A `coracle_data` object for correlation.
#' @param method The correlation method ("spearman", "pearson", or "kendall")
#'
#' @returns A data.frame of correlation results.
pairwise_correlation <- function(x, y, method) {

  if (length(intersect(x$data[[x$cols$join]],
                       y$data[[y$cols$join]])) < 3)
    cli_abort(c("x" = "Not enough overlap in join values to run correlation."))

  invalid_corrs <- c(x$leaves_invalid, y$leaves_invalid) |>
    map(\(o) corr_result(o,message = paste(o$chunk_flags), method = method)) |> list_rbind()

  leaf_pairs <- expand_grid(a = x$leaves_valid, b = y$leaves_valid) |>
    mutate(method = method)

  if (nrow(leaf_pairs) == 0)
    cli_abort(c("x" = "Not enough valid chunks to run correlation."))

  valid_corrs <- future_pmap(leaf_pairs, correlate, .progress = T) |>
    list_rbind()

  bind_rows(valid_corrs, invalid_corrs)
}

#' (Internal) Correlate data of `coracle_data` objects.
#'
#' @param a A `coracle_data` object for correlation.
#' @param b A `coracle_data` object for correlation.
#' @param method The correlation method ("spearman", "pearson", or "kendall")
#'
#' @returns A `corr_result` data.frame.
correlate <- function(a, b, method) {

  a_data <- a$data[c(a$cols$grps,
                     a$cols$join,
                     a$cols$vals)] |>
    rename_with(\(x) paste(x, a$meta$id,"a", sep = "_"))

  b_data <- b$data[c(b$cols$grps,
                     b$cols$join,
                     b$cols$vals)] |>
    rename_with(\(x) paste(x, b$meta$id,"b", sep = "_"))

  # Assiging to variables avoids `join_by`'s input validation

  a_join <- paste(a$cols$join, a$meta$id,"a", sep = "_")
  b_join <- paste(b$cols$join, b$meta$id,"b", sep = "_")


  corr_data <- full_join(a_data,
                         b_data,
                         by = join_by(!!sym(a_join) == !!sym(b_join))) |>
    drop_na()

  a_vals <- paste(a$cols$vals, a$meta$id,"a", sep = "_")
  b_vals <- paste(b$cols$vals, b$meta$id,"b", sep = "_")

  corr <- cor.test(
    x = corr_data[[a_vals]],
    y = corr_data[[b_vals]],
    method = method,
    exact = FALSE
  )

  corr_result(
    a,
    b,
    stat_value = corr$estimate,
    p = corr$p.value,
    n = nrow(corr_data),
    method = method
  )
}
