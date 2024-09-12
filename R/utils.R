#' (Internal) Update data frame column with name corresponding to correlation method.
#'
#' @param value A numeric or NA value extracted from the correlation results.
#' @param method A character of length 1 indicating which correlation coefficient is to be used: `"spearman"` (the default), `"pearson"`, or `"kendall"`.
#'
#' @return A 1x1 data frame of the value in a column named "rho", "cor", or "tau"
statistic <- function(value, method) {
  arg_match(method, c("pearson", "kendall", "spearman"))

  switch (
    method,
    "pearson" = data.frame(cor = value),
    "kendall" = data.frame(tau = value),
    "spearman" = data.frame(rho = value)
  )
}


#' (Internal) Check data frames provided as inputs for basic requirements.
#'
#' @param arg_data A data frame passed as an argument to be checked.
#' @param arg_name A character of length 1 indicating the name of the argument.
#'
#' @return NULL. Throws an error if the data frame doesn't pass the checks.
arg_check_data <- function(arg_data, arg_name) {

  # Is it a data frame? ------------------------------------------

  if (!is.data.frame(arg_data))
    cli_abort(
      c("{.var {arg_name}} must be a {.cls data.frame}.",
        "x" = "You've supplied a {.cls {class(arg_data)}}")
    )

  # Does it contain numeric data? ------------------------------------------

  has_numeric_columns <- arg_data %>%
    select(where(is.numeric)) %>%
    ncol() > 0

  if (!has_numeric_columns)
    cli_abort(c("{.var {arg_data}} does not contain numeric columns."))

  NULL

}
