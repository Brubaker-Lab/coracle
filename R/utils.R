#' (Internal) Update data frame column with name corresponding to correlation method.
#'
#' @param value A numeric or NA value extracted from the correlation results.
#' @param method A character of length 1 indicating which correlation coefficient is to be used: `"spearman"` (the default), `"pearson"`, or `"kendall"`.
#'
#' @return A 1x1 data frame of the value in a column named "rho", "cor", or "tau"
statistic <- function(value, method) {

  if()

  arg_match(method, c("pearson", "kendall", "spearman"))

  switch (
    method,
    "pearson" = data.frame(cor = value),
    "kendall" = data.frame(tau = value),
    "spearman" = data.frame(rho = value)
  )
}

check_numeric_length_1 <- function(x,
                                   arg = caller_arg(x),
                                   call = caller_env()){

  if(!is.numeric(x) | length(x) != 1)
    cli_abort(
      c("x" = "{.arg {arg}} must be a {.cls numeric} of length {.val {1}}.",
        "i" = "Input is a {.cls {class(x)}} of length {.val {length(x)}}"),
      call = call
    )

}


#' (Internal) Check data frames provided as inputs for basic requirements.
#'
#' @param arg_data A data frame passed as an argument to be checked.
#' @param arg_name A character of length 1 indicating the name of the argument.
#'
#' @return NULL. Throws an error if the data frame doesn't pass the checks.
check_data <- function(x,
                           arg = caller_arg(x),
                           call = caller_env()) {

  # Is it a data frame? ------------------------------------------

  if (!is.data.frame(x))
    cli_abort(
      c("x" = "{.arg {arg}} must be a {.cls data frame}.",
        "i" = "Input is a {.obj_type_friendly {arg}}"),
      call = call
    )

  # Does it contain numeric data? ------------------------------------------

  if(x %>% select(where(is.numeric)) %>% ncol() < 1)
    cli_abort(c("x" = "{.arg {arg}} must have at least one {.cls numeric} column."),
              call = call)

  # Does it have at least three observations/rows? ------------------------------------------

  if(nrow(x) < 3)
    cli_abort(c("x" = "{.arg {arg}} must have at least three observations/rows.",
                "i" = "Input has {.val {nrow(x)}}"),
              call = call)

}

check_string <- function(x,
                             arg = caller_arg(x),
                             call = caller_env()){

  if(!is_string(x))
    cli_abort(c("x" = "{.arg {arg}} must be a string",
                "i" = "Input is a {.obj_type_friendly {arg}}"),
              call = call)

}

check_shared_columns <- function(x,
                                  arg = caller_arg(x),
                                  call = caller_env()){



  if(!is_string(x))
    cli_abort(c("x" = "{.arg {arg}} must be a string",
                "i" = "Input is a {.obj_type_friendly {arg}}"),
              call = call)

}
