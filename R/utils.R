#' (Internal) Check data frames provided as inputs for basic requirements.
#'
#' @param arg_data A data frame passed as an argument to be checked.
#' @param arg_name A character of length 1 indicating the name of the argument.
#'
#' @return NULL. Throws an error if the data frame doesn't pass the checks.
check_data <- function(x, arg = caller_arg(x), call = caller_env()) {
  if (!is.data.frame(x))
    cli_abort(
      c("x" = "{.arg {arg}} must be a {.cls data frame}.", "i" = "Input is a {.obj_type_friendly {arg}}"),
      call = call
    )

  if (x %>% select(where(is.numeric)) %>% ncol() < 1)
    cli_abort(c("x" = "{.arg {arg}} must have at least one {.cls numeric} column."),
              call = call)

  if (nrow(x) < 3)
    cli_abort(
      c("x" = "{.arg {arg}} must have at least three observations/rows.", "i" = "Input has {.val {nrow(x)}}"),
      call = call
    )

}

check_join <- function(x,
                       y,
                       join,
                       arg_x = caller_arg(x),
                       arg_y = caller_arg(y),
                       arg_j = caller_arg(join),
                       call = caller_env()) {
  if (is.null(join)) {
    shared_cols = intersect(names(x), names(y))

    if (length(shared_cols) == 0) {
      cli_abort(
        c("x" = "{.arg {arg_x}} and {.arg {arg_y}} must have at least one shared column for joining.", "i" = "Provide a join specification to {.arg {arg_j}} with {.fn dplyr::join_by}."),
        call = call
      )
    } else if (length(shared_cols) == ncol(x)) {
      cli_abort(
        c("x" = "All columns of {.arg {arg_x}} are shared with {.arg {arg_y}} so no columns remain for calculation.", "i" = "Provide a join specification to {.arg {arg_j}} with {.fn dplyr::join_by}."),
        call = call
      )
    } else if (length(shared_cols) == ncol(y)) {
      cli_abort(
        c("x" = "All columns of {.arg {arg_y}} are shared with {.arg {arg_x}} so no columns remain for calculation.", "i" = "Provide a join specification to {.arg {arg_j}} with {.fn dplyr::join_by}."),
        call = call
      )
    }
  } else {
    if (!all(join$x %in% names(x))) {
      cli_abort(
        c("x" = "{.arg {arg_j}} join specification refers to columns not found in {.arg {arg_x}}", "i" = "Check {.arg {arg_j}} against {.arg {arg_x}} and {.fn dplyr::join_by}."),
        call = call
      )
    } else if (!all(join$y %in% names(y))) {
      cli_abort(
        c("x" = "{.arg {arg_j}} join specification refers to columns not found in {.arg {arg_y}}", "i" = "Check {.arg {arg_j}} against {.arg {arg_y}} and {.fn dplyr::join_by}."),
        call = call
      )
    }
  }
}
check_numeric_length <- function(x, arg = caller_arg(x), call = caller_env()) {
  if (!(is.atomic(x) && length(x) == 1)) {
    cli_abort(
      c("x" = "{.arg {arg}} must be length {.val {1}}.", "i" = "Input is length {.val {length(x)}}"),
      call = call
    )
  }

  if (!(is.numeric(x) || is.na(x))) {
    cli_abort(
      c("x" = "{.arg {arg}} must be {.val {NA}} or {.cls numeric}.", "i" = "Input is {.cls {class(x)}}"),
      call = call
    )
  }
}


check_name <- function(x, arg = caller_arg(x), call = caller_env()) {
  if (!is_string(x))
    cli_abort(
      c("x" = "{.arg {arg}} must be a {.cls character} of length {.val {1}} or {.cls string}", "i" = "Input is a {.cls {class(x)}}"),
      call = call
    )
}

#' (Internal) Update data frame column with name corresponding to correlation method.
#'
#' @param value A numeric or NA value extracted from the correlation results.
#' @param method A character of length 1 indicating which correlation coefficient is to be used: `"spearman"` (the default), `"pearson"`, or `"kendall"`.
#'
#' @return A 1x1 data frame of the value in a column named "rho", "cor", or "tau"

statistic_column <- function(value, method) {
  check_numeric_length(value)

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
