library(R6)
library(rlang)
library(cli)
library(tidyverse)

# Helper `is` ------------

is.coracle_obj <- function(x){

  "coracle_obj" %in% class(x)

}


# Initialize Function ------------

f_initialize <- function(data,
                         name_col = NULL,
                         join_col = NULL,
                         vals_cols = NULL,
                         call = caller_env()) {
  ## Data ------------

  if (!is.data.frame(data)) {
    cli_abort(c("x" = "{.arg data} requires a {.cls data.frame}."))
  }

  ## Wide Input ------------

  is_wide_input <- length(vals_cols) > 1

  ## Join column ------------

  if (!is_scalar_character(join_col)) {
    cli_abort(c("x" = "{.arg join_col} requires a {.cls character} scalar."),
              call = call)
  }

  if (!(join_col %in% names(data))) {
    cli_abort(c("x" = "{.arg join_col} {.val {join_col}} not in {.arg data}."),
              call = call)
  }

  ## Values column ------------

  if (!is_character(vals_cols)) {
    cli_abort(c("x" = "{.arg vals_cols} requires a {.cls character} scalar."),
              call = call)
  }

  if (!all(vals_cols %in% names(data))) {
    cli_abort(
      c("x" = "{.arg vals_cols} {.val {setdiff(vals_cols, names(data))}} not in {.arg data}."),
      call = call
    )
  }

  ## Name Column ------------

  if (!is_scalar_character(name_col)) {
    cli_abort(c("x" = "{.arg name_col} requires a {.cls character} scalar."),
              call = call)
  }

  if (!is_wide_input) {
    if (!(name_col %in% names(data))) {
      cli_abort(c("x" = "{.arg name_col} {.val {name_col}} not in {.arg data}."),
                call = call)
    }
  }

  ### Convert to long format, if necessary ------------

  vals_col <- NULL

  if (is_wide_input) {
    data <- data |>
      select(any_of(c(name_col, join_col, vals_cols))) |>
      pivot_longer(
        cols = all_of(vals_cols),
        names_to = name_col,
        values_to = "values"
      )

    vals_col <- "values"

  }

  vals_col <- vals_col %||% vals_cols

  ## Set cols ------------

  self$join_col <- join_col
  self$name_col <- name_col
  self$vals_col <- vals_col

  ## Other columns ------------

  other_cols <- data |>
    select(-any_of(c(name_col, join_col, vals_col))) |>
    names()

  self$other_cols <- other_cols

  ## Values ------------

  name_vals <- data |>
    pull(any_of(name_col)) |>
    unique() |>
    sort()

  self$name_vals <- name_vals

  join_vals <- data |>
    pull(any_of(join_col)) |>
    unique() |>
    sort()

  self$join_vals <- join_vals

  ## Split ------------

  data <- data |>
    select(any_of(c(
      name_col, join_col, vals_col, other_cols
    ))) |>
    group_by(!!sym(name_col)) |>
    group_split()


  self$data <- data
  self$id <- hash(Sys.time())
}

# Object Definition ------------

coracle_obj <- R6Class(
  "coracle_obj",
  public = list(
    id = NULL,
    data = NULL,
    name_col = NULL,
    name_vals = NULL,
    join_col = NULL,
    join_vals = NULL,
    vals_col = NULL,
    other_cols = NULL,
    initialize = f_initialize
  )
)
