library(R6)
library(rlang)
library(cli)
library(tidyverse)

# Helper `is` ------------

is.coracle_obj <- function(x) {
  "coracle_obj" %in% class(x)

}


# Initialize Function ------------

#' Title
#'
#' @param data
#' @param join_col
#' @param vals_cols
#' @param grps_cols
#' @param vals_labl
#' @param call
#'
#' @returns
#' @export
#'
#' @examples
f_initialize <- function(data,
                         join_col,
                         vals_cols = NULL,
                         grps_cols = NULL,
                         vals_labl = NULL,
                         call = caller_env()) {
  ## Initial Data Validation ------------

  if (!is.data.frame(data)) {
    cli_abort(c("x" = "{.arg data} requires a {.cls data.frame}."))
  }

  ## Join Column Validation ------------

  if (!is_scalar_character(join_col)) {
    cli_abort(c("x" = "{.arg join_col} requires a {.cls character} scalar."),
              call = call)
  }

  if (!(join_col %in% names(data))) {
    cli_abort(c("x" = "{.arg join_col} {.val {join_col}} not in {.arg data}."),
              call = call)
  }

  ## Values column(s) Validation ------------

  if (!is_character(vals_cols)) {
    cli_abort(c("x" = "{.arg vals_cols} requires a {.cls character} vector."),
              call = call)
  }

  if (!all(vals_cols %in% names(data))) {
    cli_abort(
      c("x" = "{.arg vals_cols} {.val {setdiff(vals_cols, names(data))}} not in {.arg data}."),
      call = call
    )
  }

  ## Group Column(s) Validation ------------

  if (is.null(grps_cols)) {
    if (is.grouped_df(data)) {
      grps_cols <- group_vars(data)
    } else{
      cli_abort(c("x" = "{.arg grps_cols} requires input."))
    }
  }

  if (!is_character(grps_cols)) {
    cli_abort(c("x" = "{.arg grps_col} requires a {.cls character} vector."),
              call = call)
  }

  if (!all(grps_cols %in% names(data))) {
    cli_abort(
      c("x" = "{.arg grps_cols} {.val {setdiff(grps_cols, names(data))}} not in {.arg data}."),
      call = call
    )
  }

  ## Prepare Data from Valid Inputs ------------

  ### Convert to long format with single Values column, if necessary ------------

  if (length(vals_cols) > 1) {
    if (!is_scalar_character(vals_labl)) {
      cli_abort(c("x" = "{.arg vals_labl} requires a {.cls character} scalar."),
                call = call)
    }

    data <- data |>
      select(all_of(c(join_col, vals_cols, grps_cols))) |>
      pivot_longer(
        cols = all_of(vals_cols),
        names_to = vals_labl,
        values_to = "values"
      )

    vals_col <- "values"

  } else {
    vals_col <- vals_cols
  }

  ### Re-group, if necessary ------------

  if (!is.null(vals_labl) && !(vals_labl %in% grps_cols)) {
    grps_cols <- c(grps_cols, vals_labl)
  }

  data <- data |>
    ungroup() |>
    group_by(across(all_of(grps_cols)))

  ## Other columns ------------

  other_cols <- data |>
    ungroup() |> # prevent group column(s) from being added back in
    select(-all_of(c(join_col, vals_col, grps_cols))) |>
    names()

  ## Set cols ------------

  self$join_col <- join_col
  self$vals_col <- vals_col
  self$grps_cols <- grps_cols
  self$other_cols <- other_cols

  ## Values ------------

  join_vals <- data |>
    pull(all_of(join_col)) |>
    unique() |>
    sort()

  self$join_vals <- join_vals

  ## Split ------------

  data <- data |>
    select(all_of(c(
      grps_cols, join_col, vals_col, other_cols
    ))) |>
    group_split()


  ## Recursion ------------

  if (length(data) != 1) {
    self$children <- map(
      data,
      \(d) coracle_obj$new(
        data = d,
        join_col = join_col,
        vals_cols = vals_col,
        grps_cols = grps_cols,
        vals_labl = vals_labl
      )
    )
  } else {
    self$data <- data |> pluck(1)
  }

  self$id <- hash(as.numeric(Sys.time())) |> str_sub(-7)
  self$version <- as.character(packageVersion("coracle"))
}


# Chunks function ------------

f_chunks <- function(obj = self) {
  if (!is.null(obj$data)) {
    return(obj$data)
  } else {
    Map(f_chunks, self$children)
  }

}

f_whole <- function(obj = self) {

  obj$chunks |>
    list_rbind() |>
    group_by(across(all_of(self$grps_cols)))

}

# Object Definition ------------

#' R6 Class Containing Data for Correlation
#'
#' @description
#' A coracle_obj contains data and column annotations.
#'
#' @details
#' A coracle_obj also handles formatting.

coracle_obj <- R6Class(
  "coracle_obj",
  public = list(
    version = NULL,
    id = NULL,
    data = NULL,
    children = NULL,
    grps_cols = NULL,
    join_col = NULL,
    join_vals = NULL,
    vals_col = NULL,
    other_cols = NULL,
    initialize = f_initialize
  ),
  active = list(chunks = f_chunks,
                whole = f_whole)
)
