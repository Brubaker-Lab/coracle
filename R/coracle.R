validate_coracle_input <- function(input,
                                   arg = caller_arg(input),
                                   call = caller_env()) {
  if (!is.null(input) && !is.coracle_obj(input)) {
    cli_abort(c("x" = "{.arg {arg}} requires {.cls coracle_obj}."))
  }

}

validate_label_input <- function(input,
                                 arg = caller_arg(input),
                                 call = caller_env()) {
  if (!is.null(input) && !is_scalar_character(input)) {
    cli_abort(c("x" = "{.arg {arg}} requires {.cls character} scalar."))
  }

}

validate_join_overlap <- function(x,
                                  y,
                                  arg_x = caller_arg(x),
                                  arg_y = caller_arg(y),
                                  call = caller_env()){



}


#' Title
#'
#' @param x
#' @param y
#' @param z
#' @param ...
#' @param x_name
#' @param x_join
#' @param x_vals
#' @param x_labl
#' @param y_name
#' @param y_join
#' @param y_vals
#' @param y_labl
#' @param z_name
#' @param z_join
#' @param z_vals
#' @param z_labl
#' @param method
#'
#' @returns
#' @export
#'
#' @examples
coracle <- function(x,
                    y = NULL,
                    z = NULL,
                    ...,

                    x_label = NULL,
                    y_label = NULL,
                    z_label = NULL,

                    method = "spearman") {

  validate_coracle_input(x)
  validate_coracle_input(y)
  validate_coracle_input(z)

  validate_label_input(x_label)
  validate_label_input(y_label)
  validate_label_input(z_label)

  arg_match(method, c("spearman", "pearson", "kendall"))

  result <- NULL

  if (!is.null(x) && !is.null(y) && !is.null(z)) {
    #map_pair_part(x, y, z)
  } else if (!is.null(x) && !is.null(y)) {
    map_pair(x, y)
  } else if (!is.null(x) && !is.null(z)) {
    #map_auto_part(x, z)
  } else if (!is.null(x)) {
    #map_auto(x)
  } else {
    cli_abort(c("x" = "This case should be unreachable!"))
  }

}

map_pair <- function(a, b) {

  validate_join_overlap(a, b)

  future_map(a$data, \(x_dat) map(b$data, \(y_dat) correlate(x_dat, y_dat, a, b)))

}

correlate <- function(a_dat, b_dat, a, b) {

  # avoids `join_by`'s input validation
  a_join <- a$join_col
  b_join <- b$join_col

  corr_data <- full_join(a_dat,
                         b_dat,
                         by = join_by(!!sym(a_join) == !!sym(b_join)),
                         suffix = c(a$id, b$id))

}
