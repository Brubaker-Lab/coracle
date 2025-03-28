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

corr_result <- function(name_a,
                        name_b = NA_character_,
                        name_c = NA_character_,
                        stat_value = NA,
                        p = NA,
                        n = NA,
                        message = NA_character_,
                        method
){

  stat_name <- list(spearman = "rho",
                    kendall = "tau",
                    pearson = "cor")

  result_front <- list(a = name_a,
                 b = name_b)

  if(!is.na(name_c)){
    result_front <- c(result_front, c = name_c)
  }

  statistic <- stat_value
  names(statistic) <- stat_name[[method]]

  result_back <- list(
    p = p,
    n = n,
    message = message
  )

  c(result_front, statistic, result_back)

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
    cli_abort(c("x" = "Pairwise partial correlation has not been implemented yet"))
    #map_pair_part(x, y, z)
  } else if (!is.null(x) && !is.null(y)) {
    map_pair(x, y)
  } else if (!is.null(x) && !is.null(z)) {
    cli_abort(c("x" = "Partial correlation has not been implemented yet"))
    #map_auto_part(x, z)
  } else if (!is.null(x)) {
    cli_abort(c("x" = "Autocorrelation has not been implemented yet"))
    #map_auto(x)
  } else {
    cli_abort(c("x" = "This case should be unreachable!"))
  }

}

map_pair <- function(a, b) {

  validate_join_overlap(a, b)

  future_map(a$data, \(a_data) map(b$data, \(b_data) correlate(a_data, b_data, a, b)))

}

outer_loop <- function(a,b){

  future_map(a$data,
             \(a_data){

              if(is_values_constant){
                return
              }



             })
}

is_values_constant <- function(a_data, a){

  a_data |>
    pluck(a$vals_col) |>
    unique() |>
    length() == 1

}

correlate <- function(a_data, b_data, a, b) {

  # avoids `join_by`'s input validation
  a_join <- a$join_col
  b_join <- b$join_col

  corr_data <- full_join(a_dat,
                         b_dat,
                         by = join_by(!!sym(a_join) == !!sym(b_join)),
                         suffix = c(a$id, b$id))

}
