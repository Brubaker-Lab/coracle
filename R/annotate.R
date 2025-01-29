as_ <- function(.value, .attr = TRUE, .class = TRUE, .reqf = NULL){
  function(input){

    for (reqf in .reqf) {
      reqf(input)
    }

    if(.class){
      class(input) <- c(class(input), .value)
    }

    if(.attr){
      attr(input, "coracle") <- .value
    }
    input
  }
}

is_ <- function(value){
  function(input){
    purrr::map_lgl(value,
                   \(v)
    (v %in% class(input)) || (!is.null(attr(input, "coracle")) && attr(input, "coracle") == v))|>
      any()
  }

}

un_ <- function(value){
  function(input){
    class(input) <- setdiff(class(input), value)
    attr(input, "coracle") <- NULL
    input
  }
}

req_data.frame <- function(x,
                           arg = caller_arg(x),
                           call = caller_env()){

  if(!is.data.frame(x)){
    cli_abort(c("x" = "{.arg {arg}} requires {.cls data.frame}."),
              call=call)
  }
}

name <- "name"

as_name <- as_(name)
is_name <- is_(name)
un_name <- un_(name)

join <- "join"

as_join <- as_(join)
is_join <- is_(join)
un_join <- un_(join)


vals <- "vals"

as_vals <- as_(vals, .attr = F)
is_vals <- is_(vals)
un_vals <- un_(vals)

coracle_df <- "coracle_df"

as_coracle_df <- as_(coracle_df, .attr = F, .reqf = c(req_data.frame))
is_coracle_df <- is_(coracle_df)
un_coracle_df <- un_(coracle_df)

is_coracle <- is_(c(join, name, vals, coracle_df))
un_coracle <- un_(c(join, name, vals, coracle_df))


