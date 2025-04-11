#' R6 Class Representing Data for Correlation with the `coracle` package
#'
#' @description
#' A `coracle_data` nodes stores data for correlation in a tree.
#'
#' @details
#' Branches are based on grouping variables. Data is stored as chunks in the leaf nodes of the tree.
#'
#' @export
coracle_data <- R6::R6Class(
  "coracle_data",

  ## Public ------------

  public = list(
    ### Fields ------------

    meta = NULL,
    cols = NULL,
    parent = NULL,
    #' @field children List of child `coracle_data` node(s).
    children = NULL,

    #' @field chunk `data.frame` of data for correlation.
    chunk = NULL,

    #' @field grps_vals Named list of grouping column(s) and chunk value(s).
    grps_vals = NULL,

    #' @field chunk_flags Boolean indication of whether this node is valid for correlation.
    chunk_flags = NULL,

    #' @description
    #' Create a new `coracle_data` node
    #'
    #'
    #' @param data A `data.frame` of data to prepare for correlation.
    #' @param grps <[`tidy-select`][dplyr_tidy_select]> expressions identifying grouping column(s)
    #' @param join A <[`tidy-select`][dplyr_tidy_select]> expression identifying one joining column.
    #' @param vals <[`tidy-select`][dplyr_tidy_select]> expressions identifying value column(s).
    #' @param ... Unused
    #' @param labl_cols (Optional) Character scalar column name for long-format groups.
    #' @param labl_vals (Optional) Character scalar column name for long-format values.
    #' @param parent (Optional) A `coracle_data` node.
    #' @param call Caller environment for error reporting.
    #'
    #' @returns A `coracle_data` root node.
    initialize = function(data = NULL,
                          grps = NULL,
                          join = NULL,
                          vals = NULL,
                          ...,
                          labl_cols = "cols",
                          labl_vals = "vals",
                          parent = NULL,
                          call = caller_env()) {
      ## Data ------------

      if (!is.data.frame(data))
        cli_abort(c("x" = "{.arg data} requires a {.cls data.frame}."), call = call)

      ## Parent ------------

      if (!is.null(parent) && !("coracle_data" %in% class(parent)))
        cli_abort(c("x" = "{.arg parent} requires a {.cls coracle_data}."), call = call)

      self$parent = parent

      ## Meta and Time ------------

      self$meta$id <- hash(as.numeric(Sys.time())) |> str_sub(-7)

      self$meta$version <- as.character(packageVersion("coracle"))

      self$meta$future$n_workers <- nbrOfWorkers()

      self$meta$time$start <- Sys.time()

      self$meta$data$n_col <- ncol(data)

      self$meta$data$n_row <- nrow(data)

      ## Tidyselections ------------

      ### Group Column

      grps_pos <- eval_select(enquo(grps), data)

      self$cols$grps <- names(grps_pos)

      ### Join Column

      join_pos <- eval_select(enquo(join), data)

      if (length(join_pos) != 1)
        cli_abort(c("x" = "{.arg join} must select exactly one column."), call = call)

      self$cols$join <- names(join_pos)

      ### Value Column(s)

      vals_pos <- eval_select(enquo(vals), data)

      if (length(vals_pos) < 1)
        cli_abort(c("x" = "{.arg vals} requires a selection."), call = call)

      if (length(intersect(grps_pos, join_pos)) != 0)
        cli_abort(c("x" = "{.arg grps} and {.arg join} may not share any column(s)."),
                  call = call)

      if (length(intersect(grps_pos, vals_pos)) != 0)
        cli_abort(c("x" = "{.arg grps} and {.arg vals} may not share any column(s)."),
                  call = call)

      if (length(intersect(join_pos, vals_pos)) != 0)
        cli_abort(c("x" = "{.arg join} and {.arg vals} may not share any column(s)."),
                  call = call)

      ### Other Column(s)

      self$cols$other <- subset(data, select = -c(join_pos, vals_pos, grps_pos)) |>
        names()

      ## Label(s) ------------

      if (!is_scalar_character(labl_cols))
        cli_abort(c("x" = "{.arg labl_cols} requires a {.cls character} scalar."),
                  call = call)

      if (!is_scalar_character(labl_vals))
        cli_abort(c("x" = "{.arg labl_vals} requires a {.cls character} scalar."),
                  call = call)

      ## Format Data ------------

      if (length(grps_pos) > 0 && length(vals_pos) == 1) {
        # Long

        self$cols$vals <- names(vals_pos)

      } else if (length(grps_pos) == 0 && length(vals_pos) > 1) {
        # Wide

        data <- data |> pivot_longer(cols = all_of(names(vals_pos)),
                                     names_to = labl_cols,
                                     values_to = labl_vals)

        self$cols$grps <- c(self$cols$grps, labl_cols)
        self$cols$vals <- labl_vals

      } else {
        cli_abort(
          c(
            "x" = "Error with {.arg grps} and {.arg vals} selections",
            "i" = "Data must be either:",
            ">" = "{.strong Wide} with zero (0) {.arg grps} {.emph and} multiple (2+) {.arg vals}, or",
            ">" = "{.strong Long} with one (1) {.arg grps} {.emph and} one (1) {.arg vals}"
          )
        )
      }

      ### Reorder columns -> grps, join, vals, other

      data <- data |>
        relocate(all_of(self$cols$grps),
                 self$cols$join,
                 self$cols$vals,
                 all_of(self$cols$other))

      self$meta$data$n_join <- length(unique(data[[self$cols$join]]))

      self$meta$data$n_vals <- length(unique(data[[self$cols$vals]]))

      self$meta$data$n_grps <- self$cols$grps |>
        set_names() |>
        map( ~ length(unique(data[[.]])))

      ## Initialize Children, if appropriate ------------

      for (col in self$cols$grps)
      {
        col_vals <- data[[col]] |>
          unique() |>
          sort(na.last = T)

        if (length(col_vals) == 1) {
          next

        } else {
          data <- data |>
            arrange(!!sym(col)) |>
            group_by(!!self$meta$id := !!sym(col)) |>
            tidyr::nest() |>
            ungroup() |>
            mutate(
              grps = list(self$cols$grps),
              join = self$cols$join,
              vals = self$cols$vals,
              labl_cols = labl_cols,
              labl_vals = labl_vals,
              parent = list(self)
            )

          children <- data |>
            select(-!!self$meta$id) |>
            future_pmap(coracle_data$new, .progress = T)

          names(children) <- replace(data[[self$meta$id]], is.na(data[[self$meta$id]]), "NA")

          self$children <- children

          rm(data)

          break

        }
      }

      ## If not a leaf node, store data ------------

      if (is.null(self$children)) {
        flags <- list()

        join_values <- data[[self$cols$join]] |>
          unique() |>
          sort() # drops NAs

        if (length(join_values) < 3)
          flags <- append(flags, "Less than 3 join values.")

        vals_values <- data[[self$cols$vals]] |>
          unique() |>
          sort() # drops NAs

        if (length(vals_values) <= 1)
          flags <- append(flags, "Constant values.")

        if (length(flags) > 0)
          self$chunk_flags <- flags

        self$grps_vals <- self$cols$grps |>
          set_names() |>
          map(\(x) unique(data[[x]]))

        self$chunk <- data
      }

      ## Meta and Time ------------

      self$meta$time$end <- Sys.time()

      self$meta$time$total <- self$meta$time$end - self$meta$time$start

    }
  ),

  ## Active Bindings ------------

  active = list(
    #' @description
    #' Active binding for all stored data as a `data.frame`.
    #'
    #' @param node The `coracle_data` root node for the search.
    #'
    #' @returns Data as a `data.frame`.
    data = function(node = self) {
      if (!is.null(node$chunk)) {
        return(node$chunk)
      } else {
        temp <- self$chunks

        if (!is.data.frame(temp))
          temp <- temp |> list_rbind()

        return(temp)
      }
    },

    #' @description
    #' Active binding for list of `coracle_data` leaf nodes.
    #'
    #' @param node The `coracle_data` root node for the search.
    #'
    #' @returns List of `coracle_data` leaf nodes.
    leaves = function(node = self) {
      if (!is.null(node$chunk)) {
        return(node)
      } else {
        map(node$children, \(x) x$leaves) |> list_flatten()
      }
    },

    #' @description
    #' Active binding for list of `coracle_data` leaf nodes with data valid for correlation.
    #'
    #' @param node The `coracle_data` root node for the search.
    #'
    #' @returns List of `coracle_data` leaf nodes with data valid for correlation.
    leaves_valid = function(node = self) {
      if (!is.null(node$chunk)) {
        if (is_empty(node$chunk_flags)) {
          return(node)
        } else {
          return(NULL)
        }
      } else {
        map(node$children, \(x) x$leaves_valid) |> list_flatten() |> compact()
      }
    },

    #' @description
    #' Active binding for list of `coracle_data` leaf nodes with data invalid for correlation.
    #'
    #' @param node The `coracle_data` root node for the search.
    #'
    #' @returns List of `coracle_data` leaf nodes with data invalid for correlation.
    leaves_invalid = function(node = self) {
      if (!is.null(node$chunk)) {
        if (!is_empty(node$chunk_flags)) {
          return(node)
        } else {
          return(NULL)
        }
      } else {
        map(node$children, \(x) x$leaves_invalid) |> list_flatten() |> compact()
      }
    }
  )
)
