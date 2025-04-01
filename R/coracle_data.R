#' R6 Class Representing Data for Correlation
#'
#' @description
#' A `coracle_data` nodes stores data for correlation in a tree.
#'
#' @details
#' Branches are based on grouping variables. Data is stored as chunks in the leaf nodes of the tree.
#'

# Object definition ------------

coracle_data <- R6::R6Class(
  "coracle_data",
  public = list(

    ## Fields ------------

    #' @field version Package version used to generate node.
    version = NULL,

    #' @field id Unique hash ID for node.
    id = NULL,

    #' @field chunk Atomic unit of data stored in leaves.
    chunk = NULL,

    #' @field children Child `coracle_data` node(s).
    children = NULL,

    #' @field parent Parent `coracle_data` node.
    parent = NULL,

    #' @field grps_cols Grouping column(s) in data for this node and it's children.
    grps_cols = NULL,

    #' @field join_col Joining column in data.
    join_col = NULL,

    #' @field join_vals Joining column values.
    join_vals = NULL,

    #' @field vals_col Values column in data.
    vals_col = NULL,

    #' @field other_cols Other column(s) present in data.
    other_cols = NULL,

## Initalization ------------

    #' @description
    #' Create a new `coracle_data` node
    #'
    #'
    #' @param data
    #' @param grps_cols
    #' @param join_col
    #' @param vals_cols
    #' @param vals_labl
    #' @param call
    #'
    #' @returns
    #'
    #' @examples
    initialize = function(data,
                          grps_cols,
                          join_col,
                          vals_cols,
                          vals_labl,
                          call = caller_env()) {

    }
  ),

## Active Bindings ------------

  active = list(
    #' @description
    #' Get list of data chunks.
    #'
    #'
    #' @param node
    #'
    #' @returns Stored data as a list of `data.frames` by chunk.
    #'
    #' @examples
    chunks = function(node = self) {

    },


    #' @description
    #' Get data as a data frame.
    #'
    #'
    #' @param node
    #'
    #' @returns Stored data as `data.frame`.
    #'
    #' @examples
    data = function(node = self) {

    },

    #' @description
    #' Get list of leaf nodes with data.
    #'
    #' @param node
    #'
    #' @returns List of `coracle_data` nodes storing data.
    #'
    #' @examples
    leaves = function(node = self) {

    }
  )
)
