######################################################################
##
#' @title Functions from Graphical Modelling with R book
#'
#' @description Functions that must be retained to make code from
#'     gmwr-book work
#'
#' @name gmwr_book
##
######################################################################

## Note to self: Check xxx_downstream content for function from gmwr
## book that must be retained.

#'
#' @param object An object to be coerced.
## ' @param result The format to be coerced to.
#' 
NULL

#' @export
#' @rdname gmwr_book
as.adjMAT       <- function(object) {
    as(object, "matrix")
}

## as.adjMAT       <- g_gn2xm_
