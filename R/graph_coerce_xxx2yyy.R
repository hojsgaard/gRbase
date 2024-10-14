## ##################################################################
##
#' @title API for coercing graph representations
#' @description API for coercing graph representations.
#' @name graph-coerce-api
##
## ###################################################################
#'
#' @param object An object representing a graph
#' @param result Either 'matrix' (dense) or 'dgCMatrix' (sparse, can
#'     be abbreviated to 'Matrix').
#'
#' @details No checking is made. In the function the following names are used:
#' 
#' * "ig": "igraph";
#' 
#' * "sm": "dgCMatrix" (sparse matrix);
#' 
#' * "dm": "matrix" (dense matrix)
#' 
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @seealso \code{\link{ug}}, \code{\link{dag}}
#' @keywords utilities
#' 

## ########################################################
## ##### dense matrix to something #####
## ########################################################

#' @export
#' @rdname graph-coerce-api
g_dm2sm_ <- function(object) {
    M2dgCMatrix__(object)
}

#' @export
#' @rdname graph-coerce-api
g_dm2ig_ <- function(object){
    mode <- if (isSymmetric(object)) "undirected" else "directed"
    if (is.null(rownames(object))){
        rownames(object) <- colnames(object) <- 1:ncol(object)
    }
    gg <- igraph::graph.adjacency(object, mode=mode)    
    igraph::V(gg)$label <- igraph::V(gg)$name <- colnames(object)
    gg
}

## ########################################################
## sparse matrix to something
## ########################################################

#' @export
#' @rdname graph-coerce-api
g_sm2dm_ <- function(object) {
    M2matrix__(object)
}

#' @export
#' @rdname graph-coerce-api
g_sm2ig_ <- g_dm2ig_


## ########################################################
## igraph to something
## ########################################################

#' @export
#' @rdname graph-coerce-api
g_ig2dm_ <- function(object) {
    M2matrix__(igraph::get.adjacency(object))
}

#' @export
#' @rdname graph-coerce-api
g_ig2sm_ <- function(object) {
    igraph::get.adjacency(object)
}

## ###############################################
## matrix/dgCMatrix to something
## ###############################################

#' @export
#' @rdname graph-coerce-api
g_xm2ig_ <- function( object ){ ## M | igraph
    .check.is.matrix( object )
    as(object , "igraph")
}

#' @export
#' @rdname graph-coerce-api
g_xm2dm_ <- function( object ){  ## M
    .check.is.matrix( object )
    M2matrix__(object)
}

#' @export
#' @rdname graph-coerce-api
g_xm2sm_ <- function( object ){ ## M
    .check.is.matrix( object )
    M2dgCMatrix__(object)
}

#' @export
#' @rdname graph-coerce-api
g_xm2xm_ <- function(object, result="matrix"){
    switch(result,
           "matrix"={g_xm2dm_(object)},
           "Matrix"=,
           "dgCMatrix"={g_xm2sm_(object)})
}


