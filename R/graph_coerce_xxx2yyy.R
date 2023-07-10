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
#' * "gn": "graphNEL";
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
## ##### graphNEL to something #####
## ########################################################

#' @export
#' @rdname graph-coerce-api
g_gn2dm_ <- function(object) {
    .check.is.graphNEL(object)
    as(igraph::as_adjacency_matrix(as(object, "igraph")), "matrix")

}

#' @export
#' @rdname graph-coerce-api
g_gn2sm_ <- function(object) {
    .check.is.graphNEL(object)
    igraph::as_adjacency_matrix(as(object, "igraph"))
}

#' @export
#' @rdname graph-coerce-api
g_gn2ig_ <- function(object){
    .check.is.graphNEL(object)
    igraph::igraph.from.graphNEL(object)
}

## ########################################################
## ##### dense matrix to something #####
## ########################################################

#' @export
#' @rdname graph-coerce-api
g_dm2gn_ <- function(object) {
    as(object, "graphNEL")
}

#' @export
#' @rdname graph-coerce-api
g_dm2sm_ <- function(object) {
    M2dgCMatrix__(object)
}

#' @export
#' @rdname graph-coerce-api
g_dm2ig_ <- function(object){
    mode <- if (isSymmetric(object)) "undirected" else "directed"
    gg <- igraph::graph.adjacency(object, mode=mode)    
    igraph::V(gg)$label <- igraph::V(gg)$name <- colnames(object)
    gg
}

## ########################################################
## sparse matrix to something
## ########################################################

#' @export
#' @rdname graph-coerce-api
g_sm2gn_ <- function(object) {
    as(object, "graphNEL")
}

#' @export
#' @rdname graph-coerce-api
g_sm2dm_ <- function(object) {
    M2matrix__(object)
}

#' @export
#' @rdname graph-coerce-api
g_sm2ig_ <- g_dm2ig_

## #' @rdname graph-coerce-api
## xm2ig <- dm2ig_


## ########################################################
## igraph to something
## ########################################################

#' @export
#' @rdname graph-coerce-api
g_ig2gn_ <- function(object) {
    igraph::igraph.to.graphNEL(object)
}

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
g_xm2gn_ <- function( object ){ ## M | graphNEL
    .check.is.matrix( object )
    as(object , "graphNEL")
}

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

## ###############################################
## graphNEL to something
## ###############################################

#' @export
#' @rdname graph-coerce-api
g_gn2xm_ <- function(object, result="matrix"){
    switch(result,
           "matrix"={g_gn2dm_(object)},
           "Matrix"=,
           "dgCMatrix"={g_gn2sm_(object)})
}

#' @export
#' @rdname graph-coerce-api
g_gn2ftM_ <- function(object){
    adjList2ftM__(graph::edges(object))
}

#' @export
#' @rdname graph-coerce-api 
g_gn2tfM_ <- function(object){
    adjList2tfM__(graph::edges(object))
}


#' @rdname graph-coerce-api
#' @section Synonymous functions:
#'
#' For backward compatibility with downstream packages we have the
#' following synonymous functions:
#'
#' * graphNEL2adjMAT = g_gn2xm_ (Used in HydeNet)
#' 
#' * graphNEL2M = g_gn2xm_ (Used in simPATHy)
#' 
#' * M2graphNEL = g_xm2gn_ (Used in simPATHy)
#'
#' @aliases M2graphNEL graphNEL2M graphNEL2adjMAT

#' @export
graphNEL2adjMAT <- g_gn2xm_

#' @export
graphNEL2M <- g_gn2xm_

#' @export
M2graphNEL <- g_xm2gn_
