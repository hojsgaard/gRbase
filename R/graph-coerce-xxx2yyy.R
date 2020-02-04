## ##################################################################
##
#' @title API for coercing graph representations
#' @description API for coercing graph representations.
#' @name graph_coerce_api
##
## ###################################################################
#'
#' @param object An object representing a graph
#' @param result Either 'matrix' (dense) or 'dgCMatrix' (sparse, can
#'     be abbreviated to 'Matrix').
#'
#' @details No checking is made. In the function the following names are used:
#' "ig": "igraph";
#' "gn": "graphNEL";
#' "sm": "dgCMatrix" (sparse matrix);
#' "dm": "matrix" (dense matrix)
#' 
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @seealso \code{\link{ug}}, \code{\link{dag}}
#' @keywords utilities
#' 

## ########################################################
## ##### graphNEL to something #####
## ########################################################

#' @export
#' @rdname graph_coerce_api
gn2dm_ <- function(object) {
    .check.is.graphNEL(object)
    adjList2matrix__(graph::edges(object))
}

#' @export
#' @rdname graph_coerce_api
gn2sm_ <- function(object) {
    .check.is.graphNEL(object)
    adjList2dgCMatrix__(graph::edges(object))
}

#' @export
#' @rdname graph_coerce_api
gn2ig_ <- function(object){
    .check.is.graphNEL(object)
    gg <- igraph::igraph.from.graphNEL(object)
    igraph::V(gg)$label <- igraph::V(gg)$name
    gg
}

## ########################################################
## ##### dense matrix to something #####
## ########################################################

#' @export
#' @rdname graph_coerce_api
dm2gn_ <- function(object) as(object, "graphNEL")

#' @export
#' @rdname graph_coerce_api
dm2sm_ <- function(object) M2dgCMatrix__(object)

#' @export
#' @rdname graph_coerce_api
dm2ig_ <- function(object){
    mode <- if (isSymmetric(object)) "undirected" else "directed"
    gg <- igraph::graph.adjacency(object, mode=mode)    
    igraph::V(gg)$label <- igraph::V(gg)$name <- colnames( object )
    gg
}

## ########################################################
## sparse matrix to something
## ########################################################

#' @export
#' @rdname graph_coerce_api
sm2gn_ <- function(object) as(object, "graphNEL")

#' @export
#' @rdname graph_coerce_api
sm2dm_ <- function(object) M2matrix__(object)

#' @export
#' @rdname graph_coerce_api
sm2ig_ <- dm2ig_

## #' @rdname graph_coerce_api
## xm2ig <- dm2ig_


## ########################################################
## igraph to something
## ########################################################

#' @export
#' @rdname graph_coerce_api
ig2gn_ <- function(object) igraph::igraph.to.graphNEL(object)

#' @export
#' @rdname graph_coerce_api
ig2dm_ <- function(object) M2matrix__(igraph::get.adjacency(object))

#' @export
#' @rdname graph_coerce_api
ig2sm_ <- function(object) igraph::get.adjacency(object)


## ##### END of api #####


## ###############################################
## matrix/dgCMatrix to something
## ###############################################

#' @export
#' @rdname graph_coerce_api
xm2gn_ <- function( object ){ ## M | graphNEL
    .check.is.matrix( object )
    as(object , "graphNEL")
}

#' @export
#' @rdname graph_coerce_api
xm2ig_ <- function( object ){ ## M | igraph
    .check.is.matrix( object )
    as(object , "igraph")
}

#' @export
#' @rdname graph_coerce_api
xm2dm_ <- function( object ){  ## M
    .check.is.matrix( object )
    M2matrix__(object)
}

#' @export
#' @rdname graph_coerce_api
xm2sm_ <- function( object ){ ## M
    .check.is.matrix( object )
    M2dgCMatrix__(object)
}

#' @export
#' @rdname graph_coerce_api
xm2xm_ <- function(object, result="matrix"){
    switch(result,
           "matrix"={xm2dm_(object)},
           "Matrix"=,
           "dgCMatrix"={xm2sm_(object)})
}

## ###############################################
## graphNEL to something
## ###############################################

#' @export
#' @rdname graph_coerce_api
gn2xm_ <- function(object, result="matrix"){
    switch(result,
           "matrix"={gn2dm_(object)},
           "Matrix"=,
           "dgCMatrix"={gn2sm_(object)})
}

#' @export
#' @rdname graph_coerce_api
gn2ftM_ <- function(object){
    adjList2ftM__(graph::edges(object))
}

#' @export
#' @rdname graph_coerce_api
gn2tfM_ <- function(object){
    adjList2tfM__(graph::edges(object))
}


.check.is.matrix <- function(x){
    if (!inherits(x, c("matrix", "dgCMatrix")))
        stop("Input must be a matrix or a dgCMatrix\n")
}

.check.is.graphNEL <- function(x){
    if (!inherits(x, "graphNEL"))
        stop("'x' not a graphNEL object...")    
}
