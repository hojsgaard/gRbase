## ##################################################################
##
#' @title API for coercing graph representations
#'
#' @description API for coercing graph representations.
#'
#' @name graph-coerce-api
##
## ###################################################################
#'
#' @param xx An object representing a graph
#'
#' @details No checking is made. In the function the following names are used:
#' "ig": "igraph";
#' "gn": "graphNEL";
#' "sm": "dgCMatrix" (sparse matrix);
#' "dm": "matrix" (dense matrix)
#' 
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' ## @seealso \code{\link{ug}}, \code{\link{dag}}
#' @keywords utilities
#' @examples
#' ## FIXME examples coerce graph missing
#' 
#' @rdname graph-coerce-api
gn2dm_ <- function(xx) adjList2matrix__(graph::edges(xx))

#' @rdname graph-coerce-api
gn2sm_ <- function(xx) adjList2dgCMatrix__(graph::edges(xx))

#' @rdname graph-coerce-api
gn2ig_ <- function(xx){
    gg <- igraph::igraph.from.graphNEL(xx)
    igraph::V(gg)$label <- igraph::V(gg)$name
    gg
}

#' @rdname graph-coerce-api
dm2gn_ <- function(xx) as(xx, "graphNEL")

#' @rdname graph-coerce-api
dm2sm_ <- function(xx) M2dgCMatrix__(xx)

#' @rdname graph-coerce-api
dm2ig_ <- function(xx){
    gg <- if (isSymmetric( xx )){
        igraph::graph.adjacency( xx , mode="undirected")
    } else {
        igraph::graph.adjacency( xx , mode="directed")
    }
    igraph::V(gg)$label <- igraph::V(gg)$name <- colnames( xx )
    gg
}

#' @rdname graph-coerce-api
sm2gn_ <- function(xx) as(xx, "graphNEL")

#' @rdname graph-coerce-api
sm2dm_ <- function(xx) M2dgCMatrix__(xx)

#' @rdname graph-coerce-api
sm2ig_ <- function(xx){
    gg <- if (isSymmetric( xx )){
        igraph::graph.adjacency( xx , mode="undirected")
    } else {
        igraph::graph.adjacency( xx , mode="directed")
    }
    igraph::V(gg)$label <- igraph::V(gg)$name <- colnames( xx )
    gg
}

#' @rdname graph-coerce-api
ig2gn_ <- function(xx) igraph::igraph.to.graphNEL(xx)

#' @rdname graph-coerce-api
ig2dm_ <- function(xx) M2matrix__(igraph::get.adjacency(xx))

#' @rdname graph-coerce-api
ig2sm_ <- function(xx) igraph::get.adjacency(xx)


## -----------------------------------------------
##
## Using as_() - very experimental !!
##
## -----------------------------------------------

#' @rdname graph-coerce-api
#' @details The 'as_' functions are experimental and may well change
#'     in the future.
#' @param object A graph object
#' @param Class The desired output class
#' @examples
#'
#' g <- ug(~ 1:2+2:3)
#' as_(g, "matrix")
#' as_(g, "dgCMatrix")
#' as_(g, "igraph")
#'
#' if (require(microbenchmark)){
#' microbenchmark(
#'   as_(g, "matrix"),
#'   coerceGraph(g, "matrix"),
#'   as(g, "matrix"),
#'
#'   as_(g, "dgCMatrix"),
#'   coerceGraph(g, "dgCMatrix"),
#'   as(g, "dgCMatrix")
#' )
#' }
as_ <- function(object, Class){
    UseMethod("as_")
}

#' @rdname graph-coerce-api
as_.graphNEL <- function(object, Class){
    switch(Class,
           "graphNEL"  = {object},
           "matrix"    = {gn2dm_(object)},
           "dgCMatrix" = {gn2sm_(object)},
           "igraph"    = {gn2ig_(object)})          
}

#' @rdname graph-coerce-api
as_.matrix  <- function(object, Class){
    switch(Class,
           "graphNEL"  = {dm2gn_(object)},
           "matrix"    = {object},
           "dgCMatrix" = {dm2sm_(object)},
           "igraph"    = {dm2ig_(object)})          
}

#' @rdname graph-coerce-api
as_.dgCMatrix  <- function(object, Class){
    switch(Class,
           "graphNEL"  = {sm2gn_(object)},
           "matrix"    = {sm2dm_(object)},
           "dgCMatrix" = {object},
           "igraph"    = {sm2ig_(object)})          
}

#' @rdname graph-coerce-api
as_.igraph  <- function(object, Class){
    switch(Class,
           "graphNEL"  = {ig2gn_(object)},
           "matrix"    = {ig2dm_(object)},
           "dgCMatrix" = {ig2sm_(object)},
           "igraph"    = {object})          
}
