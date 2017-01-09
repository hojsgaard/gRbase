## #################################################################
##
## Graph triangulation
##
## #################################################################

#' @title Triangulation of an undirected graph
#' 
#' @description This function will triangulate an undirected graph by
#'     adding fill-ins.
#'
#' @name graph-triangulate
#' @details The workhorse is the \code{triangulateMAT} function.
#' 
#' The triangulation is made so as the total state space is kept low
#' by applying a minimum clique weight heuristic: When a fill-in is
#' necessary, the algorithm will search for an edge to add such that
#' the complete set to be formed will have as small a state-space as
#' possible. It is in this connection that the \code{nLevels} values
#' are used.
#' 
#' Default (when \code{nLevels=NULL}) is to take \code{nLevels=2} for all
#' nodes. If \code{nLevels} is the same for all nodes then the heuristic aims
#' at keeping the clique sizes small.
#' 
#' @aliases triangulate triangulate.default triangulateMAT
#' @param object An undirected graph represented either as a \code{graphNEL}
#'     object, an \code{igraph}, a (dense) \code{matrix}, a (sparse)
#'     \code{dgCMatrix}.
#' @param nLevels The number of levels of the variables (nodes) when these are
#'     discrete. Used in determining the triangulation using a
#'     "minimum clique weight heuristic". See section 'details'.
#' @param result The type (representation) of the result. Possible values are
#'     \code{"graphNEL"}, \code{"igraph"}, \code{"matrix"}, \code{"dgCMatrix"}.
#'     Default is the same as the type of \code{object}.
#' @param check If \code{TRUE} (the default) it is checked whether the graph is
#'     triangulated before doing the triangulation; gives a speed up if \code{FALSE}
#' @param ... Additional arguments, currently not used.
#' @param amat Adjacency matrix; a (dense) \code{matrix}, or a (sparse)
#'     \code{dgCMatrix}.
#' @return A triangulated graph represented either as a \code{graphNEL}, a
#'     (dense) \code{matrix} or a (sparse) \code{dgCMatrix}.
#' @note Care should be taken when specifying \code{nLevels} for other
#'     representations than adjacency matrices: Since the \code{triangulateMAT}
#'     function is the workhorse, any other representation is transformed to an
#'     adjacency matrix and the order of values in \code{nLevels} most come in
#'     the order of the nodes in the adjacency matrix representation.
#' 
#' Currently there is no check for that the graph is undirected.
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @seealso \code{\link{ug}} \code{\link{dag}} \code{\link{mcs}},
#'     \code{\link{mcsMAT}} \code{\link{rip}}, \code{\link{ripMAT}},
#'     \code{\link{moralize}}, \code{\link{moralizeMAT}}
#' @keywords utilities
#' @examples
#' 
#' ## graphNEL
#' uG1 <- ug(~a:b + b:c + c:d + d:e + e:f + f:a)
#' tuG1 <- triangulate(uG1)
#' 
#' ## adjacency matrix
#' uG2 <- ug(~a:b + b:c + c:d + d:e + e:f + f:a, result="matrix")
#' tuG2 <- triangulate(uG2)
#' 
#' ## adjacency matrix (sparse)
#' uG2 <- ug(~a:b + b:c + c:d + d:e + e:f + f:a, result="Matrix")
#' tuG2 <- triangulate(uG2)
#' 
#' @export triangulate
triangulate <- function(object, ...)
{
  UseMethod("triangulate")
}

## FIXME: triangulate: Need clever choice of matrix-representation
## FIXME: (Sparse/dense)
#' @rdname graph-triangulate
triangulate.default <- function(object, nLevels=NULL, result=NULL, check=TRUE, ...)
{
    zzz <- c("graphNEL", "igraph", "matrix", "dgCMatrix")
    
    if (!inherits(object, zzz)) stop("Invalid class of 'object'\n")
                  
    mm <- coerceGraph( object, "matrix" )
    if ( !is.UGMAT(mm) ) stop("Graph must be undirected\n")
    
    cls <- match.arg(class( object ), zzz )
    if (is.null( result )) result <- cls

    if (!check)
        mm <- triangulateMAT( mm, nLevels=nLevels )
    else {
        if (length(mcsMAT(mm)) == 0)
            mm <- triangulateMAT( mm, nLevels=nLevels )
    }
    
    coerceGraph(mm, result)    
}

#' @rdname graph-triangulate
triangulateMAT <- function(amat, nLevels=rep(2, ncol(amat)), ...){
    if (is.null(nLevels)) nLevels <- rep( 2, ncol(amat) )
    triangulateMAT_( amat, nLevels )
}









