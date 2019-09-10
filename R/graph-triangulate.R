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
#' 
#' @details There are two type of functions: \code{triang} and \code{triangulate}
#'
#' The workhorse is the \code{triangulateMAT} function.
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
#' 
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
#' @param order Elimation order; a character vector or numeric vector.
#' @param control A list controlling the triangulation; see 'examples'.
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
#' @seealso \code{\link{ug}}, \code{\link{dag}}, \code{\link{mcs}},
#'     \code{\link{mcsMAT}}, \code{\link{rip}}, \code{\link{ripMAT}},
#'     \code{\link{moralize}}, \code{\link{moralizeMAT}}
#' @keywords utilities
#' @examples
#' 
#' ## graphNEL
#' uG1 <- ug(~a:b + b:c + c:d + d:e + e:f + f:a)
#' uG2 <- ug(~a:b + b:c + c:d + d:e + e:f + f:a, result="matrix")
#' uG3 <- ug(~a:b + b:c + c:d + d:e + e:f + f:a, result="dgCMatrix")
#' 
#' ## Default triangulation: minimum clique weight heuristic
#' # (default is that each node is given the same weight):
#' 
#' tuG1 <- triang(uG1)
#' ## Same as
#' triang_mcwh(uG1)
#'
#' ## Alternative: Triangulation from a desired elimination order
#' # (default is that the order is order of the nodes in the graph):
#' 
#' triang(uG1, control=list(method="elo"))
#' ## Same as:
#' triang_elo(uG1)
#' 
#' ## More control: Define the number of levels for each node:
#' tuG1 <- triang(uG1, control=list(method="mcwh", nLevels=c(2, 3, 2, 6, 4, 9))) 
#' tuG1 <- triang_mcwh(uG1, nLevels=c(2, 3, 2, 6, 4, 9))
#'
#' tuG1 <- triang(uG1, control=list(method="elo", order=c("a", "e", "f")))
#' tuG1 <- triang_elo(uG1, order=c("a", "e", "f"))
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
#' uG2 <- ug(~a:b + b:c + c:d + d:e + e:f + f:a, result="dgCMatrix")
#' tuG2 <- triangulate(uG2)
#' 
#' @export triangulate
#' 

#' @rdname graph-triangulate
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
                  
    mm <- coerceGraph(object, "matrix")
    if (!is.UGMAT(mm)) stop("Graph must be undirected\n")
    
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


## ----- triang -----

## GENERIC FUNCTIONS

#' @rdname graph-triangulate
triang_mcwh <- function(object, ...)
    UseMethod("triang_mcwh")

#' @rdname graph-triangulate
triang_elo <- function(object, ...)
    UseMethod("triang_elo")

#' @rdname graph-triangulate
triang <- function(object, ...)
    UseMethod("triang")

#' @rdname graph-triangulate
triang.default <- function(object, control=list(), ...){
    ctrl <- list(method="mcwh", nLevels=NULL) ## DEFAULT is mcwh
    v    <- setdiff(names(ctrl), names(control))
    control <- c(control, ctrl[v])
    switch(control$method,
           "mcwh"={triang_mcwh(object, nLevels=control$nLevels)},
           "elo" ={triang_elo (object, order=control$order)}
           )
}

#' @rdname graph-triangulate
triang_mcwh.default <- function(object, nLevels=NULL, result=NULL, check=TRUE, ...){
    triangulate.default(object, nLevels, result, check, ...)
}

#' @rdname graph-triangulate
triang_elo.default <- function(object, order=NULL, result=NULL, check=TRUE, ...){
    zzz <- c("graphNEL", "igraph", "matrix", "dgCMatrix")
    
    if (!inherits(object, zzz)) stop("Invalid class of 'object'\n")
    
    mm <- coerceGraph( object, "dgCMatrix" )
    if ( !is.UGMAT(mm) ) stop("Graph must be undirected\n")
    
    cls <- match.arg(class( object ), zzz )
    if (is.null( result )) result <- cls

    if (!check)
        mm <- triang_eloMAT( mm, order=order )
    else {
        if (length(mcsMAT(mm)) == 0)
            mm <- triang_eloMAT( mm, order=order )
    }
    
    coerceGraph(mm, result)        
}


#' @rdname graph-triangulate
triangulateMAT <- function(amat, nLevels=rep(2, ncol(amat)), ...){
    if (is.null(nLevels))
        nLevels <- rep( 2, ncol(amat) )

    .c_triang_mcwh_MAT_( amat, nLevels )
}

#' @rdname graph-triangulate
triang_mcwhMAT_ <- function(amat, nLevels=rep(2, ncol(amat)), ...){
    if (is.null(nLevels))
        nLevels <- rep(2, ncol(amat))

    .c_triang_mcwh_MAT_( amat, nLevels )
}

#' @rdname graph-triangulate
triang_eloMAT_ <- function(amat, order){
    if (is.null(order))
        order <- 1:nrow(amat)
    
    out <- .c_triang_elo_MAT_(amat, order - 1)    
    dimnames(out) <- dimnames(amat)
    out
}




#' @rdname graph-triangulate
triang_eloMAT <- function(amat, order=NULL){
    if (!inherits(amat, c("matrix", "dgCMatrix")))
        stop("'amat' must be dense or sparse dgCMatrix")

    if (!is.null(order) && !inherits(order, c("character", "numeric", "integer")))
        stop("'order' must be NULL or a character, numeric or integer vector")
    ## FIXME: If order does contain non integer values, then fail

    if (inherits(amat, "matrix")) amat <- as(amat, "dgCMatrix")

    vn <- rownames(amat)

    if (is.null(order)) order <- seq_along(vn)
    else if (is.character(order)) order <- match(order, vn)

    if (any(is.na(order))) stop("NAs in order\n")
    if (max(order) > length(vn)) stop("max of order too large")
    if (min(order) < 1) stop("min of order too large")
    
    if (length(order) < length(vn))
        order <- c(order, seq_along(vn)[-order])

    triang_eloMAT_(amat, order)
}










