############################################################################
#'
#' @title Check properties of graphs.
#' 
#' @description Check if a graph is 1) a directed acyclic graph (DAG),
#'     2) a directed graph (DG), 3) an undirected graph (UG), 4) a
#'     triangulated (chordal) undirected graph (TUG). 
#'
#' @name graph_is
#' 
############################################################################
#'
#' @param object A graph represented as a `graphNEL` (graph package),
#'     an `igraph` (igraph package), an adjacency matrix or a sparse
#'     adjacency matrix (a `dgCMatrix` from the Matrix package).
#'
#' @details
#'
#' * A non-zero value at entry (i,j) in an adjacency matrix A for a
#' graph means that there is an edge from i to j. If also (j,i) is
#' non-zero there is also an edge from j to i. In this case we may
#' think of a bidirected edge between i and j or we may think of the
#' edge as being undirected.  We do not distinguish between undirected
#' and bidirected edges in the gRbase package.  On the other hand,
#' graphNEL objects from the graph package makes such a distinction
#' (the function \code{edgemode()} will tell if edges are "directed"
#' or "undirected" in a graphNEL object).
#' 
#' * The function \code{is_ug()} checks if the adjacency matrix is
#' symmetric (If applied to a graphNEL, the adjacency matrix is
#' created and checked for symmetry.)
#' 
#' * The function \code{is_tug()} checks if the graph is undirected and
#' triangulated (also called chordal) by checking if the adjacency matrix is
#' symmetric and the vertices can be given a perfect ordering using maximum
#' cardinality seach.
#' 
#' * The function \code{is_dg()} checks if a graph is directed, i.e., that there
#' are no undirected edges. This is done by computing the elementwise product
#' of A and the transpose of A; if there are no non--zero entries in this
#' product then the graph is directed.
#' 
#' * The function \code{is_dag()} will return \code{TRUE} if all edges are
#' directed and if there are no cycles in the graph. (This is checked by
#' checking if the vertices in the graph can be given a topological ordering
#' which is based on identifying an undirected edge with a bidrected edge).
#' 
#' * There is a special case, namely if the graph has no edges at all (such that
#' the adjacency matrix consists only of zeros). Such a graph is both
#' undirected, triangulated, directed and directed acyclic.
#'
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @seealso \code{\link{dag}}, \code{\link{ug}}
#' @keywords utilities
#' @examples
#' 
#' ## DAGs
#' dag_  <- dag(~ a:b:c + c:d:e)
#' 
#' ## Undirected graphs
#' ug_  <- ug(~a:b:c + c:d:e)
#' 
#' ## Is graph a DAG?
#' is_dag(dag_)
#' is_dag(ug_)
#' 
#' ## Is graph an undirected graph
#' is_ug(dag_)
#' is_ug(ug_)
#'  
#' ## Is graph a triangulated (i.e. chordal) undirected graph
#' is_tug(dag_)
#' is_tug(ug_)
#' 
#' ## Example where the graph is not triangulated
#' ug2_  <- ug(~ a:b + b:c + c:d + d:a)
#' is_tug(ug2_)
#' 

#' @export
#' @rdname graph_is
is_dag <- function(object){
    UseMethod("is_dag")
}

## #' @export
## is_dag.graphNEL <- function(object){
##     is_dagMAT(as(object, "matrix"))
## }

#' @export
is_dag.igraph <- function(object){
     ## is_dagMAT(as(object, "matrix"))
    igraph::is_dag(object)
}


#' @export
is_dag.default <- function( object ){
    .check.is.matrix(object)
    isdagMAT_(object)
}

#' @export
#' @rdname graph_is
is_dagMAT <- function(object){
    isdagMAT_(object)
}


## ######################################

#' @export
#' @rdname graph_is
is_ug <- function(object){
    UseMethod("is_ug")
}

## #' @export
## is_ug.graphNEL <- function(object){
##     isugMAT_(as(object, "matrix"))
## }

#' @export
is_ug.igraph <- function(object){
    isugMAT_(as(object, "matrix"))
}


#' @export
is_ug.default <- function(object){
    .check.is.matrix(object)
    isugMAT_(object)
}
#' @export
#' @rdname graph_is
is_ugMAT <- function(object){
    isugMAT_(object)
}

## ######################################

#' @export
#' @rdname graph_is
is_tug <- function(object){
  UseMethod("is_tug")
}

## #' @export
## is_tug.graphNEL <- function(object){
##     z <- as(object, "matrix")
##     if (!isugMAT_(z)) FALSE
##     else length(ripMAT(z)) > 0
## }

#' @export
is_tug.igraph <- function(object){
    z <- as(object, "matrix")
    if (!isugMAT_(z)) FALSE
    else length(ripMAT(z)) > 0
}

#' @export
is_tug.default <- function(object){
    .check.is.matrix(object)
    if (isugMAT_(object)) length(mcsMAT(object)) > 0
    else FALSE
}

#' @export
#' @rdname graph_is
is_tugMAT <- function(object){
    isugMAT_(object) && length(mcsMAT(object))>0
}

## ######################################

#' @export
#' @rdname graph_is
is_dg <- function(object){
    UseMethod("is_dg")
}

## #' @export
## is_dg.graphNEL <- function(object){
##     is_dgMAT(as(object, "matrix"))
## }

#' @export
is_dg.igraph <- function(object){
    is_dgMAT(as(object, "matrix"))
}



#' @export
is_dg.default <- function(object){
    .check.is.matrix(object)
    eps <- 1e-4
    if (isadjMAT_(object))
        max(abs(sum(object * t(object)))) <= eps 
    else FALSE
}

#' @export
#' @rdname graph_is
is_dgMAT <- function(object){
    if (!is.adjMAT(object)) stop("Matrix is not adjacency matrix...\n")
    eps <- 1e-4
    max(abs(sum(object * t(object)))) <= eps
}


#' @export
#' @rdname graph_is
is_adjMAT <- function(object){
    .check.is.matrix(object)
    isadjMAT_(object)
}

#' @rdname graph_is
#' @section Synonymous functions:
#'
#' The functions
#'
#' * `is.TUG`/`is.DAG`/`is.DG`/`is.UG`/`is.adjMAT`
#'
#' are synonymous with
#'
#' * `is_tug`/`is_dag`/`is_dg`/`is_ug`/`is_adjMAT`.
#'
#' The `is.X` group of functions will be deprecated.
#'
#' @aliases is.adjMAT is.TUG is.DAG is.DG is.UG

#' @export
is.adjMAT <- is_adjMAT

#' @export
is.TUG <- is_tug

#' @export
is.DAG <- is_dag

#' @export
is.DG  <- is_dg

#' @export
is.UG  <- is_ug



