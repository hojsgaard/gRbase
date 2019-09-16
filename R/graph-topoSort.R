# In graph theory, a topological sort or topological ordering of a
# directed acyclic graph (DAG) is a linear ordering of its nodes in
# which each node comes before all nodes to which it has outbound edges.
# Every DAG has one or more topological sorts. If such ordering can
# not be found then the graph has cycles
#
# Input:  list of vectors of the form (v,pa(v))
# Output: vector with ordering
#
# should perhaps be called dagTopoSort

#' @title Topological sort of vertices in directed acyclic graph
#' 
#' @description A topological ordering of a directed graph is a linear
#'     ordering of its vertices such that, for every edge (u->v), u
#'     comes before v in the ordering.  A topological ordering is
#'     possible if and only if the graph has no directed cycles, that
#'     is, if it is a directed acyclic graph (DAG). Any DAG has at
#'     least one topological ordering. Can hence be used for checking
#'     if a graph is a DAG.
#' 
#' @name graph-toposort
#' 
#' @aliases topo_sort topo_sort.default topo_sortMAT topo_sort_vparList
#' @param object An graph represented either as a \code{graphNEL}
#'     object, an \code{igraph}, a (dense) \code{matrix}, a (sparse)
#'     \code{dgCMatrix}.
#' @param index If FALSE, an ordering is returned if it exists and
#'     \code{character(0)} otherwise. If TRUE, the index of the
#'     variables in an adjacency matrix is returned and \code{-1}
#'     otherwise.
#' @return If FALSE, an ordering is returned if it exists and
#'     \code{character(0)} otherwise. If TRUE, the index of the
#'     variables in an adjacency matrix is returned and \code{-1}
#'     otherwise.
#' @section Note: The workhorse is the \code{topo_sortMAT} function
#'     which takes an adjacency matrix as input
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @seealso \code{\link{dag}}, \code{\link{ug}}
#' @keywords utilities
#' @examples
#' 
#' dagMAT  <- dag(~a:b:c+c:d:e, result="matrix")
#' dagMATS <- as(dagMAT, "dgCMatrix")
#' dagNEL  <- as(dagMAT, "graphNEL")
#' 
#' topo_sort(dagMAT)
#' topo_sort(dagMATS)
#' topo_sort(dagNEL)
#' 
#' @export topo_sort
topo_sort <- function(object, index=FALSE){
  UseMethod("topo_sort")
}

#' @rdname graph-toposort
topo_sort.default <- function(object, index=FALSE){
    ## cls <- match.arg(class( object ),
    ##                  c("graphNEL", "igraph", "matrix", "dgCMatrix"))
    topo_sortMAT(as_(object, "dgCMatrix"), index=index)
}


#' @rdname graph-toposort
#' @param amat Adjacency matrix. 
topo_sortMAT <- function(amat, index=FALSE){
    ans <- topo_sortMAT_( amat )
    if (index){
        if (ans[1] != -1) ans
        else -1L
    } else {
        if (ans[1] != -1) colnames(amat)[ans]
        else character(0)
    }
}

## FIXME topo_sort_vparList Delete?
topo_sort_vparList<- function(glist){
    ##topo_sort(vpaList2adjMAT(vpaL, result="Matrix"))
    ##topo_sort(dagList2M(glist, result="dgCMatrix"))
    topo_sortMAT(dgl2sm_(glist))
}




