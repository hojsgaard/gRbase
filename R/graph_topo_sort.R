############################################################################
#'
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
############################################################################
#'
#' @param object An graph represented either as a \code{graphNEL}
#'     object, an \code{igraph}, a (dense) \code{matrix}, a (sparse)
#'     \code{dgCMatrix}.
#' @param amat Adjacency matrix. 
#' @param index If FALSE, an ordering is returned if it exists and
#'     \code{character(0)} otherwise. If TRUE, the index of the
#'     variables in an adjacency matrix is returned and \code{-1}
#'     otherwise.
#' @return If FALSE, an ordering is returned if it exists and
#'     \code{character(0)} otherwise. If TRUE, the index of the
#'     variables in an adjacency matrix is returned and \code{-1}
#'     otherwise.
#'
#' @note The workhorse is the \code{topo_sortMAT} function which takes
#'     an adjacency matrix as input.
#'
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @seealso \code{\link{dag}}, \code{\link{ug}}
#' @keywords utilities
#' 
#' @examples
#' dagMAT  <- dag(~a:b:c + c:d:e, result="matrix")
#' dagMATS <- as(dagMAT, "dgCMatrix")
#' dagNEL  <- as(dagMAT, "graphNEL")
#' 
#' topo_sort(dagMAT)
#' topo_sort(dagMATS)
#' topo_sort(dagNEL)


#' @export topo_sort
topo_sort <- function(object, index=FALSE){
  UseMethod("topo_sort")
}

#' @export
topo_sort.default <- function(object, index=FALSE){
    topo_sortMAT(as(object, "dgCMatrix"), index=index)
}

#' @export
#' @rdname graph-toposort
topo_sortMAT <- function(amat, index=FALSE){
    ans <- topo_sortMAT_(amat)
    if (index){
        if (ans[1] != -1) ans
        else -1L
    } else {
        if (ans[1] != -1) colnames(amat)[ans]
        else character(0)
    }
}



#' @section Synonymous functions:
#'
#' The functions `topo_sort` / `topoSort` are synonymous with `topo_sortMAT` /
#' `topoSortMAT`. One of the groups may be deprecated in the future.


#' @export
#' @rdname graph-toposort
topoSort <- topo_sort

#' @export
#' @rdname graph-toposort
topoSortMAT <- topo_sortMAT



## ## FIXME topo_sort_vparList Delete?
## #' @export
## topo_sort_vparList<- function(glist){
##     topo_sortMAT(g_dagl2sm_(glist))
## }



