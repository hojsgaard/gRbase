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
#' @aliases topoSort topoSort.default topoSortMAT topoSort_vparList
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
#' @section Note: The workhorse is the \code{topoSortMAT} function
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
#' topoSort(dagMAT)
#' topoSort(dagMATS)
#' topoSort(dagNEL)
#' 
#' @export topoSort
topoSort <- function(object, index=FALSE){
  UseMethod("topoSort")
}

#' @rdname graph-toposort
topoSort.default <- function(object, index=FALSE){
    cls <- match.arg(class( object ),
                     c("graphNEL","igraph","matrix","dgCMatrix"))
    switch(cls,
           "graphNEL" ={topoSortMAT(graphNEL2dgCMatrix(object), index=index) },
           "igraph"   ={topoSortMAT(igraph::get.adjacency(object), index=index) },
           "dgCMatrix"=,
           "matrix"   ={topoSortMAT(object, index=index)} )
}


#' @rdname graph-toposort
#' @param amat Adjacency matrix. 
topoSortMAT <- function(amat, index=FALSE){
    ans <- topoSortMAT_( amat )
    if (index){
        if (ans[1]!=-1){
            ans
        } else {
            -1L
        }
    } else {
        if (ans[1]!=-1){
            colnames(amat)[ans]
        } else {
            character(0)
        }
    }
}


## FIXME topoSort_vparList Delete?
topoSort_vparList<- function(glist){
    ##topoSort(vpaList2adjMAT(vpaL, result="Matrix"))
    topoSort(dagList2M(glist, result="Matrix"))
}



## topoSort.graphNEL<- function(object, index=FALSE){
##   topoSortMAT(as(object,"Matrix"), index=index)
## }



## topoSort.matrix <- topoSort.Matrix <- function(object, index=FALSE){
##   topoSortMAT(object, index=index)
## }

## topoSortMAT <- function(XX_, index=FALSE){
##   if (inherits(XX_, "Matrix")){
##     ans <- .Call("gRbase_topoSortMAT_sp", XX_ ,package="gRbase")
##   } else {
##     if (inherits(XX_, "matrix")){
##       ans <- .Call("gRbase_topoSortMAT_st", XX_ ,package="gRbase")
##     } else {
##       stop("'XX_' must be a matrix or a sparse matrix (a 'dgCMatrix')")
##     }
##   }
##   if (index){
##     if (ans[1]!=-1){
##       ans
##     } else {
##       -1L
##     }
##   } else {
##     if (ans[1]!=-1){
##       colnames(XX_)[ans]
##     } else {
##       character(0)
##     }
##   }
## }
