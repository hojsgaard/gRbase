##################################################################
####
#### Create undirected graphs or DAGs from graph specification
####
##################################################################

####################
## Undirected graphs
####################

#' @title Create undirected and directed graphs
#' 
#' @description These functions are wrappers for creation of graphs as
#'     implemented by graphNEL objects in the \code{graph} package.
#' 
#' @name graph-create
#' 
#' @aliases ug dag ugList dagList ugList2matrix ugList2dgCMatrix dagList2matrix
#'     dagList2dgCMatrix
#' @param \dots A generating class for a graph, see examples below
#' @param forceCheck Logical determining if it should be checked if the graph is
#'     acyclical. Yes, one can specify graphs with cycles using the \code{dag()}
#'     function.
#' @param result The format of the graph. The possible choices are "graphNEL"
#'     (for a graphNEL object), "matrix" (for an adjacency matrix), "dgCMatrix"
#'     (for a sparse matrix), "igraph" (for an igraph object).
#' @return Functions \code{ug()}, and \code{dag()} can return a \code{graphNEL}
#'     object, a sparse or dense adjacency matrix or an \code{igraph} object.
#'     
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @keywords utilities
#' @examples
#' 
#' ## The following specifications of undirected graphs are equivalent:
#' uG1 <- ug(~ a:b:c + c:d)
#' uG2 <- ug(c("a","b","c"), c("c","d"))
#' uG3 <- ug(c("a","b"), c("a","c"), c("b","c"), c("c","d"))
#' 
#' graph::edges(uG1)
#' graph::nodes(uG1)
#' 
#' ## The following specifications of directed acyclig graphs are equivalent:
#' daG1 <- dag(~ a:b:c + b:c + c:d)
#' daG2 <- dag(c("a","b","c"), c("b","c"), c("c","d"))
#' 
#' graph::edges(daG1)
#' graph::nodes(daG2)
#' 
#' ## dag() allows to specify directed graphs with cycles:
#' daG4 <- dag(~ a:b + b:c + c:a) # A directed graph but with cycles
#' ## A check for acyclicity can be done with
#' ## daG5 <- dag(~ a:b + b:c + c:a, forceCheck=TRUE) 
#' 
#' ## A check for acyclicity is provided by topoSort
#' topoSort( daG2 )
#' topoSort( daG4 )
#' 
#' ## Different representations
#' uG6 <- ug(~a:b:c + c:d, result="graphNEL")  # default
#' uG6
#' uG7 <- ug(~a:b:c + c:d, result="NEL")       # same
#' uG7
#' uG8 <- ug(~a:b:c + c:d, result="matrix")    # dense matrix
#' uG8
#' uG9 <- ug(~a:b:c + c:d, result="dgCMatrix") # sparse matrix
#' uG9
#' 
#' @export ug
ug <- function(..., result="graphNEL"){
  ugList(list(...), result=result)
}

ugList <- function(x, result="graphNEL"){
    result <- match.arg(result, c("matrix","Matrix","dgCMatrix","igraph","NEL","graphNEL"))
    x   <- unlist(lapply(x, function(g) rhsf2list(g)), recursive=FALSE)
    vn  <- unique.default(unlist(x))

    switch(result,
           "NEL"      =,
           "graphNEL" ={ugList2graphNEL(x, vn)},
           "Matrix"   =,
           "dgCMatrix"={ugList2dgCMatrix(x, vn)},
           "matrix"   ={ugList2matrix(x, vn)},
           "igraph"   ={
               gg <- igraph::igraph.from.graphNEL(ugList2graphNEL(x, vn))
               igraph::V(gg)$label <- igraph::V(gg)$name
               gg
           })
}


###########################
## Directed acyclic graphs
###########################

#' @rdname graph-create
dag <- function(..., result="graphNEL", forceCheck=FALSE){
  dagList(list(...), result=result, forceCheck=forceCheck)
}

dagList <- function(x, result="graphNEL", forceCheck=FALSE){
    result <- match.arg(result, c("matrix","Matrix","dgCMatrix","igraph","NEL","graphNEL"))
    x   <- unlist(lapply(x, function(g) rhsf2list(g)), recursive=FALSE)
    vn  <- unique(unlist(x))

    out <- switch(result,
                  "NEL"       =,
                  "graphNEL"  = {dagList2graphNEL(x, vn)},
                  "Matrix"    =,
                  "dgCMatrix" = {dagList2dgCMatrix(x, vn)},
                  "matrix"    = {dagList2matrix(x, vn)},
                  "igraph"    = {
                      gg <- igraph::igraph.from.graphNEL(dagList2graphNEL(x, vn))
                      igraph::V(gg)$label <- igraph::V(gg)$name
                      gg
                  })
    if (forceCheck){
        if( length( topoSort( out )) == 0){
            stop("In dag/dagList: Graph is not a DAG", call.=FALSE)
        }
    }
    out
}



