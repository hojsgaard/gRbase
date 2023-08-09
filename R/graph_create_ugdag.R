############################################################################
#'
#' @title Create undirected and directed graphs 
#' @description These functions are wrappers for creation of graphs as
#'     implemented by graphNEL objects in the \code{graph} package.
#' @name graph-create
#' 
############################################################################ 
#'
#' @param \dots A generating class for a graph, see examples below
#' @param forceCheck Logical determining if it should be checked if
#'     the graph is acyclical. Yes, one can specify graphs with cycles
#'     using the \code{dag()} function.
#' @param x A list or individual components from which a graph can be
#'     created.
#' @param result The format of the graph. The possible choices are
#'     "graphNEL" (for a `graphNEL` object), "igraph" (for an `igraph`
#'     object), "matrix" (for an adjacency matrix), "dgCMatrix" (for a
#'     sparse matrix).
#' @return Functions \code{ug()}, and \code{dag()} can return a
#'     \code{graphNEL} object, an `igraph` object, a sparse or a dense
#'     adjacency matrix.
#'     
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @keywords utilities
#' @examples
#' 
#' ## The following specifications of undirected graphs are equivalent:
#' uG1 <- ug(~ a:b:c + c:d)
#' uG2 <- ug(c("a", "b", "c"), c("c", "d"))
#' uG3 <- ug(c("a", "b"), c("a", "c"), c("b", "c"), c("c", "d"))
#' 
#' ## The following specifications of directed acyclig graphs are equivalent:
#' daG1 <- dag(~ a:b:c + b:c + c:d)
#' daG2 <- dag(c("a", "b", "c"), c("b", "c"), c("c", "d"))
#' 
#' ## dag() allows to specify directed graphs with cycles:
#' daG4 <- dag(~ a:b + b:c + c:a) # A directed graph but with cycles
#' 
#' ## A check for acyclicity can be done with
#' ## daG5 <- dag(~ a:b + b:c + c:a, forceCheck=TRUE) 
#' 
#' ## A check for acyclicity is provided by topoSort
#' topo_sort( daG2 )
#' topo_sort( daG4 )
#' 
#' ## Different representations
## ' uG6 <- ug(~a:b:c + c:d, result="graphNEL")  # default
#' uG7 <- ug(~a:b:c + c:d, result="igraph")    # igraph
#' uG8 <- ug(~a:b:c + c:d, result="matrix")    # dense matrix
#' uG9 <- ug(~a:b:c + c:d, result="dgCMatrix") # sparse matrix

#' @export
#' @rdname graph-create
ug <- function(..., result="igraph") {  ## FIXME was graphNEL
  ugList(list(...), result=result)
}

#' @export
#' @rdname graph-create
ugi <- function(...) {
  ugList(list(...), result="igraph")
}

.spam.result <- function(result) {
    ##if (identical(result, "Matrix")) stop('"Matrix" is deprecated; use "dgCMatrix" instead\n')
    if (identical(result, "NEL")) stop('"NEL" is deprecated; use "graphNEL" instead\n')
}

#' @export
#' @rdname graph-create
ugList <- function(x, result="igraph") {  ## FIXME was graphNEL
    result <- match.arg(result, c("graphNEL", "matrix", "dgCMatrix", "igraph", "Matrix", "NEL"))
    .spam.result(result)
    
    x   <- unlist(lapply(x, function(g) rhsf2list(g)), recursive=FALSE)
    vn  <- unique.default(unlist(x))

    switch(result,
           ## "graphNEL" ={g_ugl2gn_(x, vn)},
           "igraph"   ={g_ugl2ig_(x, vn)},
           "matrix"   ={g_ugl2dm_(x, vn)},
           "Matrix"   =,
           "dgCMatrix"={g_ugl2sm_(x, vn)})
}

    
###########################
## Directed acyclic graphs
###########################

#' @export
#' @rdname graph-create
dag <- function(..., result="igraph", forceCheck=FALSE){  ## FIXME was graphNEL
  dagList(list(...), result=result, forceCheck=forceCheck)
}

#' @export
#' @rdname graph-create
dagi <- function(..., forceCheck=FALSE) {
  dagList(list(...), result="igraph", forceCheck=forceCheck)
}

#' @export
#' @rdname graph-create
dagList <- function(x, result="igraph", forceCheck=FALSE) {  ## FIXME was graphNEL
    result <- match.arg(result, c("graphNEL", "matrix", "dgCMatrix", "igraph", "Matrix", "NEL"))
    .spam.result(result)
    
    x   <- unlist(lapply(x, function(g) rhsf2list(g)), recursive=FALSE)
    vn  <- unique.default(unlist(x))

    out <- switch(result,
                  ## "graphNEL"  = {g_dagl2gn_(x, vn)},
                  "igraph"    = {g_dagl2ig_(x, vn)},
                  "matrix"    = {g_dagl2dm_(x, vn)},
                  "Matrix"    = ,
                  "dgCMatrix" = {g_dagl2sm_(x, vn)})

    if (forceCheck) {
        if( length(topo_sort(out)) == 0) {
            stop("In dag/dagList: Graph is not a DAG", call.=FALSE)
        }
    }
    out
}



