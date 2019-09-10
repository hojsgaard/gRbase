## ##############################################################
##
## Lower level graph, matrix, glist coercions
##
## ##############################################################

## ----------------------------------
## Interface
## ----------------------------------
matrix2dgCMatrix <- matrix2dgCMatrix__
dgCMatrix2matrix <- dgCMatrix2matrix__

.check.is.matrix <- function(x){
    if (!inherits(x, c("matrix", "dgCMatrix")))
        stop("Input must be a matrix or a dgCMatrix\n")
}

.check.is.graphNEL <- function(x){
    if (!inherits(x, "graphNEL"))
        stop("'gn' not a graphNEL object...")    
}

#' @title Graph, matrix and generating class coercions
#'
#' @description Graph and matrix coercions where speed is an issue.
#'
#' @name graph-coerce-misc
#'
#' @aliases  graphNEL2adjMAT
#' 
#' @param glist A list of generators where a generator is a character
#'     vector. If interpreted as generators of an undirected graph, a
#'     generator is a complete set of vertices in the graph. If
#'     interpreted as generators of a dag, a generator (v1,...,vn)
#'     means that there will be arrows from v2,...,vn to v1.
#' @param vn The names of the vertices in the graphs. These will be
#'     the row and column names of the matrix
#' @param result Either \code{"matrix"} or \code{"dgCMatrix"} (for a
#'     sparse matrix representation)
#' @return An adjacency matrix (or \code{NULL} if \code{glist} has
#'     length 0)
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @seealso \code{\link{ug}}, \code{\link{dag}}
#' @keywords utilities
#' @examples
#' 
#' glist <- list(1:3, 2:4, 4:5)
#' am1 <- ugList2M( glist )
#' am2 <- dagList2M( glist )
#' if (interactive() && require(Rgraphviz)){
#'   plot(as(am1, "graphNEL"))
#'   plot(as(am2, "graphNEL"))
#' }
#' 

## ################################################
## Convert from igraph
## ################################################

#' @rdname graph-coerce-misc
#' @param  ig igraph-object
#' @details The igraph2something functions only serve to provide a
#'     uniform coercion interface; the functions are wrappers to
#'     \pkg{igraph} functions.
igraph2graphNEL   <- function(ig) ig2gn_(ig)

#' @rdname graph-coerce-misc
igraph2matrix     <- function(ig) ig2dm_(ig)

#' @rdname graph-coerce-misc
igraph2dgCMatrix  <- function(ig) ig2sm_(ig)


## ###############################################
## Convert between matrix and dgCMatrix
## ###############################################

#' @rdname graph-coerce-misc
#' @param mat Either a dense matrix or a sparse dgCMatrix.
#'
#' @examples
#' ## FIXME : This is a mess
#' m <- matrix(1:25, nrow=5)
#' M <- M2dgCMatrix(m)
#' m2 <- M2matrix(M)
#' M2 <- M2dgCMatrix(m2)
#'
## #' if (require(microbenchmark)){
## #'   microbenchmark(as(M, "matrix"), M2matrix(M), M2matrix__(M),
## #'                  as(m, "dgCMatrix"), M2dgCMatrix(m), M2dgCMatrix__(m))
## #' }

M2matrix <- function( mat ){
    .check.is.matrix( mat )
    M2matrix__(mat)
}

#' @rdname graph-coerce-misc
M2dgCMatrix <- function( mat ){
    .check.is.matrix( mat )
    M2dgCMatrix__(mat)
}

## #################################################################
## Convert from graphNEL
## #################################################################

#' @rdname graph-coerce-misc
#' @param gn A graphNEL object
#'
#' @examples
#' g <- ug(~ a:b + b:c)
#' graphNEL2M(g)
#' graphNEL2M(g, "dgCMatrix")
#' 
#' @rdname graph-coerce-misc
graphNEL2M <- function(gn, result="matrix"){
    .check.is.graphNEL(gn)
    switch(result,
           "matrix"={gn2dm_(gn)},
           "dgCMatrix"={gn2sm_(gn)}
           )
}

## Think as.adjMAT is used in the book
#' @rdname graph-coerce-misc
as.adjMAT       <- graphNEL2M

## FIXME graphNEL2dgCMatrix to be replaced with graphNEL2M( , "matrix") Used a lot; on the the other hand...
#' @rdname graph-coerce-misc
graphNEL2dgCMatrix <- function(gn){
    .check.is.graphNEL(gn)
    gn2sm_(gn)
}

## FIXME: graphNEL2matrix Delete this - Never used
#' @rdname graph-coerce-misc 
graphNEL2matrix    <- function(gn){
    .check.is.graphNEL(gn)
    gn2dm_(gn)
}

#' @rdname graph-coerce-misc
graphNEL2igraph <- function( gn ){
    .check.is.graphNEL(gn)        
    gn2ig_(gn)
}
    
#' @rdname graph-coerce-misc
graphNEL2ftM <- function(gn){
    .check.is.graphNEL(gn)    
    adjList2ftM(graph::edges(gn))
}

#' @rdname graph-coerce-misc
graphNEL2tfM <- function(gn){
    .check.is.graphNEL(gn)        
    adjList2tfM(graph::edges(gn))
}




## #################################################################
## matrix/dgCMatrix 2 something
## #################################################################

#' @rdname graph-coerce-misc
#' @param amat Adjacency matrix
#' @examples
#' 
#' ## Sparse and dense adjacency matrices converted to igraph
#' g1 <- ug(~a:b + b:c + c:d, result="matrix")
#' g2 <- ug(~a:b + b:c + c:d, result="dgCMatrix")
#' plot( M2igraph( g1 ) )
#' plot( M2igraph( g2 ) )
#' 
M2igraph <- function( amat ){
    if (isSymmetric( amat )){
        gg <- igraph::graph.adjacency( amat , mode="undirected")
    } else {
        gg <- igraph::graph.adjacency( amat , mode="directed")
    }
    igraph::V(gg)$label <- igraph::V(gg)$name <- colnames( amat )
    gg
}

#' @rdname graph-coerce-misc
#' @examples
#' 
#' ## Sparse and dense adjacency matrices converted to graphNEL
#' g1 <- ug(~a:b + b:c + c:d, result="matrix")
#' g2 <- ug(~a:b + b:c + c:d, result="dgCMatrix")
#' M2graphNEL( g1 )
#' M2graphNEL( g2 )
M2graphNEL <- function( amat ){
    .check.is.matrix( amat )
    as( amat , "graphNEL")
}






## -----------------------------------------
## as.functions
## -----------------------------------------


## FIXME should have as.graphNEL method as well

## ------------------------------------
## FIXME: Stuff that can be deleted
## ------------------------------------


## FIXME graphNEL2adjMAT used by HydeNet package; I do not use it.
## FIXME graphNEL2adjMAT tell author to use graphNEL2matrix or graphNEL2M(, "matrix")
graphNEL2adjMAT <- graphNEL2M

## Represent list of sets in a matrix...
#' @rdname graph-coerce-misc
#'
#' @examples
#'
#' g <- list(c(1,2,3), c(2,3,4), c(4,5))
#' glist2setMAT(g)
glist2setMAT <- function(glist, vn=unique(unlist(glist))){
    amat <- matrix(0, nrow=length(glist), ncol = length(vn))
    colnames(amat) <- vn
    for (i in 1:length(glist)){
        amat[i, glist[[i]]] <- 1
    }
    amat
}






## vpaL2tfM: (v,pa(v))-list 2 to-from-matrix
## FIXME vpaL2tfM: rename to vpaList2ftM; used in topoSort

## #' @rdname graph-coerce-misc
## dagList2tfM <- function(glist){
##  eMat  <- lapply(glist, function(xx) names2pairs(xx[1], xx[-1],
##                                                  sort = FALSE, result = "matrix"))
##  do.call(rbind, eMat)
## }
