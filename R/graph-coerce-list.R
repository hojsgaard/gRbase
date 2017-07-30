## --------------------------------------------------------------
##
## Coercions of graphs involving lists (adjacency lists etc...)
##
## --------------------------------------------------------------

## ----------------------------------
## Interface
## ----------------------------------
## adjList2matrix     <- adjList2matrix_
## adjList2dgCMatrix  <- adjList2dgCMatrix_
## ugList2matrix      <- ugList2matrix_
## ugList2dgCMatrix   <- ugList2dgCMatrix_
## dagList2matrix     <- dagList2matrix_
## dagList2dgCMatrix  <- dagList2dgCMatrix_
adjList2ftM        <- adjList2ftM__
adjList2tfM        <- adjList2tfM__

#' @title Coercion of graphs represented as lists
#'
#' @description FIXME
#'
#' @title graph-coerce-list
#'
#' ## @aliases adl2dm adl2gn adl2ig ugl2dm ugl2gn ugl2ig ugl2sm adl2sm
#' ##    dgl2dm dgl2gn dgl2ig dgl2sm 
#'
#' 
#' @param amat Adjacency matrix (dense or sparse dgCMatrix).
#' @param glist A list of generators where a generator is a character
#'     vector. If interpreted as generators of an undirected graph, a
#'     generator is a complete set of vertices in the graph. If
#'     interpreted as generators of a dag, a generator (v1,...,vn)
#'     means that there will be arrows from v2,...,vn to v1.
#' @param vn The names of the vertices in the graphs. These will be
#'     the row and column names of the matrix
#' @param result FIXME
#' 
#' 
## FIXME M2adjList fails for sparse matrix
#' @rdname graph-coerce-list
#' @examples
#' 
#' ## Sparse and dense adjacency matrices converted to adjacency list 
#' g1 <- ug(~a:b + b:c + c:d, result="matrix")
#' g2 <- ug(~a:b + b:c + c:d, result="dgCMatrix")
#' M2adjList( g1 )
#' ## M2adjList( g2 ) ## FIXME FAILS for sparse matrix
#' 
#' ## Sparse and dense adjacency matrices converted to cliques
#' M2ugList( g1 )
#' ## M2ugList( g2 ) ## FIXME Is there an issue here??
#' ## Sparse and dense adjacency matrices converted to cliques
#' M2dagList( g1 )
#' ## M2dagList( g2 ) ## Fails for sparse matrix
#' 

#' @rdname graph-coerce-list
#' @param zz An object representing a graph.
ugl2gn_ <- function(glist, vn=NULL){
    if (is.null(vn)) vn <- unique.default(unlist(glist))
    zzz <- lapply(glist, function(xx) names2pairs(xx, sort=TRUE, result="matrix"))
    ftM <- do.call(rbind, zzz)
    if ( nrow(ftM) > 0 ){
        tofrom <- unique(rowmat2list__(ftM))
        fff <- do.call(rbind, tofrom)
        graph::ftM2graphNEL(fff, V=as.character(vn), edgemode="undirected")
    } else {
        new("graphNEL", nodes=as.character(vn), edgemode="undirected")
    }
}

#' @rdname graph-coerce-list
ugl2M_ <- function(glist, vn=NULL, result="matrix"){
    result <- match.arg(result, c("matrix", "dgCMatrix"))
    if (is.null(vn)) vn <- unique.default(unlist(glist))
    switch(result,
           "dgCMatrix" = {ugl2sm_( glist, vn )},
           "matrix"    = {ugl2dm_( glist, vn )}  )
}

#' @rdname graph-coerce-list
ugl2dm_ <- function(zz, vn=NULL) ugList2matrix__(zz, vn)

#' @rdname graph-coerce-list
ugl2sm_ <- function(zz, vn=NULL) ugList2dgCMatrix__(zz, vn)

#' @rdname graph-coerce-list
ugl2ig_ <- function(zz, vn=NULL) {
    gg <- igraph::igraph.from.graphNEL(ugl2gn_(zz, vn))
    igraph::V(gg)$label <- igraph::V(gg)$name
    gg
}

#' @rdname graph-coerce-list
dgl2gn_ <- function(glist, vn=NULL){
    if (is.null(vn)) vn <- unique.default(unlist(glist))
    zzz <- lapply(glist, function(xx) names2pairs(xx[1],xx[-1],
                                                  sort=FALSE, result="matrix"))
    ftM <- do.call(rbind, zzz)
    if (nrow(ftM) > 0){
        tfL <- unique(rowmat2list__(ftM))
        ftM <- do.call(rbind, tfL)[,2:1,drop=FALSE]
        graph::ftM2graphNEL(ftM, V=as.character(vn), edgemode="directed")
    } else {
        new("graphNEL", nodes=as.character(vn), edgemode="directed")
    }
}

#' @rdname graph-coerce-list
dgl2dm_ <- function(zz, vn=NULL) dagList2matrix__(zz, vn)

#' @rdname graph-coerce-list
dgl2sm_ <- function(zz, vn=NULL) dagList2dgCMatrix__(zz, vn)

#' @rdname graph-coerce-list
dgl2ig_ <- function(zz, vn=NULL){    
    gg <- igraph::igraph.from.graphNEL(dgl2gn_(zz, vn))
    igraph::V(gg)$label <- igraph::V(gg)$name
    gg
}

#' @rdname graph-coerce-list
dgl2M_ <- function(glist, vn=NULL, result="matrix"){
    result <- match.arg(result, c("matrix", "dgCMatrix"))
    if (is.null(vn)) vn <- unique.default(unlist(glist))
    switch(result,
           "dgCMatrix" = {dgl2sm_( glist, vn )},
           "matrix"    = {dgl2dm_( glist, vn )}  )
}

#' @rdname graph-coerce-list
adl2gn_ <- function(zz) stop("Function not implemented - hmmmmmmmmmm") ## FIXME
#' @rdname graph-coerce-list
adl2dm_ <- function(zz) adjList2matrix__(zz)
#' @rdname graph-coerce-list
adl2sm_ <- function(zz) adjList2dgCMatrix__(zz)
#' @rdname graph-coerce-list
adl2ig_ <- function(zz) stop("Function not implemented") ## FIXME

#' @rdname graph-coerce-list
#' @param alist An adjacency list. 
adl2M_ <- function(alist, result="matrix"){
    result <- match.arg(result, c("matrix", "dgCMatrix"))
    switch(result,
           "matrix"   = {adl2dm_( alist )},
           "dgCMatrix"= {adl2sm_( alist )})
}

#' @rdname graph-coerce-list
M2adl_ <- function( amat ){
    .check.is.matrix( amat )
    if (!isadjMAT_( amat ))  stop("' amat ' not an adjacency matrix\n")
    vn <- colnames( amat )
    r  <- rowmat2list__( amat )
    i  <- lapply(r, function(z) which(z!=0))
    out <- lapply(i, function(j) vn[j])
    names(out) <- vn
    out
}

#' @rdname graph-coerce-list
M2ugl_ <- function( amat ){
    ## FIXME: M2ugList: Need a check for undirectedness
    .check.is.matrix( amat )
    maxCliqueMAT( amat )[[1]]
}

#' @rdname graph-coerce-list
M2dgl_ <- function( amat ){
    .check.is.matrix( amat )
    vn <- colnames( amat )
    c  <- colmat2list( amat )
    i  <- lapply(c, function(z) which(z != 0))
    i  <- lapply(1:length(vn), function(j) c(j, i[[j]]))
    out <- lapply(i, function(j) vn[j])
    out
}



## #################################################################
##
## Various lists 2 something
##
## adjList : named list as returned by graph::edges( ) : For each
## component v in the list, a vector (nb1, ..., nbn) or (ch1, ..., chn)
## 
## glist: A list of vectors of the form (v, pa1, pa2, ... pan)
##
## #################################################################

#' @rdname graph-coerce-list
M2adjList <- M2adl_

#' @rdname graph-coerce-list
M2ugList <- M2ugl_

#' @rdname graph-coerce-list
M2dagList <- M2dgl_

#' @rdname graph-coerce-list
ugList2graphNEL <- ugl2gn_

#' @rdname graph-coerce-list
ugList2M <- ugl2M_

#' @rdname graph-coerce-list
dagList2graphNEL <- dgl2gn_

#' @rdname graph-coerce-list
dagList2M <- dgl2M_

#' @rdname graph-coerce-list
adjList2M <- adl2M_


