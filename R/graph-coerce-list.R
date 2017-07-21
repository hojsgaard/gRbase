## --------------------------------------------------------------
##
## Coercions of graphs involving lists (adjacency lists etc...)
##
## --------------------------------------------------------------

## ----------------------------------
## Interface
## ----------------------------------
adjList2matrix     <- adjList2matrix_
adjList2dgCMatrix  <- adjList2dgCMatrix_
ugList2matrix      <- ugList2matrix_
ugList2dgCMatrix   <- ugList2dgCMatrix_
dagList2matrix     <- dagList2matrix_
dagList2dgCMatrix  <- dagList2dgCMatrix_
adjList2ftM        <- adjList2ftM_
adjList2tfM        <- adjList2tfM_

#' @title Coercion of graphs represented as lists
#'
#' @description FIXME
#'
#' @title graph-coerce-list
#'
#' @aliases adjList2dgCMatrix_ adjList2matrix_ adjList2ftList_
#'     adjList2ftM_ adjList2tfList_ adjList2tfM_ adl2dm adl2gn adl2ig
#'     adl2sm dagList2dgCMatrix_ dagList2matrix_ dgCMatrix2matrix_
#'     dgl2dm dgl2gn dgl2ig dgl2sm matrix2dgCMatrix_ ugList2dgCMatrix_
#'     ugList2matrix_ ugl2dm ugl2gn ugl2ig ugl2sm
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
M2adjList <- function( amat ){
    .check.is.matrix( amat )
    if (!isadjMAT_( amat ))  stop("' amat ' not an adjacency matrix\n")
    vn <- colnames( amat )
    r  <- rowmat2list( amat )
    i  <- lapply(r, function(z) which(z!=0))
    out <- lapply(i, function(j) vn[j])
    names(out) <- vn
    out
}

#' @rdname graph-coerce-list
M2ugList <- function( amat ){
    ## FIXME: M2ugList: Need a check for undirectedness
    .check.is.matrix( amat )
    maxCliqueMAT( amat )[[1]]
}

#' @rdname graph-coerce-list
M2dagList <- function( amat ){
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
## 
## glist: A list of vectors of the form (v, pa1, pa2, ... pan)
##
## #################################################################

## vpa.list  (vlist / vpalist) - only for dag
## gen.list  (glist / genlist) - only for ug
## adj.list  (alist / adjlist) - for ug and dag

## ugList2graphNEL : same as ugList( , result="graphNEL")

#' @rdname graph-coerce-list
ugList2graphNEL<- function(glist, vn=NULL){
    if (is.null(vn)) vn <- unique.default(unlist(glist))
    zzz <- lapply(glist, function(xx) names2pairs(xx, sort=TRUE, result="matrix"))
    ftM <- do.call(rbind, zzz)
    if ( nrow(ftM) > 0 ){
        tofrom <- unique(rowmat2list(ftM))
        fff <- do.call(rbind, tofrom)
        graph::ftM2graphNEL(fff, V=as.character(vn), edgemode="undirected")
    } else {
        new("graphNEL", nodes=as.character(vn), edgemode="undirected")
    }
}

#' @rdname graph-coerce-list
ugList2M <- function(glist, vn=NULL, result="matrix"){
    result <- match.arg(result, c("matrix","Matrix","dgCMatrix"))
    if (is.null(vn)) vn <- unique.default(unlist(glist))
    switch(result,
           "Matrix"=,
           "dgCMatrix" = {ugList2dgCMatrix( glist, vn )},
           "matrix"    = {ugList2matrix( glist, vn )}  )
}


## dagList2graphNEL : same as dagList( , result="graphNEL")
#' @rdname graph-coerce-list
dagList2graphNEL<- function(glist, vn=NULL){
    if (is.null(vn)) vn <- unique.default(unlist(glist))
    zzz <- lapply(glist, function(xx) names2pairs(xx[1],xx[-1],
                                                  sort=FALSE, result="matrix"))
    ftM <- do.call(rbind, zzz)
    if (nrow(ftM) > 0){
        tfL <- unique(rowmat2list(ftM))
        ftM <- do.call(rbind, tfL)[,2:1,drop=FALSE]
        graph::ftM2graphNEL(ftM, V=as.character(vn), edgemode="directed")
    } else {
        new("graphNEL", nodes=as.character(vn), edgemode="directed")
    }
}

#' @rdname graph-coerce-list
dagList2M <- function(glist, vn=NULL, result="matrix"){
    result <- match.arg(result, c("matrix", "Matrix", "dgCMatrix"))
    if (is.null(vn)) vn <- unique.default(unlist(glist))
    switch(result,
           "Matrix"=,
           "dgCMatrix" = {dagList2dgCMatrix( glist, vn )},
           "matrix"    = {dagList2matrix( glist, vn )}  )
}


#' @rdname graph-coerce-list
#' @param alist An adjacency list. 
adjList2M <- function( alist, result="matrix"){
    result <- match.arg(result, c("matrix", "Matrix", "dgCMatrix"))
    switch(result,
           "matrix"   = {adjList2matrix( alist )},
           "Matrix"   = ,
           "dgCMatrix"= {adjList2dgCMatrix( alist )})
}



#' @rdname graph-coerce-list
#' @param zz An object representing a graph.
ugl2gn_ <- function(zz) ugList2graphNEL(zz)
#' @rdname graph-coerce-list
ugl2dm_ <- function(zz) ugList2matrix_(zz, NULL)
#' @rdname graph-coerce-list
ugl2sm_ <- function(zz) ugList2dgCMatrix_(zz, NULL)
#' @rdname graph-coerce-list
ugl2ig_ <- function(zz) stop("Function not implemented") ## FIXME

#' @rdname graph-coerce-list
dgl2gn_ <- function(zz) dagList2graphNEL(zz, NULL)
#' @rdname graph-coerce-list
dgl2dm_ <- function(zz) dagList2matrix(zz, NULL)
#' @rdname graph-coerce-list
dgl2sm_ <- function(zz) dagList2dgCMatrix(zz, NULL)
#' @rdname graph-coerce-list
dgl2ig_ <- function(zz) stop("Function not implemented") ## FIXME

#' @rdname graph-coerce-list
adl2gn <- function(zz) stop("Function not implemented - hmmmmmmmmmm") ## FIXME
#' @rdname graph-coerce-list
adl2dm <- function(zz) adjList2matrix(zz)
#' @rdname graph-coerce-list
adl2sm <- function(zz) adjList2dgCMatrix(zz)
#' @rdname graph-coerce-list
adl2ig <- function(zz) stop("Function not implemented") ## FIXME


## FIXME adjList2adjMAT Delete
##adjList2adjMAT <- adjList2M

