## ##############################################################
##
## Lower level graph, matrix, glist coercions
##
## ##############################################################

## graphNEL 2 YYY
## --------------
## graphNEL2M <- function(object, result="matrix"){
## graphNEL2MAT <- function(object, limit=100){
## graphNEL2matrix    <- function(object){
## graphNEL2dgCMatrix <- function(object){
## graphNEL2ftM <- function(object){
## graphNEL2tfM <- function(object){
## graphNEL2igraph <- function(x){
##
## graphNEL2vpalist : 
## graphNEL2glist   : 
## graphNEL2alist   : 


## matrix/dgCMatrix to YYY
## -----------------------
## M2adjList <- function(x){
## M2ugList <- function(x){
## M2graphNEL <- function(x){
## M2dagList <- function(x){
## MAT2matrix <- function(x){
## MAT2dgCMatrix <- function(x){
## matrix2igraph <- function(x){
##
## M2igraph : ?

## vpa.list, gen.list, adj.list 2 YYY
## ----------------------------------
## ugList2graphNEL<- function(gset, vn=NULL){
## dagList2graphNEL<- function(gset, vn=NULL){
## vpaList2adjMAT <- function(glist, vn=unique(unlist(glist)), result="matrix"){
## glist2adjMAT <- function(glist, vn=unique(unlist(glist)), result="matrix"){
## adjList2adjMAT <- function(adjList, result="matrix"){
## adjList2M <- function( x, result="matrix"){
## vpaL2tfM <- function(vpaL){
## ugList2M <- function(x, result="matrix"){
## dagList2M <- function(x, result="matrix"){
##

## Special coercions

## ug2dag <- function(object){
## dag2ug ???

## glist2setMAT <- function(glist,vn=unique(unlist(glist))){


#' @title Graph, matrix and generating class coercions
#'
#' @description Graph and matrix coercions where speed is an issue.
#'
#' @name graph-xxx2yyy
#' 
#' @aliases glist2adjMAT vpaList2adjMAT vpaL2tfM graphNEL2adjMAT
#'     graphNEL2matrix graphNEL2dgCMatrix adjList2adjMAT
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
#' if (interactive()){
#'   plot(as(am1, "graphNEL"))
#'   plot(as(am2, "graphNEL"))
#' }
#' 



## #################################################################
##
## graphNEL 2 something
##
## #################################################################

#' @rdname graph-xxx2yyy
#' @param gn A graphNEL object
graphNEL2M <- function(gn, result="matrix"){
    if( class(gn) != "graphNEL" )
        stop("'gn' must be a graphNEL object...")
    adjList2M( graph::edges(gn), result=result )
}

#' @rdname graph-xxx2yyy
#' @param limit If number of nodes is larger than \code{limit}, the
#'     result will be a sparse dgCMatrix; otherwise a dense matrix.
graphNEL2MAT <- function(gn, limit=100){
    if( class(gn) != "graphNEL" )
        stop("'gn' must be a graphNEL object...")

    result <-
        if ( length( graph::nodes(gn) ) > limit )
            "dgCMatrix" else "matrix"

    adjList2M( graph::edges(gn), result=result )
}

## Never used
## #' @rdname graph-xxx2yyy
graphNEL2matrix    <- function(gn){
    graphNEL2M(gn, result="matrix")
}

## FIXME graphNEL2dgCMatrix to be replaced with graphNEL2M( , "matrix") Used a lot
## #' @rdname graph-xxx2yyy
graphNEL2dgCMatrix <- function(gn){
    graphNEL2M(gn, result="Matrix")
}

## FIXME graphNEL2adjMAT used by HydeNet package; I do not use it.
## FIXME graphNEL2adjMAT replace by graphNEL2M(, "matrix")
graphNEL2adjMAT <- graphNEL2M


#' @rdname graph-xxx2yyy
graphNEL2ftM <- function(gn){
    if( class(gn) != "graphNEL" )
        stop("'gn' must be a graphNEL object...")
    adjList2ftM(graph::edges(gn))
}

#' @rdname graph-xxx2yyy
graphNEL2tfM <- function(gn){
    if( class(gn) != "graphNEL" )
        stop("'gn' must be a graphNEL object...")
    adjList2tfM(graph::edges(gn))
}


#' @rdname graph-xxx2yyy
graphNEL2igraph <- function( gn ){
    gg <- igraph::igraph.from.graphNEL( gn )
    igraph::V(gg)$label <- igraph::V(gg)$name
    gg
}


## #################################################################
##
## matrix/dgCMatrix 2 YYY
##
## #################################################################

#' @rdname graph-xxx2yyy
#' @param amat Adjacency matrix
#' @examples
#' 
#' ## Sparse and dense adjacency matrices converted to igraph
#' g1 <- ug(~a:b + b:c + c:d, result="matrix")
#' g2 <- ug(~a:b + b:c + c:d, result="dgCMatrix")
#' plot( M2igraph( g1 ) )
#' plot( M2igraph( g2 ) )
M2igraph <- function( amat ){
    if (isSymmetric( amat )){
        gg <- igraph::graph.adjacency( amat , mode="undirected")
    } else {
        gg <- igraph::graph.adjacency( amat , mode="directed")
    }
    igraph::V(gg)$label <- igraph::V(gg)$name <- colnames( amat )
    gg
}

#' @rdname graph-xxx2yyy
#' @examples
#' 
#' ## Sparse and dense adjacency matrices converted to graphNEL
#' g1 <- ug(~a:b + b:c + c:d, result="matrix")
#' g2 <- ug(~a:b + b:c + c:d, result="dgCMatrix")
#' M2graphNEL( g1 )
#' M2graphNEL( g2 )
M2graphNEL <- function( amat ){
    .check.that.input.is.matrix( amat )
    as( amat , "graphNEL")
}


## FIXME M2adjList fails for sparse matrix
#' @rdname graph-xxx2yyy
#' @examples
#' 
#' ## Sparse and dense adjacency matrices converted to adjacency list 
#' g1 <- ug(~a:b + b:c + c:d, result="matrix")
#' g2 <- ug(~a:b + b:c + c:d, result="dgCMatrix")
#' M2adjList( g1 )
#' ## M2adjList( g2 ) FAILS for sparse matrix
M2adjList <- function( amat ){
    .check.that.input.is.matrix( amat )
    vn <- colnames( amat )
    if (!isadjMAT_( amat ))
        stop("' amat ' is not an adjacency matrix\n")
    r  <- rowmat2list( amat )
    i  <- lapply(r, function(z) which(z!=0))
    out <- lapply(i, function(j) vn[j])
    names(out) <- vn
    out
}

#' @rdname graph-xxx2yyy
#' @examples
#' 
#' ## Sparse and dense adjacency matrices converted to cliques
#' g1 <- ug(~a:b + b:c + c:d, result="matrix")
#' g2 <- ug(~a:b + b:c + c:d, result="dgCMatrix")
#' M2ugList( g1 )
#' M2ugList( g2 ) 
M2ugList <- function( amat ){
    ## FIXME: M2ugList: Need a check for undirectedness
    .check.that.input.is.matrix( amat )
    maxCliqueMAT( amat )[[1]]
}


#' @rdname graph-xxx2yyy
#' @examples
#' 
#' ## Sparse and dense adjacency matrices converted to cliques
#' g1 <- ug(~a:b + b:c + c:d, result="matrix")
#' g2 <- ug(~a:b + b:c + c:d, result="dgCMatrix")
#' M2dagList( g1 )
#' ## M2dagList( g2 ) ## Fails for sparse matrix
#' 
#' g1 <- dag(~a:b + b:c + c:d, result="matrix")
#' g2 <- dag(~a:b + b:c + c:d, result="dgCMatrix")
#' M2dagList( g1 )
#' ## M2dagList( g2 ) ## FIXME Fails for sparse matrix
#' 
M2dagList <- function( amat ){
    .check.that.input.is.matrix( amat )
    vn <- colnames( amat )
    c  <- colmat2list( amat )
    i  <- lapply(c, function(z) which(z!=0))
    i  <- lapply(1:length(vn), function(j) c(j, i[[j]]))
    out <- lapply(i, function(j) vn[j])
    ##names(out) <- vn
    out
}


## #################################################################
##
## Various lists 2 YYY
##
## adjList : named list as returned by graph::edges( )
##
## glist: A list of vectors of the form (v, pa1, pa2, ... pan)
##
## #################################################################

## vpa.list  (vlist / vpalist) - only for dag
## gen.list  (glist / genlist) - only for ug
## adj.list  (alist / adjlist) - for ug and dag

## adjlist2M
## genlist2M / gclist2M / gc2M
## vpalist2M / vpa2M

## adj_list2M
## gen_list2M / gclist2M / gc2M
## vpa_list2M / vpa2M


## ugList2graphNEL : same as ugList( , result="graphNEL")

#' @rdname graph-xxx2yyy
ugList2graphNEL<- function(glist, vn=NULL){
    if ( is.null(vn) )
        vn <- unique.default( unlist(glist, use.names=FALSE) )
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


#' @rdname graph-xxx2yyy
ugList2M <- function(glist, vn=NULL, result="matrix"){
    result <- match.arg(result, c("matrix","Matrix","dgCMatrix"))
    if ( is.null(vn) )
        vn <- unique.default(unlist(glist), use.names=FALSE)
    switch(result,
           "Matrix"=,
           "dgCMatrix" = {ugList2dgCMatrix( glist, vn )},
           "matrix"    = {ugList2matrix( glist, vn )}  )
}

## FIXME: glist2adjMAT Delete 
glist2adjMAT <- ugList2M


## dagList2graphNEL : same as dagList( , result="graphNEL")

#' @rdname graph-xxx2yyy
dagList2graphNEL<- function(glist, vn=NULL){
    if ( is.null(vn) )
        vn <- unique.default( unlist(glist, use.names=FALSE) )
    zzz <- lapply(glist, function(xx) names2pairs(xx[1],xx[-1],
                                                 sort=FALSE, result="matrix"))
    ftM <- do.call(rbind, zzz)
    if (nrow(ftM)>0){
        tfL <- unique(rowmat2list(ftM))
        ftM <- do.call(rbind,tfL)[,2:1,drop=FALSE]
        graph::ftM2graphNEL(ftM, V=as.character(vn),
                            edgemode="directed")
    } else {
        new("graphNEL", nodes=as.character(vn), edgemode="directed")
    }
}

#' @rdname graph-xxx2yyy
dagList2M <- function(glist, vn=NULL, result="matrix"){
    result <- match.arg(result, c("matrix", "Matrix", "dgCMatrix"))
    if ( is.null(vn) )
        vn <- unique.default(unlist(glist), use.names=FALSE)
    switch(result,
           "Matrix"=,
           "dgCMatrix" = {dagList2dgCMatrix( glist, vn )},
           "matrix"    = {dagList2matrix( glist, vn )}  )
}

## FIXME vpaList2adjMAT Delete
vpaList2adjMAT <- dagList2M

#' @rdname graph-xxx2yyy
#' @param alist An adjacency list. 
adjList2M <- function( alist, result="matrix"){
    result <- match.arg(result, c("matrix", "Matrix", "dgCMatrix"))
    switch(result,
           "matrix"   = {adjList2matrix( alist )},
           "Matrix"   = ,
           "dgCMatrix"= {adjList2dgCMatrix( alist )})
}

## FIXME adjList2adjMAT Delete
adjList2adjMAT <- adjList2M


## FIXME should have as.graphNEL method as well

## vpaL2tfM: (v,pa(v))-list 2 to-from-matrix
## FIXME vpaL2tfM: rename to vpaList2ftM; used in topoSort

#' @rdname graph-xxx2yyy
dagList2tfM <- function(glist){
 eMat  <- lapply(glist, function(xx) names2pairs(xx[1], xx[-1],
                                                 sort = FALSE, result = "matrix"))
 do.call(rbind, eMat)
}


.check.that.input.is.matrix <- function(x){
    if ( !(class(x)=="matrix" || class(x)=="dgCMatrix") )
        stop("Input must be a matrix or a dgCMatrix\n")
}





#' @rdname graph-xxx2yyy
as.adjMAT       <- graphNEL2M


#' @rdname graph-xxx2yyy
ug2dag <- function(gn){
    if (class(gn) != "graphNEL")
        stop("'gn' must be a graphNEL")
    if (graph::edgemode(gn) != "undirected")
        stop("Graph must have undirected edges")
    if (length( m <- mcs(gn) )==0)
        stop("Graph is not chordal")

    adjList  <- graph::adj(gn, m)
    vparList <- vector("list", length(m))
    names(vparList) <- m

    vparList[[1]] <- m[1]
    if (length(m) > 1){
        for (i in 2:length(m)){
            vparList[[ i ]] <- c(m[ i ],
                                intersectPrim(adjList[[ i ]], m[ 1:i ]))
        }
    }

    dg <- dagList(vparList)
    dg
}

## Represent list of sets in a matrix...
## FIXME: glist2setMAT: Used in gRain 1.2-3, but not in gRain 1.2-4
## FIXME: should be deleted for next release
glist2setMAT <- function(glist,vn=unique(unlist(glist))){
  amat <- matrix(0, nrow=length(glist), ncol = length(vn))
  colnames(amat) <- vn
  for (i in 1:length(glist)){
    amat[i, glist[[i]] ] <- 1
  }
  amat
}


##################################################
##
## Convert between matrix and dgCMatrix
##
##################################################

#' @rdname graph-xxx2yyy
#' @param mat Either a dense matrix or a sparse dgCMatrix.
MAT2matrix <- function( mat ){
    .check.that.input.is.matrix( mat )
    switch( class( mat ),
           "matrix"    ={ mat },
           "dgCMatrix" ={dgCMatrix2matrix( mat )})
}

#' @rdname graph-xxx2yyy
MAT2dgCMatrix <- function( mat ){
    .check.that.input.is.matrix( mat )
    switch( class( mat ),
           "matrix"    ={matrix2dgCMatrix( mat )},
           "dgCMatrix" ={ mat })
}


