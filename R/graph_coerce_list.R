## #########################################################################
##
#' @title Coercion of graphs represented as lists
#' @description Coercion of graphs represented as lists to various
#'     graph formats.
#' @name graph_coerce_list
##
## #########################################################################
#'
#' @param amat Adjacency matrix (dense or sparse dgCMatrix).
#' @param glist A list of generators where a generator is a character
#'     vector. If interpreted as generators of an undirected graph, a
#'     generator is a complete set of vertices in the graph. If
#'     interpreted as generators of a dag, a generator (v1,...,vn)
#'     means that there will be arrows from v2,...,vn to v1.
#' @param zz An object representing a graph.
#' @param outtype What should a list be coerced to.
#' @param vn The names of the vertices in the graphs. These will be
#'     the row and column names of the matrix.
#' @param alist An adjacency list. 
#' @param result A graph object.
#' 
#' @examples
#' 
#' ## Sparse and dense adjacency matrices converted to adjacency list 
#' g1 <- ug(~a:b + b:c + c:d, result="matrix")
#' g2 <- ug(~a:b + b:c + c:d, result="dgCMatrix")
#' g_M2adl_( g1 )
#' 
#' ## Sparse and dense adjacency matrices converted to cliques
#' g_M2ugl_( g1 )
#'
#' ## Sparse and dense adjacency matrices converted to cliques
#' g_M2dagl_( g1 )
#' 
#' ## g_M2adl_( g2 ) ## FIXME FAILS for sparse matrix
#' ## g_M2ugl_( g2 ) ## FIXME Is there an issue here??
#' ## g_M2dagList( g2 ) ## Fails for sparse matrix
#' 

## ##########################################################
##
## ug_list2XX
##
## ##########################################################

#' @export
#' @rdname graph_coerce_list
g_ugl2ig_ <- function(zz, vn=NULL) {
    ## gg <- igraph::igraph.from.graphNEL(g_ugl2gn_(zz, vn))
    ## igraph::V(gg)$label <- igraph::V(gg)$name
    ## gg
    gg <- ug_list2igraph(zz)
    gg
}

#' @export
#' @rdname graph_coerce_list
g_ugl2dm_ <- function(zz, vn=NULL) {
    if (is.null(vn)) vn <- unique.default(unlist(zz))
    ugList2matrix__(zz, vn)
}

#' @export
#' @rdname graph_coerce_list
g_ugl2sm_ <- function(zz, vn=NULL){
    if (is.null(vn)) vn <- unique.default(unlist(zz))
    ugList2dgCMatrix__(zz, vn)
}

#' @export
#' @rdname graph_coerce_list
g_ugl2XX_ <- function(zz, outtype, vn=NULL) {
    switch(outtype,
           ## "graphNEL"   ={g_ugl2gn_(zz, vn)},
           "igraph"     ={g_ugl2ig_(zz, vn)},
           "matrix"     ={g_ugl2dm_(zz, vn)},
           "dgCMatrix"  =,
           "Matrix"     ={g_ugl2sm_(zz, vn)}
           )
}

## ##########################################################
##
## dag_list2XX
##
## ##########################################################

## #' @export
## #' @rdname graph_coerce_list
## g_dagl2gn_ <- function(glist, vn=NULL){
##     if (is.null(vn)) vn <- unique.default(unlist(glist))
##     zzz <- lapply(glist, function(xx) names2pairs(xx[1],xx[-1],
##                                                   sort=FALSE, result="matrix"))
##     ftM <- do.call(rbind, zzz)
##     if (nrow(ftM) > 0){
##         tfL <- unique(rowmat2list__(ftM))
##         ftM <- do.call(rbind, tfL)[,2:1,drop=FALSE]
##         graph::ftM2graphNEL(ftM, V=as.character(vn), edgemode="directed")
##     } else {
##         new("graphNEL", nodes=as.character(vn), edgemode="directed")
##     }
## }

#' @export
#' @rdname graph_coerce_list
g_dagl2ig_ <- function(zz, vn=NULL){    
    ## gg <- igraph::igraph.from.graphNEL(g_dagl2gn_(zz, vn))
    ## igraph::V(gg)$label <- igraph::V(gg)$name
    ## gg
    gg <- dag_list2igraph(zz)
    gg
    
}

#' @export
#' @rdname graph_coerce_list
g_dagl2dm_ <- function(zz, vn=NULL){
    if (is.null(vn)) vn <- unique.default(unlist(zz))
    dagList2matrix__(zz, vn)
}

#' @export
#' @rdname graph_coerce_list
g_dagl2sm_ <- function(zz, vn=NULL) {
    if (is.null(vn)) vn <- unique.default(unlist(zz))    
    dagList2dgCMatrix__(zz, vn)
}

#' @export
#' @rdname graph_coerce_list
g_dagl2XX_ <- function(zz, outtype, vn=NULL) {
    switch(outtype,
           ## "graphNEL"   ={g_dagl2gn_(zz, vn)},
           "igraph"     ={g_dagl2ig_(zz, vn)},
           "matrix"     ={g_dagl2dm_(zz, vn)},
           "dgCMatrix"  =,
           "Matrix"     ={g_dagl2sm_(zz, vn)}
           )
}

## ##########################################################
##
## adj_list2XX
##
## ##########################################################

## #' @export
## #' @rdname graph_coerce_list
## g_adl2gn_ <- function(zz) stop("Function not implemented") ## FIXME

#' @export
#' @rdname graph_coerce_list
g_adl2ig_ <- function(zz) stop("Function not implemented") ## FIXME

#' @export
#' @rdname graph_coerce_list
g_adl2dm_ <- function(zz) adjList2matrix__(zz)

#' @export
#' @rdname graph_coerce_list
g_adl2sm_ <- function(zz) adjList2dgCMatrix__(zz)

#' @export
#' @rdname graph_coerce_list
g_adl2XX_ <- function(zz, outtype) {
    switch(outtype,
           ## "graphNEL"   ={g_adl2gn_(zz)},
           "igraph"     ={g_adl2ig_(zz)},
           "matrix"     ={g_adl2dm_(zz)},
           "dgCMatrix"  =,
           "Matrix"     ={g_adl2sm_(zz)}
           )
}

## ##########################################################
##
## matrix2XXXX
##
## ##########################################################

#' @export
#' @rdname graph_coerce_list
g_M2adl_ <- function( amat ){
    .check.is.matrix( amat )
    if (!isadjMAT_( amat ))  stop("' amat ' not an adjacency matrix\n")
    vn <- colnames( amat )
    r  <- rowmat2list__( amat )
    i  <- lapply(r, function(z) which(z!=0))
    out <- lapply(i, function(j) vn[j])
    names(out) <- vn
    out
}

#' @export
#' @rdname graph_coerce_list
g_M2ugl_ <- function( amat ){
    ## FIXME: M2ugList: Need a check for undirectedness
    .check.is.matrix( amat )
    max_cliqueMAT( amat )[[1]]
}

#' @export
#' @rdname graph_coerce_list
g_M2dagl_ <- function( amat ){
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

#' @export
#' @rdname graph_coerce_list
g_ugl2M_ <- function(glist, vn=NULL, result="matrix"){
    result <- match.arg(result, c("matrix", "dgCMatrix", "Matrix"))
    if (is.null(vn)) vn <- unique.default(unlist(glist))
    switch(result,
           "Matrix"    =,
           "dgCMatrix" = {g_ugl2sm_( glist, vn )},
           "matrix"    = {g_ugl2dm_( glist, vn )}  )
}

#' @export
#' @rdname graph_coerce_list
g_dagl2M_ <- function(glist, vn=NULL, result="matrix"){
    result <- match.arg(result, c("matrix", "dgCMatrix", "Matrix"))
    if (is.null(vn)) vn <- unique.default(unlist(glist))
    switch(result,
           "Matrix"    =,
           "dgCMatrix" = {g_dagl2sm_( glist, vn )},
           "matrix"    = {g_dagl2dm_( glist, vn )}  )
}

#' @export
#' @rdname graph_coerce_list
g_adl2M_ <- function(alist, result="matrix"){
    result <- match.arg(result, c("matrix", "dgCMatrix", "Matrix"))
    switch(result,
           "matrix"   = {g_adl2dm_( alist )},
           "Matrix"   =,
           "dgCMatrix"= {g_adl2sm_( alist )})
}


ug_list2igraph <- function(x) {
    is_iso <- sapply(x, length) == 1
    iso <- unlist(x[is_iso])
    ed  <- x[!is_iso]
    
    if (length(ed)==0){
        g <- make_empty_graph(length(iso), directed = FALSE)
        V(g)$name <- iso
    } else {
        em <- do.call(rbind, unlist(lapply(ed, names2pairs), recursive = FALSE))
        em <- t.default(em[!duplicated(em),])
        g <- make_graph(em, isolates=iso, directed = FALSE)        
    }
    return(g)
}


dag_list2igraph <- function(x) {
    is_iso <- sapply(x, length) == 1
    iso <- unlist(x[is_iso])
    ed  <- x[!is_iso]

    if (length(ed) > 0 && length(iso) > 0) {
        aa <- !sapply(iso, function(v) {isin_(ed, v)})
        iso <- iso[aa]
    }
    
    if (length(ed)==0) {
        g <- igraph::make_empty_graph(length(iso), directed = TRUE)
        V(g)$name <- iso
    } else {

        ## NOTE This is from-to rather than to-from as in graph package
        pp <- lapply(ed,
               function(ee) {
                   names2pairs(ee[-1], ee[1], sort=FALSE)
               })


        
        ed2 <- unlist(pp, recursive = FALSE)
        em <- do.call(rbind, ed2)

        ## em <- do.call(rbind, unlist(lapply(ed,
        ##                                    function(ee) {
        ##                                        names2pairs(ee[1], ee[-1])
        ##                                    }), recursive = FALSE))
        


        em <- t.default(em[!duplicated(em),])
        g <- igraph::make_graph(em, isolates=iso, directed = TRUE)        
    }
    return(g)
}
