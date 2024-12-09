## #####################################################################
##
#' @title Graph coercion
#' @description Methods for changing graph representations
#' @name graph_coerce
##
## #####################################################################
#' 
#' @param object A graph object
#' @param class The desired output class
#'
#' @details coerceGraph is used in the book "Graphical models with R".
#' A more generic approach is as().
#'
#' @examples
#' 
#' g1 <- ug(~a:b+b:c)
#' as(g1, "igraph")
#' as(g1, "matrix")
#' as(g1, "Matrix")
#' as(g1, "dgCMatrix")
#'
#' ## graph_as(g1, "ugList") ## Fails
#' ## getCliques(g1)         ## Works
#' 
#' l1 <- list(c("a" ,"b"), c("b", "c"))
#' 
#' @export
#' @rdname graph_coerce
coerceGraph <- function(object, class){
    as(object, class)
}



## ####################################################################
##
## as( ): Coercion between igraph, matrix, dgCMatrix.
##
## ####################################################################

setOldClass("igraph")
setAs("igraph",    "matrix",    function(from) g_ig2dm_(from))
setAs("igraph",    "dgCMatrix", function(from) g_ig2sm_(from))
setAs("matrix",    "igraph",    function(from) g_dm2ig_(from))
setAs("dgCMatrix", "igraph",    function(from) g_sm2ig_(from))



#' @export
#' @rdname graph_coerce
#'
#' @param outtype The desired output outtype
#' @param intype The desired output outtype (only relevant if object is a list)
#' 
graph_as  <- function(object, outtype, intype=NULL){
    
    if (!inherits(object, c(.graph_type(), "list", "formula")))
        stop("Wrong input format\n")
       
    if (inherits(object, c("list", "formula")))
        return(.handle_list_case(object, outtype, intype))

    if (inherits(object, c("matrix", "Matrix")))
        return(.handle_matrix_case(object, outtype))

    if (inherits(object, c("igraph")))
        return(.handle_graph_case(object, outtype))
}


.handle_matrix_case <- function(object, outtype){
    if (outtype %in% .graph_type())
        return(as(object, outtype))

    id <- match(outtype, .list_format())
    idt <- .get_idtype(id)
    
    switch(id,
           "ugl" ={g_M2ugl_(object)},
           "dagl"={g_M2dagl_(object)},
           "adl" ={g_M2adl_(object)})                            
}

.handle_graph_case <- function(object, outtype){
    if (outtype %in% .graph_type())
        return(as(object, outtype))
    stop(".handle_graph_case not implemented for this outtype\n")
}

.handle_list_case <- function(object, outtype, intype) {
    if (inherits(object, "formula"))
        object <- rhsf2list(object)
    
    id <- match(intype, .list_format())
    idt <- .get_idtype(id)
    
    switch(id,
           "ugl"  ={g_ugl2XX_(object, outtype)},
           "dagl" ={g_dagl2XX_(object, outtype)},
           "adl"  ={g_adl2XX_(object, outtype)})                            
}

.get_idtype  <- function(id) {
    if (id %in% 1:4) idt <- "ugl"
    else if (id %in% 5:8) idt <- "dagl"
    else if (id %in% 9:12) idt <- "adl"
    else stop("invalid 'type'\n")
    idt
}

.graph_type <- function() {
    c("matrix", "dgCMatrix", "igraph")
}

.list_format <- function() {
    c("ugList",   "uglist",   "ug_list",  "ugl",
      "dagList",  "daglist",  "dag_list", "dagl",
      "adjList",  "adjlist",  "adj_list", "adl")
}




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

#' @export
#' @rdname graph_coerce_list
g_dagl2ig_ <- function(zz, vn=NULL) {    
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
    check_is_matrix( amat )
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
    check_is_matrix( amat )
    max_cliqueMAT( amat )[[1]]
}

#' @export
#' @rdname graph_coerce_list
g_M2dagl_ <- function( amat ){
    check_is_matrix( amat )
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
g_dagl2M_ <- function(glist, vn=NULL, result="matrix") {
    result <- match.arg(result, c("matrix", "dgCMatrix", "Matrix"))
    if (is.null(vn)) vn <- unique.default(unlist(glist))
    switch(result,
           "Matrix"    =,
           "dgCMatrix" = {g_dagl2sm_( glist, vn )},
           "matrix"    = {g_dagl2dm_( glist, vn )}  )
}

#' @export
#' @rdname graph_coerce_list
g_adl2M_ <- function(alist, result="matrix") {
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

        em <- t.default(em[!duplicated(em),])
        g <- igraph::make_graph(em, isolates=iso, directed = TRUE)        
    }
    return(g)
}


## ##################################################################
##
#' @title API for coercing graph representations
#' @description API for coercing graph representations.
#' @name graph-coerce-api
##
## ###################################################################
#'
#' @param object An object representing a graph
#' @param result Either 'matrix' (dense) or 'dgCMatrix' (sparse, can
#'     be abbreviated to 'Matrix').
#'
#' @details No checking is made. In the function the following names are used:
#' 
#' * "ig": "igraph";
#' 
#' * "sm": "dgCMatrix" (sparse matrix);
#' 
#' * "dm": "matrix" (dense matrix)
#' 
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @seealso \code{\link{ug}}, \code{\link{dag}}
#' @keywords utilities
#' 

## ##### dense matrix to something #####

#' @export
#' @rdname graph-coerce-api
g_dm2sm_ <- function(object) {
    M2dgCMatrix__(object)
}

#' @export
#' @rdname graph-coerce-api
g_dm2ig_ <- function(object) {
    mode <- if (isSymmetric(object)) "undirected" else "directed"
    if (is.null(rownames(object))){
        rownames(object) <- colnames(object) <- 1:ncol(object)
    }
    gg <- igraph::graph.adjacency(object, mode=mode)    
    igraph::V(gg)$label <- igraph::V(gg)$name <- colnames(object)
    gg
}

## sparse matrix to something

#' @export
#' @rdname graph-coerce-api
g_sm2dm_ <- function(object) {
    M2matrix__(object)
}

#' @export
#' @rdname graph-coerce-api
g_sm2ig_ <- g_dm2ig_


## igraph to something

#' @export
#' @rdname graph-coerce-api
g_ig2dm_ <- function(object) {
    M2matrix__(igraph::get.adjacency(object))
}

#' @export
#' @rdname graph-coerce-api
g_ig2sm_ <- function(object) {
    igraph::get.adjacency(object)
}

## matrix/dgCMatrix to something

#' @export
#' @rdname graph-coerce-api 
g_xm2ig_ <- function( object ) { ## M | igraph
    check_is_matrix( object )
    as(object , "igraph")
}

#' @export
#' @rdname graph-coerce-api
g_xm2dm_ <- function( object ) {  ## M
    check_is_matrix( object )
    M2matrix__(object)
}

#' @export
#' @rdname graph-coerce-api
g_xm2sm_ <- function( object ) { ## M
    check_is_matrix( object )
    M2dgCMatrix__(object)
}

#' @export
#' @rdname graph-coerce-api
g_xm2xm_ <- function(object, result="matrix") {
    switch(result,
           "matrix"={g_xm2dm_(object)},
           "Matrix"=,
           "dgCMatrix"={g_xm2sm_(object)})
}





