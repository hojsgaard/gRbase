
## children <- function(set, object){
##   if (graph::edgemode(object)=="undirected")
##     return(NULL)
##   ch <- structure(unlist(graph::edges(object)[set]), names=NULL)
##   if (length(ch)) ch else NULL
## }


## #' @export
## #' @rdname graph_coerce_list
## g_ugl2gn_ <- function(glist, vn=NULL){
##     if (is.null(vn)) vn <- unique.default(unlist(glist))
##     zzz <- lapply(glist, function(xx) names2pairs(xx, sort=TRUE, result="matrix"))
##     ftM <- do.call(rbind, zzz)
##     if ( nrow(ftM) > 0 ){
##         tofrom <- unique(rowmat2list__(ftM))
##         fff <- do.call(rbind, tofrom)
##         graph::ftM2graphNEL(fff, V=as.character(vn), edgemode="undirected")
##     } else {
##         new("graphNEL", nodes=as.character(vn), edgemode="undirected")
##     }
## }

## glist2igraph <- function(x, directed=TRUE) {
##     is_iso <- sapply(x, length) == 1
##     iso <- unlist(x[is_iso])
##     ed  <- x[!is_iso]
    
##     if (length(ed)==0){
##         g <- igraph::make_empty_graph(length(iso), directed = directed)
##         V(g)$name <- iso
##     } else {
##         em <- do.call(cbind, unlist(lapply(ed, names2pairs), recursive = FALSE))
##         g <- igraph::make_graph(em, isolates=iso, directed = directed)        
##     }
##     return(g)
## }


## #' @export
## ## #' @rdname graph_vpar
## vpar.graphNEL <- function(object, getv=TRUE, forceCheck=TRUE){

##     ## ## FIXME: PERHAPS NOT A GOOD IDEA:
##     ## no_edges  <- all(sapply(graph::edges(object), length) == 0)

##     ##    if (!no_edges){
##         if (forceCheck && graph::edgemode(object)=="undirected")
##             stop("Graph is undirected; (v,pa(v)) does not exist...\n")
##     ##}
    
##     ch <- graph::edges(object) ## Nodes and their children
##     vn <- names(ch)
##     tf <- lapply(seq_along(ch),
##                  function(i)
##                      all_pairs( ch[[i]], vn[i],
##                                   sort=FALSE, result="matrix"))

##     tf <- do.call(rbind, tf) # matrix in to-from form
##     out <- lapply(seq_along(ch),
##                   function(i)
##                       c(vn[i], tf[tf[, 1] == vn[i], 2]))
##     names(out) <- vn

##     if (!getv) # Don't want v, just pa(v)
##         lapply(out, function(x)x[-1])
##     else
##         out
## }



## #' @export
## ## #' @rdname graph_vpar
## vchi.graphNEL <- function(object, getv=TRUE, forceCheck=TRUE){
##     if (forceCheck && graph::edgemode(object)=="undirected")
##         stop("Graph is undirected; (v,pa(v)) does not exist...\n")

##     ch <- graph::edges(object) ## Nodes and their children
##     vn <- names(ch)


##     out <- lapply(seq_along(ch), function(i) c(vn[i], ch[[i]]))
##     names(out) <- vn
##     out

##     if (!getv) # Don't want v, just pa(v)
##         lapply(out, function(x)x[-1])
##     else
##         out
## }

## ### From ->->-> To  ###
## setAs("graphNEL", "igraph",    function(from) g_gn2ig_(from))
## setAs("graphNEL", "matrix",    function(from) g_gn2dm_(from))
## setAs("graphNEL", "dgCMatrix", function(from) g_gn2sm_(from))
## setAs("graphNEL", "Matrix",    function(from) g_gn2sm_(from))

## setAs("igraph",   "graphNEL",  function(from) g_ig2gn_(from))

## matrix -> graphNEL : is in graph package (I guess)
## matrix -> dgCMatrix: is in Matrix package. Should be used
## Matrix -> graphNEL : in the graph package (I guess)
## Matrix -> matrix : in the Matrix package



## ############################################################
##
## ##### .as_fun #####
##
## ############################################################

## .as_fun <- function(object, class){
##     UseMethod(".as_fun")
## }

## ## #' @rdname graph-coerce-user
## .as_fun.graphNEL <- function(object, class){
##     switch(class,
##            "graphNEL"  = {object},
##            "matrix"    = {gn2dm_(object)},
##            "dgCMatrix" = {gn2sm_(object)},
##            "igraph"    = {gn2ig_(object)})          
## }

## ## #' @rdname graph-coerce-user
## .as_fun.matrix  <- function(object, class){
##     switch(class,
##            "graphNEL"  = {dm2gn_(object)},
##            "matrix"    = {object},
##            "dgCMatrix" = {dm2sm_(object)},
##            "igraph"    = {dm2ig_(object)})          
## }

## ## #' @rdname graph-coerce-user
## .as_fun.dgCMatrix  <- function(object, class){
##     switch(class,
##            "graphNEL"  = {sm2gn_(object)},
##            "matrix"    = {sm2dm_(object)},
##            "dgCMatrix" = {object},
##            "igraph"    = {sm2ig_(object)})          
## }

## ## #' @rdname graph-coerce-user
## .as_fun.igraph  <- function(object, class){
##     switch(class,
##            "graphNEL"  = {ig2gn_(object)},
##            "matrix"    = {ig2dm_(object)},
##            "dgCMatrix" = {ig2sm_(object)},
##            "igraph"    = {object})          
## }

