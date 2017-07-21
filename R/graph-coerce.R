## ####################################################################
##
## This file implements as() and coerceGraph() for conversion between
## different graph representations: graphNEL, igraph, matrix and
## dgCMatrix. (coerceGraph methods are used in the GMwR book).
## Generally, the coerceGraph methods are faster than the as( )
## methods.
##
## Implementation is based on the api-interface: xx2yy_ and as_()
##
## ####################################################################

## ####################################################################
##
## as( ): Coercion between graphNEL, igraph, matrix, dgCMatrix.
##
## ####################################################################

setOldClass("igraph")

## From graphNEL
## -------------
setAs("graphNEL", "igraph",    function(from) gn2ig_(from))
setAs("graphNEL", "matrix",    function(from) gn2dm_(from))
setAs("graphNEL", "dgCMatrix", function(from) gn2sm_(from))

## From matrix
## -----------
setAs("matrix", "igraph", function(from) dm2ig_(from))
## matrix -> graphNEL : is in graph package (I guess)
## matrix -> dgCMatrix: is in Matrix package. Should be used

## From dgCMatrix
## -----------
setAs("dgCMatrix", "igraph", function(from){ sm2ig_(from)})
## Matrix -> graphNEL : in the graph package (I guess)
## Matrix -> matrix : in the Matrix package

## From igraph
## -----------
setAs("igraph",   "graphNEL",    function(from) ig2gn_(from))
setAs("igraph",   "matrix",      function(from) ig2dm_(from))
setAs("igraph",   "dgCMatrix",   function(from) ig2sm_(from))

## #####################################################################
##
## coerceGraph methods
##
## #####################################################################

#' @title Graph coercion
#'
#' @description Methods for changing graph representations
#'
#' @name graph-coerce
#'
#' @param object A graph object
#' @param Class The desired output class
#'
#' @details coerceGraph is used in the book "Graphical models with R".
#' A more generic approach is as().
#'
#' @rdname graph-coerce
coerceGraph <- function(object, Class){
    as_(object, Class)
}






































### OBSOLETE STUFF BELOW HERE ###

## setAs("igraph",   "Matrix",      function(from) M2dgCMatrix(igraph::get.adjacency(from)))

#### setAs("graphNEL", "igraph",    function(from) graphNEL2igraph(from))
#### setAs("graphNEL", "matrix",    function(from) graphNEL2M(from, result="matrix"))
#### setAs("graphNEL", "Matrix",    function(from) graphNEL2M(from, result="Matrix"))
#### setAs("graphNEL", "dgCMatrix", function(from) graphNEL2M(from, result="Matrix"))
#### setAs("graphNEL", "Matrix",    function(from) graphNEL2M(from, result="Matrix"))
#### setAs("matrix", "igraph", function(from) M2igraph(from))
####setAs("Matrix", "igraph", function(from){ M2igraph( as.matrix( from )) })
## ## setAs("igraph",   "graphNEL",    function(from) igraph::igraph.to.graphNEL(from))
## ## setAs("igraph",   "matrix",      function(from) as(igraph::get.adjacency(from),"matrix"))
## ## setAs("igraph",   "Matrix",      function(from) M2dgCMatrix(igraph::get.adjacency(from)))
## ## setAs("igraph",   "dgCMatrix",   function(from) M2dgCMatrix(igraph::get.adjacency(from)))





## #' @rdname graph-coerce
## coerceGraph <- function(object, Class){
##   UseMethod("coerceGraph")
## }

## #' @rdname graph-coerce
## coerceGraph.graphNEL <- function(object, Class){
##   Class <- match.arg(Class, c("graphNEL", "matrix", "dgCMatrix", "igraph"))
##   switch(Class,
##          "graphNEL" = {object},
##          "igraph"   = {gn2ig_(object)},
##          "matrix"   = {gn2dm_(object)},
##          "dgCMatrix"= {gn2sm_(object)}
##          )
## }

## #' @rdname graph-coerce
## coerceGraph.matrix <- function(object, Class){
##   Class <- match.arg(Class, c("graphNEL", "matrix", "dgCMatrix", "igraph"))
##   switch(Class,
##          "graphNEL" ={ dm2gn_(object)},
##          "igraph"   ={ dm2ig_(object)},
##          "matrix"   ={ object },
##          "dgCMatrix"={ dm2sm_(object)})
## }

## #' @rdname graph-coerce
## coerceGraph.dgCMatrix <- function(object, Class){
##   Class <- match.arg(Class, c("graphNEL", "matrix", "dgCMatrix", "igraph"))    
##   switch(Class,
##          "graphNEL" ={ sm2gn_(object)},
##          "igraph"   ={ sm2ig_(object)},
##          "matrix"   ={ sm2dm_(object)},
##          "dgCMatrix"={ object  })
## }

## #' @rdname graph-coerce
## coerceGraph.igraph <- function(object, Class){
##   Class <- match.arg(Class, c("graphNEL", "matrix", "dgCMatrix", "igraph"))        
##   switch(Class,
##          "graphNEL" ={ ig2gn_(object)},
##          "igraph"   ={ object},
##          "matrix"   ={ ig2dm_(object)},
##          "dgCMatrix"={ ig2sm_(object)}
##          )
## }





## coerceGraph <- function(object, Class){
##   UseMethod("coerceGraph")
## }

## #' @rdname graph-coerce
## coerceGraph.graphNEL <- function(object, Class){
##   Class <- match.arg(Class, c("graphNEL","matrix","dgCMatrix","Matrix","igraph"))
##   switch(Class,
##          "graphNEL" = {object},
##          "igraph"   = {graphNEL2igraph(object)},
##          "matrix"   = {graphNEL2matrix(object)},
##          "Matrix"   =,
##          "dgCMatrix"= {graphNEL2dgCMatrix(object)}
##          )
## }

## #' @rdname graph-coerce
## coerceGraph.matrix <- function(object, Class){
##   Class <- match.arg(Class, c("graphNEL","matrix","dgCMatrix","Matrix","igraph"))
##   switch(Class,
##          "graphNEL" ={ M2graphNEL(object)},
##          "igraph"   ={ M2igraph(object)},
##          "matrix"   ={ object },
##          "Matrix"   =,
##          "dgCMatrix"={ M2dgCMatrix( object )})
## }

## #' @rdname graph-coerce
## coerceGraph.dgCMatrix <- function(object, Class){
##   Class <- match.arg(Class, c("graphNEL","igraph","matrix","dgCMatrix","Matrix"))
##   switch(Class,
##          "graphNEL" ={ M2graphNEL(object)},
##          "igraph"   ={ M2igraph(dgCMatrix2matrix(object))},
##          "matrix"   ={ M2matrix( object )},
##          "Matrix"   =,
##          "dgCMatrix"={ object  })
## }

## #' @rdname graph-coerce
## coerceGraph.igraph <- function(object, Class){
##   Class <- match.arg(Class, c("graphNEL","matrix","dgCMatrix","Matrix","igraph"))
##   switch(Class,
##          "graphNEL"={ igraph2graphNEL(object)},
##          "igraph"  ={ object},
##          "matrix"  ={ igraph2matrix(object)},
##          "Matrix"  =,
##          "dgCMatrix"={ igraph2dgCMatrix(object)}
##          )
## }
