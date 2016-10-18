## ####################################################################
##
## This file implements as() for conversion between different graph
## representations: graphNEL, igraph, matrix and dgCMatrix

## This file also implements coerceGraph methods as they are used in
## the GMwR book.

## Generally, the coerceGraph methods are faster than the as( )
## methods

## Lower level coercion methods (including methods for coercion of
## generating classes) can be found in graph-xxx2yyy

## FIXME: How to document "setAs" methods (using roxygen)?
##
## ####################################################################

## ####################################################################
##
## as( ): Coercion between graphNEL, igraph and matrix
##
## ####################################################################

setOldClass("igraph")

## From graphNEL
## -------------
setAs("graphNEL", "igraph",      function(from) graphNEL2igraph(from))
setAs("graphNEL", "matrix",      function(from) graphNEL2M(from, result="matrix"))
setAs("graphNEL", "Matrix",      function(from) graphNEL2M(from, result="Matrix"))
setAs("graphNEL", "dgCMatrix",   function(from) graphNEL2M(from, result="Matrix"))

## From matrix
## -----------
setAs("matrix", "igraph", function(from) M2igraph(from))

## matrix -> graphNEL : is in graph package (I guess)
## matrix -> dgCMatrix: is in Matrix package. Should be used
## matrix -> Matrix : is in Matrix package but care should be taken
## because the output can be of different types

## From Matrix
## -----------
setAs("Matrix", "igraph", function(from){ M2igraph( as.matrix( from )) })

# Matrix -> graphNEL : in the graph package (I guess)
# Matrix -> matrix : in the Matrix package

## From igraph
## -----------
setAs("igraph",   "graphNEL",    function(from) igraph::igraph.to.graphNEL(from))
setAs("igraph",   "matrix",      function(from) as(igraph::get.adjacency(from),"matrix"))
setAs("igraph",   "Matrix",      function(from) MAT2dgCMatrix(igraph::get.adjacency(from)))
setAs("igraph",   "dgCMatrix",   function(from) MAT2dgCMatrix(igraph::get.adjacency(from)))

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
#' @param result The desired output type
#' 
coerceGraph <- function(object, result){
  UseMethod("coerceGraph")
}

#' @rdname graph-coerce
coerceGraph.graphNEL <- function(object, result){
  result <- match.arg(result, c("graphNEL","matrix","dgCMatrix","Matrix","igraph"))
  switch(result,
         "graphNEL"={object},
         "igraph"  ={gg <- igraph::igraph.from.graphNEL(object)
                     igraph::V(gg)$label <- igraph::V(gg)$name
                     gg
                   },
         "matrix" =,
         "Matrix" =,
         "dgCMatrix"={
           graphNEL2M(object, result=result)
         }
         )
}

#' @rdname graph-coerce
coerceGraph.matrix <- function(object, result){
  result <- match.arg(result, c("graphNEL","matrix","dgCMatrix","Matrix","igraph"))
  switch(result,
         "graphNEL" ={ as(object,"graphNEL")},
         "igraph"   ={ M2igraph(object)},
         "matrix"   ={ object },
         "Matrix"   =,
         "dgCMatrix"={ matrix2dgCMatrix( object )})
}

#' @rdname graph-coerce
coerceGraph.dgCMatrix <- function(object, result){
  result <- match.arg(result, c("graphNEL","igraph","matrix","dgCMatrix","Matrix"))
  switch(result,
         "graphNEL" ={ as(object,"graphNEL")},
         "igraph"   ={ M2igraph(dgCMatrix2matrix(object))},
         "matrix"   ={ dgCMatrix2matrix( object )},
         "Matrix"   =,
         "dgCMatrix"={ object  })
}

#' @rdname graph-coerce
coerceGraph.igraph <- function(object, result){
  result <- match.arg(result, c("graphNEL","matrix","dgCMatrix","Matrix","igraph"))
  switch(result,
         "graphNEL"={ igraph::igraph.to.graphNEL(object)},
         "igraph"  ={ object},
         "matrix"  ={ as(igraph::get.adjacency(object),"matrix")},
         "Matrix"  =,
         "dgCMatrix"={ MAT2dgCMatrix(igraph::get.adjacency(object))}
         )
}


