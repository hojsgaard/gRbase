## ##################################################################
##
## Properties of generating class (i.e. of graph defined by a
## generating class)
##
## ##################################################################

#' @title Properties of a generating class (for defining a graph)
#' 
#' @description A set of generators define an undirected graph, here
#'     called a dependence graph. Given a set of generators it is
#'     checked 1) if the dependence dependence graph is in
#'     1-1-correspondance with the genrators (such that the
#'     corresponding model is graphical) and 2) if the dependence
#'     graph is chordal (triangulated) (such that the corresponding
#'     model is decomposable).
#'
#' @name graph-gcproperties
#' 
#' @details A set of sets of variables, say A_1, A_2, ... A_K is
#'     called a generating class for a graph with vertices V and edges
#'     E. If two variables a,b are in the same generator, say A_j,
#'     then a and b are vertices in the graph and there is an
#'     undirected edge between a and b.
#'
#'     The graph induced by \code{g1 = ~a:b + a:c + b:c + c:d} has
#'     edges \code{ab,ac,bc,cd}. The
#'     cliques of this graph are \code{abc,cd}. Hence there is not a
#'     1-1-correspondance between the graph and the generators.
#' 
#'     On the other hand, \code{g2 <- ~a:b:c + c:d} induces the same
#'     graph in this case there is a 1-1-correspondance.
#' 
#'     The graph induced by \code{g3 <- ~a:b + b:c + c:d + d:a} is in
#'     1-1-correspondance with its dependence graph, but the graph is not
#'     chordal.
#' 
#' @aliases isGraphical isGraphical.default isDecomposable
#' isDecomposable.default
#' @param x A generating class given as right hand sided formula or a list; see
#' 'examples' below
#' @return TRUE or FALSE
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @seealso \code{\link{mcs}}, \code{\link{rip}}
#' @keywords utilities models
#' @examples
#' 
#' g1 <- ~a:b+a:c+b:c+c:d
#' g2 <- ~a:b:c+c:d
#' g3 <- ~a:b + b:c + c:d + d:a
#' 
#' isGraphical( g1 ) # FALSE
#' isGraphical( g2 ) # TRUE
#' isGraphical( g3 ) # TRUE
#' 
#' isDecomposable( g1 ) # FALSE
#' isDecomposable( g2 ) # TRUE
#' isDecomposable( g3 ) # TRUE
#' 
#' ## A generating class can be given as a list:
#' f <- list(c("a","b"), c("b","c"), c("a","c"))
#' isGraphical( f )
#' isDecomposable( f )
#' 
#' 
#' @export isGraphical
isGraphical <- function(x){
    UseMethod("isGraphical")
}

#' @rdname graph-gcproperties
isGraphical.default <- function( x ){
    if ((cls <- class( x )) %in% c("formula","list")){
        switch(cls,
               "formula"={
                   if (length(x)==3)
                       x <- formula(delete.response(terms(x)))
                   .isGraphical_glist( rhsf2list( x ) )
               },
               "list"={
                   if (!all(unlist(lapply(x, is.atomic))))
                       stop("'x' must be a list of atomic vectors")
                   if (length( x ) == 0)
                       stop("'x' must have positive length")
                   .isGraphical_glist( x )
               },
               stop("'x' must be a formula or a list of atomic vectors\n")
               )

    }
}

.isGraphical_glist <- function(x){
    vn <- unique( unlist(x) )
    amat <- ugList2M(x, vn=vn)
    cliq <- maxCliqueMAT(amat)[[1]]
    all(unlist(lapply(cliq, function(sss) isin(x, sss))))
}

#' @rdname graph-gcproperties
isDecomposable <- function(x){
    UseMethod("isDecomposable")
}

#' @rdname graph-gcproperties
isDecomposable.default <- function( x ){
    if ((cls <- class( x )) %in% c("formula","list")){
        switch(cls,
               "formula"={
                   if (length(x)==3)
                       x <- formula(delete.response(terms(x)))
                   .isDecomposable_glist( rhsf2list( x ) )
               },
               "list"={
                   if (!all(unlist(lapply(x, is.atomic))))
                       stop("'x' must be a list of atomic vectors")
                   if (length( x ) == 0)
                       stop("'x' must have positive length")
                   .isDecomposable_glist( x )
               },
               stop("'x' must be a formula or a list of atomic vectors\n")
               )

    }
}

.isDecomposable_glist <- function(x){
    vn <- unique( unlist(x) )
    amat <- ugList2M(x, vn=vn)
    cliq <- maxCliqueMAT(amat)[[1]]
    isg <- all(unlist(lapply(cliq, function(sss) isin(x, sss))))
    if (isg){
        length( mcsMAT( amat ) ) > 0
    } else
        FALSE
}

## Is model defined by <glist> graphical and strongly decomposable
## if discrete=NULL, then the check is just if the graph is decomposable
## Issues: Fails on the "empty graph".

isGSD_glist <- function(glist, vn=unique(unlist(glist)), discrete=NULL)
{
  amat <- ugList2M(glist, vn=vn)
  cliq <- maxCliqueMAT(amat)[[1]]
  isg  <- all(unlist(lapply(cliq, function(sss) isin(glist, sss))))
  if (!isg){
    return(c(isg=FALSE, issd=FALSE))
  } else {
    return(c(isg=TRUE, issd=length(mcsmarkedMAT(amat,discrete=discrete)) > 0))
  }
}

properties_glist <- function(glist,
                             vn=unique(unlist(glist)),
                             amat=ugList2M(glist,vn=vn),
                             cliq=maxCliqueMAT(amat)[[1]], discrete=NULL){

  isg <- all(unlist(lapply(cliq, function(sss) isin(glist, sss))))
  if (!isg){
    return(c(isg=FALSE, issd=FALSE))
  } else {
    return(c(isg=TRUE, issd=length(mcsmarkedMAT(amat,discrete=discrete)) > 0))
  }
}


##
## is.DAG, is.UG, is.TUG implemented for graphNEL, matrix and Matrix
##

is.adjMAT <- function(x){
    .check.is.matrix(x)
    isadjMAT_( x )
}

## #####################################################
##
## Check properties of graphs.
##
## #####################################################

#' @title Check properties of graphs.
#' 
#' @description Check if a graph is 1) a directed acyclic graph (DAG),
#'     2) a directed graph (DG), 3) an undirected graph (UG), 4) a
#'     triangulated (chordal) undirected graph (TUG). This is done for
#'     graphs represented as 1) graphNEL (from the graph package), 2)
#'     an adjacency matrix, 3) a sparse adjacency matrix (a dgCMatrix
#'     from the Matrix package).
#'
#' @name graph-is
#' 
#' @details A non-zero value at entry (i,j) in an adjacency matrix A
#'     for a graph means that there is an edge from i to j. If also
#'     (j,i) is non-zero there is also an edge from j to i. In this
#'     case we may think of a bidirected edge between i and j or we
#'     may think of the edge as being undirected.  We do not
#'     distinguish between undirected and bidirected edges in the
#'     gRbase package.  On the other hand, graphNEL objects from the
#'     graph package makes such a distinction (the function
#'     \code{edgemode()} will tell if edges are "directed" or
#'     "undirected" in a graphNEL object).
#' 
#' The function \code{is.UG()} checks if the adjacency matrix is
#' symmetric (If applied to a graphNEL, the adjacency matrix is
#' created and checked for symmetry.)
#' 
#' The function \code{is.TUG()} checks if the graph is undirected and
#' triangulated (also called chordal) by checking if the adjacency matrix is
#' symmetric and the vertices can be given a perfect ordering using maximum
#' cardinality seach.
#' 
#' The function \code{is.DG()} checks if a graph is directed, i.e., that there
#' are no undirected edges. This is done by computing the elementwise product
#' of A and the transpose of A; if there are no non--zero entries in this
#' product then the graph is directed.
#' 
#' The function \code{is.DAG()} will return \code{TRUE} if all edges are
#' directed and if there are no cycles in the graph. (This is checked by
#' checking if the vertices in the graph can be given a topological ordering
#' which is based on identifying an undirected edge with a bidrected edge).
#' 
#' There is a special case, namely if the graph has no edges at all (such that
#' the adjacency matrix consists only of zeros). Such a graph is both
#' undirected, triangulated, directed and directed acyclic.
#' 
#' @aliases is.DAG is.DAG.graphNEL is.DAG.default is.DAGMAT is.DG
#'     is.DG.graphNEL is.DG.default is.DGMAT is.UG is.UG.graphNEL
#'     is.UG.default is.UGMAT is.TUG is.TUG.graphNEL is.TUG.default
#'     is.TUGMAT is.adjMAT
#' @param object A graph represented as 1) graphNEL (from the graph
#'     package), 2) an adjacency matrix, 3) a sparse adjacency matrix
#'     (a dgCMatrix from the Matrix package).
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @seealso \code{\link{dag}}, \code{\link{ug}}
#' @keywords utilities
#' @examples
#' 
#' 
#' ## DAGs
#' dagNEL  <- dag(~ a:b:c + c:d:e, result="graphNEL")
#' dagMAT  <- dag(~ a:b:c + c:d:e, result="matrix")
#' dagMATS <- dag(~ a:b:c + c:d:e, result="dgCMatrix")
#' 
#' ## Undirected graphs
#' ugNEL  <- ug(~a:b:c + c:d:e, result="graphNEL")
#' ugMAT  <- ug(~a:b:c + c:d:e, result="matrix")
#' ugMATS <- ug(~a:b:c + c:d:e, result="dgCMatrix")
#' 
#' ## Is it a DAG?
#' is.DAG(dagNEL)
#' is.DAG(dagMAT)
#' is.DAG(dagMATS)
#' 
#' is.DAG(ugNEL)
#' is.DAG(ugMAT)
#' is.DAG(ugMATS)
#' 
#' ## Is it an undirected graph
#' is.UG(dagNEL)
#' is.UG(dagMAT)
#' is.UG(dagMATS)
#' 
#' is.UG(ugNEL)
#' is.UG(ugMAT)
#' is.UG(ugMATS)
#' 
#' ## Is it a triangulated (i.e. chordal)  undirected graph
#' is.TUG(dagNEL)
#' is.TUG(dagMAT)
#' is.TUG(dagMATS)
#' 
#' is.TUG(ugNEL)
#' is.TUG(ugMAT)
#' is.TUG(ugMATS)
#' 
#' ## Example where the graph is not triangulated
#' ug2NEL  <- ug(~ a:b + b:c + c:d + d:a, result="graphNEL")
#' ug2MAT  <- ug(~ a:b + b:c + c:d + d:a, result="matrix")
#' ug2MATS <- ug(~ a:b + b:c + c:d + d:a, result="dgCMatrix")
#' 
#' is.TUG(ug2NEL)
#' is.TUG(ug2MAT)
#' is.TUG(ug2MATS)
#' 
#' ## Bidirected graphs
#' graph::edgemode(ugNEL)
#' graph::edgemode(ugNEL) <- "directed"
#' graph::edgemode(ugNEL)
#' is.DAG(ugNEL)
#' is.UG(ugNEL)
#' 
#' 
#' @export is.DAG
is.DAG <- function(object){UseMethod("is.DAG")}

#' @rdname graph-is
is.DAG.graphNEL <- function(object){
    is.DAGMAT( gn2sm_(object) )
}

#' @rdname graph-is
is.DAG.default <- function( object ){
    .check.is.matrix(object)
    isdagMAT_( object )
}

#' @rdname graph-is
is.DAGMAT <- function(object){
    isdagMAT_( object )
}

#is.DAG.matrix <- is.DAG.Matrix <- is.DAGMAT

## ######################################

#' @rdname graph-is
is.UG <- function(object){
    UseMethod("is.UG")
}

#' @rdname graph-is
is.UG.graphNEL <- function(object){
    isugMAT_( gn2dm_( object ) )
}

#' @rdname graph-is
is.UG.default <- function( object ){
    .check.is.matrix(object)
    isugMAT_( object )
}

#' @rdname graph-is
is.UGMAT <- function(object){
    isugMAT_(object)
}

## ######################################

#' @rdname graph-is
is.TUG <- function(object){
  UseMethod("is.TUG")
}

#' @rdname graph-is
is.TUG.graphNEL <- function(object){
    z <- gn2dm_( object )
    if (!isugMAT_( z ))
        FALSE
    else
        length( ripMAT( z ) )>0
}

#' @rdname graph-is
is.TUG.default <- function(object){
    .check.is.matrix(object)
    if (isugMAT_( object ))
        length(mcsMAT(object)) > 0
    else
        FALSE
}

#' @rdname graph-is
is.TUGMAT <- function(object){
    isugMAT_(object) && length(mcsMAT(object))>0
}

## ######################################

#' @rdname graph-is
is.DG <- function(object){
    UseMethod("is.DG")
}

#' @rdname graph-is
is.DG.graphNEL <- function(object){
    is.DGMAT( gn2dm_(object) )
}

#' @rdname graph-is
is.DG.default <- function(object){
    .check.is.matrix(object)
    if (isadjMAT_(object))
        sum(object * t(object)) == 0
    else
        FALSE
}

#' @rdname graph-is
is.DGMAT <- function(object){
    if (!is.adjMAT(object)) stop("Matrix is not adjacency matrix...\n")
    sum(object * t(object)) == 0
}



















## is.DG.matrix <- is.DG.Matrix <- is.DGMAT

## is.UG.graphNEL <- function(object){
##     is.UGMAT( gn2sm_(object) )
## }

## is.UGMAT <- function(object){
##     isugMAT_( object )
## }

## is.UG.matrix <- is.UG.Matrix <- is.UGMAT

## .is.DAGMAT <- function(object){
##     if (!is.adjMAT(object)) stop("Matrix is not adjacency matrix...\n")
##     length(topoSort(object))>0
## }

## .is.UGMAT <- function(object){
##     if (!is.adjMAT(object)) stop("Matrix is not adjacency matrix...\n")
##     isSymmetric(object)
## }


## isGraphical.formula <- function(x){
##     if (length(x)==3)
##         x <- formula(delete.response(terms(x)))
##     .isGraphical_glist( rhsf2list( x ) )
## }

## isGraphical.list <- function(x){
##     if (!all(unlist(lapply(x, is.atomic))))
##         stop("'x' must be a list of atomic vectors")
##     if (length( x ) == 0)
##         stop("'x' must have positive length")
##     .isGraphical_glist( x )
## }

## isDecomposable.formula <- function(x){
##     if (length(x)==3)
##         x <- formula(delete.response(terms(x)))
##     .isDecomposable_glist( rhsf2list( x ) )
## }

## isDecomposable.list <- function(x){
##     if (!all(unlist(lapply(x, is.atomic))))
##         stop("'x' must be a list of atomic vectors")
##     if (length( x ) == 0)
##         stop("'x' must have positive length")
##     .isDecomposable_glist( x )
## }


## .is.adjMAT <- function(x){
##   res <- FALSE
##   if (inherits(x, c("matrix","Matrix"))){
##     d <- dim(x)
##     if (d[1L]==d[2L]){
##       v <- 1:d[1L]
##       if( all(x[cbind(v, v)]==0) ){
##         res <- TRUE
##       }
##     }
##   }
##   res
## }
