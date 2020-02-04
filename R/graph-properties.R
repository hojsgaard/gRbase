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
#' @param x A generating class given as right hand sided formula or a list; see
#' 'examples' below
#' @return TRUE or FALSE
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @seealso \code{\link{mcs}}, \code{\link{rip}}
#' @keywords utilities models
#' @examples
#' 
#' g1 <- ~a:b + a:c + b:c + c:d
#' g2 <- ~a:b:c + c:d
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


#' @export
#' @rdname graph-gcproperties
isGraphical <- function(x){
    UseMethod("isGraphical")
}

#' @export
#' @rdname graph-gcproperties
isGraphical.default <- function( x ){
    if ((cls <- class( x )) %in% c("formula", "list")){
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
    ##amat <- ugList2M(x, vn=vn)
    amat <- ugl2M_(x, vn=vn)
    cliq <- max_cliqueMAT(amat)[[1]]
    all(unlist(lapply(cliq, function(sss) .isin(x, sss))))
}

#' @export
#' @rdname graph-gcproperties
isDecomposable <- function(x){
    UseMethod("isDecomposable")
}

#' @export
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
    ##amat <- ugList2M(x, vn=vn)
    amat <- ugl2M_(x, vn=vn)
    cliq <- max_cliqueMAT(amat)[[1]]
    isg <- all(unlist(lapply(cliq, function(sss) .isin(x, sss))))
    if (isg){
        length(mcsMAT(amat)) > 0
    } else
        FALSE
}

## Is model defined by <glist> graphical and strongly decomposable
## if discrete=NULL, then the check is just if the graph is decomposable
## Issues: Fails on the "empty graph".

#' @export
isGSD_glist <- function(glist, vn=unique(unlist(glist)), discrete=NULL)
{
  ##amat <- ugList2M(glist, vn=vn)
  amat <- ugl2M_(glist, vn=vn)
  cliq <- max_cliqueMAT(amat)[[1]]
  isg  <- all(unlist(lapply(cliq, function(sss) .isin(glist, sss))))
  if (!isg){
    return(c(isg=FALSE, issd=FALSE))
  } else {
    return(c(isg=TRUE, issd=length(mcs_markedMAT(amat, discrete=discrete)) > 0))
  }
}

## #' @export
properties_glist <- function(glist,
                             vn=unique(unlist(glist)),
                             ##amat=ugList2M(glist,vn=vn),
                             amat=ugl2M_(glist, vn=vn),
                             cliq=max_cliqueMAT(amat)[[1]], discrete=NULL){

  isg <- all(unlist(lapply(cliq, function(sss) .isin(glist, sss))))
  if (!isg){
    return(c(isg=FALSE, issd=FALSE))
  } else {
    return(c(isg=TRUE, issd=length(mcs_markedMAT(amat, discrete=discrete)) > 0))
  }
}

#' @export
is.adjMAT <- function(x){
    .check.is.matrix(x)
    isadjMAT_( x )
}

## #####################################################
##
## Check properties of graphs.
##
## #####################################################

##
## is_dag, is_ug, is_tug is_dg implemented for graphNEL, igraph, matrix and dgCMatrix
##


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
#' The function \code{is_ug()} checks if the adjacency matrix is
#' symmetric (If applied to a graphNEL, the adjacency matrix is
#' created and checked for symmetry.)
#' 
#' The function \code{is_tug()} checks if the graph is undirected and
#' triangulated (also called chordal) by checking if the adjacency matrix is
#' symmetric and the vertices can be given a perfect ordering using maximum
#' cardinality seach.
#' 
#' The function \code{is_dg()} checks if a graph is directed, i.e., that there
#' are no undirected edges. This is done by computing the elementwise product
#' of A and the transpose of A; if there are no non--zero entries in this
#' product then the graph is directed.
#' 
#' The function \code{is_dag()} will return \code{TRUE} if all edges are
#' directed and if there are no cycles in the graph. (This is checked by
#' checking if the vertices in the graph can be given a topological ordering
#' which is based on identifying an undirected edge with a bidrected edge).
#' 
#' There is a special case, namely if the graph has no edges at all (such that
#' the adjacency matrix consists only of zeros). Such a graph is both
#' undirected, triangulated, directed and directed acyclic.
#'
#' @aliases is.adjMAT
#' @param object A graph represented as 1) graphNEL (from the graph
#'     package), 2) an adjacency matrix, 3) a sparse adjacency matrix
#'     (a dgCMatrix from the Matrix package).
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @seealso \code{\link{dag}}, \code{\link{ug}}
#' @keywords utilities
#' @examples
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
#' is_dag(dagNEL)
#' is_dag(dagMAT)
#' is_dag(dagMATS)
#' 
#' is_dag(ugNEL)
#' is_dag(ugMAT)
#' is_dag(ugMATS)
#' 
#' ## Is it an undirected graph
#' is_ug(dagNEL)
#' is_ug(dagMAT)
#' is_ug(dagMATS)
#' 
#' is_ug(ugNEL)
#' is_ug(ugMAT)
#' is_ug(ugMATS)
#' 
#' ## Is it a triangulated (i.e. chordal)  undirected graph
#' is_tug(dagNEL)
#' is_tug(dagMAT)
#' is_tug(dagMATS)
#' 
#' is_tug(ugNEL)
#' is_tug(ugMAT)
#' is_tug(ugMATS)
#' 
#' ## Example where the graph is not triangulated
#' ug2NEL  <- ug(~ a:b + b:c + c:d + d:a, result="graphNEL")
#' ug2MAT  <- ug(~ a:b + b:c + c:d + d:a, result="matrix")
#' ug2MATS <- ug(~ a:b + b:c + c:d + d:a, result="dgCMatrix")
#' 
#' is_tug(ug2NEL)
#' is_tug(ug2MAT)
#' is_tug(ug2MATS)
#' 
#' ## Bidirected graphs
#' graph::edgemode(ugNEL)
#' graph::edgemode(ugNEL) <- "directed"
#' graph::edgemode(ugNEL)
#' is_dag(ugNEL)
#' is_ug(ugNEL)
#' 

#' @export
#' @rdname graph-is
is_dag <- function(object){
    UseMethod("is_dag")
}

#' @export
is_dag.graphNEL <- function(object){
    is_dagMAT(as(object, "matrix"))
}

#' @export
is_dag.igraph <- is_dag.graphNEL

#' @export
is_dag.default <- function( object ){
    .check.is.matrix(object)
    isdagMAT_(object)
}

#' @export
#' @rdname graph-is
is_dagMAT <- function(object){
    isdagMAT_(object)
}

#is_dag.matrix <- is_dag.Matrix <- is_dagMAT

## ######################################

#' @export
#' @rdname graph-is
is_ug <- function(object){
    UseMethod("is_ug")
}
#' @export
#' @rdname graph-is
is_ug.graphNEL <- function(object){
    isugMAT_(as(object, "matrix"))
}
#' @export
#' @rdname graph-is
is_ug.igraph <- is_ug.graphNEL
#' @export
#' @rdname graph-is
is_ug.default <- function(object){
    .check.is.matrix(object)
    isugMAT_(object)
}
#' @export
#' @rdname graph-is
is_ugMAT <- function(object){
    isugMAT_(object)
}

## ######################################

#' @export
#' @rdname graph-is
is_tug <- function(object){
  UseMethod("is_tug")
}

#' @export
is_tug.graphNEL <- function(object){
    z <- as(object, "matrix")
    if (!isugMAT_(z)) FALSE
    else length(ripMAT(z)) > 0
}

#' @export
is_tug.igraph <- is_tug.graphNEL

#' @export
is_tug.default <- function(object){
    .check.is.matrix(object)
    if (isugMAT_(object)) length(mcsMAT(object)) > 0
    else FALSE
}

#' @export
#' @rdname graph-is
is_tugMAT <- function(object){
    isugMAT_(object) && length(mcsMAT(object))>0
}

## ######################################

#' @export
#' @rdname graph-is
is_dg <- function(object){
    UseMethod("is_dg")
}

#' @export
is_dg.graphNEL <- function(object){
    is_dgMAT(as(object, "matrix"))
}

#' @export
is_dg.igraph <- is_dg.graphNEL

#' @export
is_dg.default <- function(object){
    .check.is.matrix(object)
    eps <- 1e-4
    if (isadjMAT_(object))
        max(abs(sum(object * t(object)))) <= eps 
    else FALSE
}

#' @export
#' @rdname graph-is
is_dgMAT <- function(object){
    if (!is.adjMAT(object)) stop("Matrix is not adjacency matrix...\n")
    eps <- 1e-4
    max(abs(sum(object * t(object)))) <= eps
}







