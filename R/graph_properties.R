#####################################################################
#'
#' @title Properties of a generating class (for defining a graph).
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
#####################################################################
## FIXME: Remove these aliases in due time
#' @aliases isGraphical.default isDecomposable.default
#' 
#' @details A set of sets of variables, say A_1, A_2, ... A_K is
#'     called a generating class for a graph with vertices V and edges
#'     E. If two variables a,b are in the same generator, say A_j,
#'     then a and b are vertices in the graph and there is an
#'     undirected edge between a and b.
#'
#'     The graph induced by \code{g1 = ~a:b + a:c + b:c + c:d} has
#'     edges \code{ab, ac, bc, cd}. The
#'     cliques of this graph are \code{abc, cd}. Hence there is not a
#'     1-1-correspondance between the graph and the generators.
#' 
#'     On the other hand, \code{g2 <- ~a:b:c + c:d} induces the same
#'     graph in this case there is a 1-1-correspondance.
#' 
#'     The graph induced by \code{g3 <- ~a:b + b:c + c:d + d:a} is in
#'     1-1-correspondance with its dependence graph, but the graph is
#'     not chordal.
#' 
#' @param x A generating class given as right hand sided formula or a
#'     list; see `examples` below.
#' 
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

## FIXME export of isGraphical.default needed for gRain; remove when
## no longer imported explicitly in gRain

#' @method isGraphical default
#' @export isGraphical.default
#' @export
isGraphical.default <- function( x ){
    if (!(inherits(x, c("formula", "list"))))
        stop("'x' must be a formula or a list of atomic vectors\n")

    if (inherits(x, "formula")){
        if (length(x) == 3)
            stop("'x' is not right hand sided formula\n")
        x <- formula(delete.response(terms(x)))
        x <- rhsf2list(x)
    } else {
        if (!is_list_of_atomic(x))
            stop("'x' must be a list of atomic vectors")
        if (length(x) == 0)
            stop("'x' must have positive length")
    }
    .isGraphical_glist( x )
}

.isGraphical_glist <- function(x){
    amat <- g_ugl2M_(x, vn=unique(unlist(x)))
    cliq <- max_cliqueMAT(amat)[[1]]
    ## all(unlist(lapply(cliq, function(cq) .isin(x, cq))))
    all(unlist(lapply(cliq, function(cq) is_inset(cq, x))))
}


#' @export
#' @rdname graph-gcproperties
isDecomposable <- function(x){
    UseMethod("isDecomposable")
}

## FIXME export of isDecomposable.default needed for gRain; remove when
## no longer imported explicitly in gRain

#' @method isDecomposable default
#' @export isDecomposable.default
#' @export
isDecomposable.default <- function( x ) {
    if (!(inherits(x, c("formula", "list"))))
        stop("'x' must be a formula or a list of atomic vectors\n")

    if (inherits(x, "formula")){
        if (length(x) == 3)
            stop("'x' is not right hand sided formula\n")
        x <- formula(delete.response(terms(x)))
        x <- rhsf2list(x)
    } else {
        if (!is_list_of_atomic(x))
            stop("'x' must be a list of atomic vectors")
        if (length(x) == 0)
            stop("'x' must have positive length")
    }
    .isDecomposable_glist( x )
}



.isDecomposable_glist <- function(x) {
    amat <- g_ugl2M_(x, vn=unique(unlist(x)))
    cliq <- max_cliqueMAT(amat)[[1]]
    ## isg <- all(unlist(lapply(cliq, function(cq) .isin(x, cq))))
    isg <- all(unlist(lapply(cliq, function(cq) is_inset(cq, x))))
    if (isg) length(mcsMAT(amat)) > 0
    else FALSE
}

## Is model defined by <glist> graphical and strongly decomposable
## if discrete=NULL, then the check is just if the graph is decomposable
## Issues: Fails on the "empty graph".

#' @export
isGSD_glist <- function(glist, vn=unique(unlist(glist)), discrete=NULL) {
  ##amat <- ugList2M(glist, vn=vn)
    amat <- g_ugl2M_(glist, vn=vn)
    cliq <- max_cliqueMAT(amat)[[1]]
    ## isg  <- all(unlist(lapply(cliq, function(sss) .isin(glist, sss))))
    isg  <- all(unlist(lapply(cliq, function(ss) is_inset(ss, glist))))
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
                             amat=g_ugl2M_(glist, vn=vn),
                             cliq=max_cliqueMAT(amat)[[1]], discrete=NULL) {

    ## isg <- all(unlist(lapply(cliq, function(ss) .isin(glist, ss))))
    isg <- all(unlist(lapply(cliq, function(ss) is_inset(ss, glist))))
    if (!isg){
        return(c(isg=FALSE, issd=FALSE))
    } else {
        return(c(isg=TRUE, issd=length(mcs_markedMAT(amat, discrete=discrete)) > 0))
    }
}

