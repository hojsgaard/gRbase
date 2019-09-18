#' @title Downstream aliases
#'
#' @description Downstream aliases for other graphical modelling
#'     packages.  May change without further notice.
#' 
#' @aliases
#'     tabAdd__ tabDiv__ tabDiv0__ tabMarg__ tabMult__ tabSubt__
#'
#' @name downstream-aliases
#' 
NULL

## FIXME I c-koden er der defineret tabMarg__, tabDiv0__, tabMult__ og
## FIXME der står at dette er af hensyn til gRain. Skal ryddes op.

## for gRain compatibility FIXME REMOVE LATER

#' @rdname downstream-aliases
tabAdd__  <- tab_add_
#' @rdname downstream-aliases
tabDiv__  <- tab_div_
#' @rdname downstream-aliases
tabDiv0__ <- tab_div0_
#' @rdname downstream-aliases
tabMarg__ <- tab_marg_
#' @rdname downstream-aliases
tabMult__ <- tab_mult_
#' @rdname downstream-aliases
tabSubt__ <- tab_subt_
#' @rdname downstream-aliases

## grain uses topoSort; replace with topo_sort
#' @rdname downstream-aliases
topoSort <- function(object, index=FALSE){
  UseMethod("topoSort")
}
#' @rdname downstream-aliases
topoSort.default <- function(object, index=FALSE){
    topo_sortMAT(as_(object, "dgCMatrix"), index=index)
}

## grain uses isin; replace with is_inset (remember that arguments
## must be switched)
#' @rdname downstream-aliases
isin <- .isin

## grain uses subsetof; don't remember details of this function
## (different from is.subsetof)

#' @rdname downstream-aliases
subsetof <- function(x, y){
  #all(.Internal(match( x, y, 0, NULL))>0)
  all(match(x,y,0)>0)
}

## Used in CRAN version of gRain
#' @rdname downstream-aliases
unlistPrim <- function(x){
    unlist(x)
}

## CRAN version of gRim
#' @rdname downstream-aliases
removeRedundant  <- remove_redundant
## uses glist2adjMAT; see graph-coerce-list.R

#' @rdname downstream-aliases
mcsmarked  <- mcs_marked

#' @rdname downstream-aliases
getCliques <- get_cliques


#' @rdname downstream-aliases
maxCliqueMAT  <- max_cliqueMAT

#' @rdname downstream-aliases
combnPrim <- combn_prim

#' @rdname downstream-aliases
mcsmarkedMAT  <- mcs_markedMAT

#' @rdname downstream-aliases
nextCell <- next_cell

#' @rdname downstream-aliases
ell <- function(Sigma, S, n){

    shdet <- function(Sigma){
        prod(eigen(Sigma)[[1]])
    }
    p <- dim(S)[1]
    const <- -n * p/2 * log(2 * pi)
    const - n/2 * log(shdet(Sigma)) - n/2 * sum(diag( solve(Sigma) %*% S )) 
}

#' @rdname downstream-aliases
ellK <- function (K, S, n)
{
    value <- (n/2) * (log(det(K)) - sum(rowSums(K * S)))
    value
}
