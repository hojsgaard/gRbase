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
tabAdd__  <- tab_add_
tabDiv__  <- tab_div_
tabDiv0__ <- tab_div0_
tabMarg__ <- tab_marg_
tabMult__ <- tab_mult_
tabSubt__ <- tab_subt_

## grain uses topoSort; replace with topo_sort
topoSort <- function(object, index=FALSE){
  UseMethod("topoSort")
}

topoSort.default <- function(object, index=FALSE){
    topo_sortMAT(as_(object, "dgCMatrix"), index=index)
}

## grain uses isin; replace with is_inset (remember that arguments
## must be switched)
isin <- .isin

## grain uses subsetof; replace with is_subsetof
subsetof <- function(x, set){
  all(match(x, set) > 0)
}




