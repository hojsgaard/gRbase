#' @title Downstream aliases
#'
#' @description Downstream aliases for other graphical modelling
#'     packages.  May change without further notice.
#' 
#' @aliases tabAdd__ tabDiv__ tabDiv0__ tabMarg__ tabMult__ tabSubt__
#'     topoSort topoSort.default topoSortMAT subsetof removeRedundant mcsmarked
#'     jTree jTree.default
#'     getCliques maxCliqueMAT combnPrim mcsmarkedMAT nextCell ell ellK isin
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

## Used by mcmcabn
topoSortMAT <- topo_sortMAT

## grain uses isin; replace with is_inset (remember that arguments
## must be switched)


## Used by rags2ridges
jTree <- function(object, ...){
  UseMethod("jTree")
}

jTree.default <-  function(object, nLevels = NULL, ...){
    cls <- match.arg(class( object ),
                     c("graphNEL","igraph","matrix","dgCMatrix"))

    switch(cls,
           "graphNEL" ={junction_treeMAT(gn2sm_(object), nLevels=nLevels, ... )},
           "igraph"   ={junction_treeMAT(ig2sm_(object), nLevels=nLevels, ... )},
           "dgCMatrix"=,
           "matrix"   ={junction_treeMAT(object, nLevels=nLevels, ...)})
}




isin <- .isin

## grain uses subsetof; don't remember details of this function
## (different from is.subsetof)

subsetof <- function(x, y){
  #all(.Internal(match( x, y, 0, NULL))>0)
  all(match(x,y,0)>0)
}

## ## Used in CRAN version of gRain
## unlistPrim

## CRAN version of gRim

removeRedundant  <- remove_redundant
## uses glist2adjMAT; see graph-coerce-list.R


mcsmarked  <- mcs_marked


getCliques <- get_cliques


maxCliqueMAT  <- max_cliqueMAT


combnPrim <- combn_prim


mcsmarkedMAT  <- mcs_markedMAT


nextCell <- next_cell


ell <- function(Sigma, S, n){

    shdet <- function(Sigma){
        prod(eigen(Sigma)[[1]])
    }
    p <- dim(S)[1]
    const <- -n * p/2 * log(2 * pi)
    const - n/2 * log(shdet(Sigma)) - n/2 * sum(diag( solve(Sigma) %*% S )) 
}


ellK <- function (K, S, n)
{
    value <- (n/2) * (log(det(K)) - sum(rowSums(K * S)))
    value
}
