#' @title Downstream aliases
#'
#' @description Downstream aliases for other graphical modelling
#'     packages.  Will be deprecated in due course. 
#' 
#' @aliases tabAdd__ tabDiv__ tabDiv0__ tabMarg__ tabMult__ tabSubt__
#'     topoSort topoSort.default topoSortMAT subsetof removeRedundant
#'     mcsmarked jTree jTree.default getCliques maxCliqueMAT combnPrim
#'     mcsmarkedMAT nextCell ell ellK isin
#'     is.TUG is.DAG
#'     graphNEL2adjMAT
#'     glist2adjMAT
#'     is_subsetof_ get_superset_ get_subset_
#'     is.UG is.DG
#'     graphNEL2M M2graphNEL
#'     tab
#' 
#' @name downstream-aliases
#' 
NULL

## NOTE to self: is_subsetof_ get_superset_ get_subset_ are pure cpp
## functions; perhaps let them live as an api thing



## FIXME I c-koden er der defineret tabMarg__, tabDiv0__, tabMult__ og
## FIXME der står at dette er af hensyn til gRain. Skal ryddes op.

## --- Used by gRain ---
## ---------------------

## for gRain compatibility: FIXME REMOVE LATER
#tabAdd__  <- tab_add_
##tabDiv__  <- tab_div_
##tabDiv0__ <- tab_div0_
##tabMarg__ <- tab_marg_
##tabMult__ <- tab_mult_
##tabSubt__ <- tab_subt_

## grain uses topoSort;
## FIXME: replace with topo_sort
## FIXME: No, gRain does NOT use topoSort.
## topoSort <- function(object, index=FALSE){
##   UseMethod("topoSort")
## }

## topoSort.default <- function(object, index=FALSE){
##     topo_sortMAT(as(object, "dgCMatrix"), index=index)
## }

## grain uses isin;
## FIXME: gRain replace isin with is_inset (remember that arguments
## must be switched)
## FIXME: done
#' @export
isin <- .isin ## potentialList.R

## grain uses subsetof; don't remember details of this function
## (different from is.subsetof) 

#' @export
subsetof <- function(x, y){
  #all(.Internal(match( x, y, 0, NULL))>0)
  all(match(x,y,0)>0)
}

#' @export
tab <- tabNew

## grain uses these;
## FIXME: in gRain replace is.TUG is.DAG
## FIXME DONE

#' @export
is.TUG <- is_tug

#' @export
is.DAG <- is_dag

## FIXME Used in simPATHy; request replace
#' @export
is.DG  <- is_dg
#' @export
is.UG  <- is_ug

## --- Used by gRim ---
## --------------------

## FIXME Used by gRim
#' @export
glist2adjMAT <- ugl2M_

#' @export
removeRedundant  <- remove_redundant
## FIXME: Replace

#' @export
mcsmarked     <- mcs_marked

#' @export
getCliques    <- get_cliques

#' @export
maxCliqueMAT  <- max_cliqueMAT

#' @export
combnPrim     <- combn_prim

#' @export
mcsmarkedMAT  <- mcs_markedMAT

#' @export
nextCell      <- next_cell

#' @export
ell <- function(Sigma, S, n){

    shdet <- function(Sigma){
        prod(eigen(Sigma)[[1]])
    }
    p <- dim(S)[1]
    const <- -n * p/2 * log(2 * pi)
    const - n/2 * log(shdet(Sigma)) - n/2 * sum(diag( solve(Sigma) %*% S )) 
}

#' @export
ellK <- function (K, S, n)
{
    value <- (n/2) * (log(det(K)) - sum(rowSums(K * S)))
    value
}


## --- Used by mcmcabn ---
## -----------------------

## FIXME: Request replacement

#' @export
topoSortMAT <- topo_sortMAT


## --- Used by rags2ridges ---
## ---------------------------
## FIXME: Request replacement

#' @export
jTree <- function(object, ...){
  UseMethod("jTree")
}

#' @export
jTree.default  <- junction_tree.default



## --- Used by HydeNet ---
## -----------------------
## FIXME request replacement
##graphNEL2adjMAT <- graphNEL2M
#' @export
graphNEL2adjMAT <- gn2xm_




## --- Used by simPATHy ---
## --------------------

#' @export
is.DG  <- is_dg

#' @export
is.UG  <- is_ug

#' @export
graphNEL2M <- gn2xm_

#' @export
M2graphNEL <- xm2gn_


