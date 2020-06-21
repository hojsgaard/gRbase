####################################################################
#'
#' @title Downstream aliases
#' @description Downstream aliases for other graphical modelling
#'     packages.  Will be deprecated in due course. 
#' @name downstream-aliases
#'
####################################################################
#'
#' @aliases
#'
#' removeRedundant 
#' combnPrim
#' mcsmarked mcsmarkedMAT 
#' nextCell ell ellK glist2adjMAT


## graphNEL2M M2graphNEL graphNEL2adjMAT
## tabAdd__ tabDiv__ tabDiv0__ tabMarg__ tabMult__ tabSubt__

## NOTE to self: is_subsetof_ get_superset_ get_subset_ are pure cpp
## functions; perhaps let them live as an api thing

## FIXME I c-koden er der defineret tabMarg__, tabDiv0__, tabMult__ og
## FIXME der står at dette er af hensyn til gRain. Skal ryddes op.


## ### Used by gRain ###

#' @rdname downstream-aliases
#' @aliases ar_marg ar_mult ar_slice ar_slice_mult ar_new ar_prod
#'     ar_perm ar_prod_list isin tab is_subsetof_ get_superset_
#'     get_subset_ subsetof
#' tabMult__ tabDiv0__ tabMarg__

#' @export
#' @param lst A list of arrays
ar_prod_list  <- tabListMult

#' @export
ar_marg  <- tabMarg

#' @export
ar_mult  <- tabMult

#' @export
ar_slice  <- tabSlice

#' @export
ar_slice_mult <- tabSliceMult

#' @export
ar_new <- tabNew

#' @export
ar_prod <- tabProd

#' @export
ar_perm <- tabPerm

#' @export
isin <- .isin ## potentialList.R

#' @export
tab <- tabNew

#' @export
is_subsetof_ <- is_subsetof_

#' @export
get_superset_  <- get_superset_

#' @export
get_subset_  <- get_subset_


## grain uses subsetof; don't remember details of this function
## (different from is.subsetof) 

#' @export
subsetof <- function(x, y){
  all(match(x, y, 0) > 0)
}


#' @export
tabMult__ <- tabMult__

#' @export
tabDiv0__ <- tabDiv0__

#' @export
tabMarg__ <- tabMarg__


## ### Used by gRim ###

#' @export
glist2adjMAT <- g_ugl2M_


## 20/6/20: removeRedundant not used in gRim anymore
#' @export
removeRedundant  <- remove_redundant

## 20/6/20: mcsmarked not used in gRim anymore
#' @export
mcsmarked     <- mcs_marked

## 20/6/20: mcsmarkedMAT not used in gRim anymore
#' @export
mcsmarkedMAT  <- mcs_markedMAT


#' @export
combnPrim     <- combn_prim

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
ellK <- function (K, S, n){
    value <- (n/2) * (log(det(K)) - sum(rowSums(K * S)))
    value
}










