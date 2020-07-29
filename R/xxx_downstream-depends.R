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
#' ell ellK
#' 
NULL

## graphNEL2M M2graphNEL graphNEL2adjMAT
## tabAdd__ tabDiv__ tabDiv0__ tabMarg__ tabMult__ tabSubt__
## #' removeRedundant 
## #' combnPrim
## #' mcsmarked mcsmarkedMAT 
## #' nextCell glist2adjMAT

## #' 
## #' @aliases
## #'
## #' is_subsetof_ get_superset_ get_subset_ subsetof
## #'
## #' tabMult__ tabDiv0__ tabMarg__


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




## ### Used by gRim ###

## #' @export
## glist2adjMAT <- g_ugl2M_


## ## 20/6/20: removeRedundant not used in gRim anymore
## #' @export
## removeRedundant  <- remove_redundant

## ## 20/6/20: mcsmarked not used in gRim anymore
## #' @export
## mcsmarked     <- mcs_marked

## ## 20/6/20: mcsmarkedMAT not used in gRim anymore
## #' @export
## mcsmarkedMAT  <- mcs_markedMAT

## #' @export
## combnPrim     <- combn_prim

## #' @export
## nextCell      <- next_cell

## #' @export
## isin <- .isin ## potentialList.R



## #' @export
## ## #' @param lst A list of arrays
## ar_prod_list  <- tabListMult

## #' @export
## ar_marg  <- tabMarg

## #' @export
## ar_slice  <- tabSlice

## #' @export
## ar_slice_mult <- tabSliceMult

## #' @export
## ar_new <- tabNew






## #' ar_marg ar_mult ar_slice ar_slice_mult ar_new ar_prod ar_perm
## #'     ar_prod_list
## #'
## #' isin tab


## #' @export
## ar_mult  <- tabMult



## #' @export
## ar_prod <- tabProd

## #' @export
## ar_perm <- tabPerm


## FIXME I c-koden er der defineret tabMarg__, tabDiv0__, tabMult__ og
## FIXME der står at dette er af hensyn til gRain. Skal ryddes op.

## #' @export
## tabMult__ <- tabMult__

## #' @export
## tabDiv0__ <- tabDiv0__

## #' @export
## tabMarg__ <- tabMarg__




## #' @export
## tab <- tabNew



## ## removeRedundant needed by gRc
## #' @export
## removeRedundant  <- remove_redundant



### Used by gRain ###

## #' @export
## is_subsetof_ <- is_subsetof_

## #' @export
## get_superset_  <- get_superset_

## #' @export
## get_subset_  <- get_subset_
