## ################################################
##
#' @title Array algebra
#'
#' @description Addition, subtraction etc. of arrays
#'
#' @name array-algebra
#' 
## ###############################################
#' 
#' @param a,a1,a2,... Arrays (with named dimnames)

#' @param lst List of arrays.
#' @aliases %a+% %a-% %a*% %a/% %a/0%
#' 
## #' tab_add_ tab_div0_ tab_div_ tab_mult_ tab_subt_ tab_op_
#' 
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#'
#' @examples
#' hec <- HairEyeColor
#' a1 <- ar_marg(hec, c("Hair", "Eye"))
#' a2 <- ar_marg(hec, c("Hair", "Sex"))
#' a3 <- ar_marg(hec, c("Eye", "Sex"))
#'
#' ## Binary operations
#' a1 %a+% a2
#' a1 %a-% a2
#' a1 %a*% a2
#' a1 %a/% a2
#' 
#' ar_sum(a1, a2, a3)
#' ar_prod(a1, a2, a3)
#' 

#' @rdname array-algebra
ar_sum <- tabSum
#' @rdname array-algebra
ar_prod <- tabProd

#' @rdname array-algebra
ar_prod_list <- tabListMult
#' @rdname array-algebra
ar_sum_list <- tabListAdd

#' @rdname array-algebra
"%a+%" <- function(a1, a2){tabAdd(a1,a2)}
#' @rdname array-algebra
"%a-%" <- function(a1, a2){tabSubt(a1,a2)}
#' @rdname array-algebra
"%a*%" <- function(a1, a2){tabMult(a1,a2)}
#' @rdname array-algebra
"%a/%" <- function(a1, a2){tabDiv(a1,a2)}
#' @rdname array-algebra
"%a/0%" <- function(a1, a2){tabDiv0(a1,a2)}

#' @rdname array-algebra
ar_add <- function(a1, a2){ tabAdd(a1, a2) }
#' @rdname array-algebra
ar_subt <- function(a1, a2){ tabSubt(a1, a2) }
#' @rdname array-algebra
ar_mult <- function(a1, a2){ tabMult(a1, a2) }
#' @rdname array-algebra
ar_div <- function(a1, a2){ tabDiv(a1, a2) }
#' @rdname array-algebra
ar_div0 <- function(a1, a2){ tabDiv0(a1, a2) }






