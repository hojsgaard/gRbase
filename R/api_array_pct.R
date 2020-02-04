########################################################
#' @title Array algebra
#' @description Addition, subtraction etc. of arrays
#' @name api-pct-operations
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
########################################################
#' 
#' @param a,a1,a2,... Arrays (with named dimnames)
## #' @param lst List of arrays.
#' @param tab1,tab2 Multidimensional arrays with named dimnames
#'     (we call them 'named arrays').
#' @param perm A vector of indices or dimnames or a right hand sided
#'     formula giving the desired permutiation.
#' @param marg A vector of indices or dimnames or a right hand sided
#'     formula giving the desired marginal.
## #' @param eps Criterion for checking equality of two arrays.
#' @param extra List defining the extra dimensions.
## #' @param aux Either a list with names and dimnames or a named array
## #'     from which such a list can be extracted.
#' @aliases %a+% %a-% %a*% %a/% %a/0%
#'
#' @examples
#' hec <- HairEyeColor
#' a1 <- tabMarg(hec, c("Hair", "Eye"))
#' a2 <- tabMarg(hec, c("Hair", "Sex"))
#' a3 <- tabMarg(hec, c("Eye", "Sex"))
#'
#' ## Binary operations
#' a1 %a+% a2
#' a1 %a-% a2
#' a1 %a*% a2
#' a1 %a/% a2

#' @export
#' @rdname api-pct-operations
"%a+%" <- function(a1, a2){tabAdd(a1,a2)}

#' @export
#' @rdname api-pct-operations
"%a-%" <- function(a1, a2){tabSubt(a1,a2)}

#' @export
#' @rdname api-pct-operations
"%a*%" <- function(a1, a2){tabMult(a1,a2)}

#' @export
#' @rdname api-pct-operations
"%a/%" <- function(a1, a2){tabDiv(a1,a2)}

#' @export
#' @rdname api-pct-operations
"%a/0%" <- function(a1, a2){tabDiv0(a1,a2)}


#' @export
#' @rdname api-pct-operations
"%ap%" <- function(tab1, perm){tabPerm(tab1, perm)}

#' @export
#' @rdname api-pct-operations
"%a_%" <- function(tab1, marg){tabMarg(tab1, marg)}

#' @export
#' @rdname api-pct-operations
"%a==%" <- function(tab1, tab2){tabEqual(tab1, tab2)}

#' @export
#' @rdname api-pct-operations
"%a^%" <- function(tab1, extra){tabExpand(tab1, extra)}

#' @export
#' @rdname api-pct-operations                   
"%aa%" <- function(tab1, tab2){tabAlign(tab1, tab2)}

