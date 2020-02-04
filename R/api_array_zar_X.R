########################################################
#' @title Array algebra
#' @description Addition, subtraction etc. of arrays
#' @name api_ar_operations
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
########################################################
#' 
#' @param a,a1,a2,... Arrays (with named dimnames)
#' @param lst List of arrays.
#' @param tab,tab1,tab2 Multidimensional arrays with named dimnames
#'     (we call them 'named arrays').
#' @param perm A vector of indices or dimnames or a right hand sided
#'     formula giving the desired permutiation.
#' @param marg A vector of indices or dimnames or a right hand sided
#'     formula giving the desired marginal.
#' @param eps Criterion for checking equality of two arrays.
#' @param extra List defining the extra dimensions.
#' @param aux Either a list with names and dimnames or a named array
#'     from which such a list can be extracted.
#' @aliases %a+% %a-% %a*% %a/% %a/0%
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

#' @export
#' @rdname api_ar_operations
ar_sum <- tabSum

#' @export
#' @rdname api_ar_operations
ar_prod <- tabProd

#' @export
#' @rdname api_ar_operations
ar_prod_list <- tabListMult

#' @export
#' @rdname api_ar_operations
ar_sum_list <- tabListAdd

#' @export
#' @rdname api_ar_operations
"%a+%" <- function(a1, a2){tabAdd(a1,a2)}

#' @export
#' @rdname api_ar_operations
"%a-%" <- function(a1, a2){tabSubt(a1,a2)}

#' @export
#' @rdname api_ar_operations
"%a*%" <- function(a1, a2){tabMult(a1,a2)}

#' @export
#' @rdname api_ar_operations
"%a/%" <- function(a1, a2){tabDiv(a1,a2)}

#' @export
#' @rdname api_ar_operations
"%a/0%" <- function(a1, a2){tabDiv0(a1,a2)}

#' @export
#' @rdname api_ar_operations
ar_add <- function(a1, a2){ tabAdd(a1, a2) }

#' @export
#' @rdname api_ar_operations
ar_subt <- function(a1, a2){ tabSubt(a1, a2) }

#' @export
#' @rdname api_ar_operations
ar_mult <- function(a1, a2){ tabMult(a1, a2) }

#' @export
#' @rdname api_ar_operations
ar_div <- function(a1, a2){ tabDiv(a1, a2) }

#' @export
#' @rdname api_ar_operations
ar_div0 <- function(a1, a2){ tabDiv0(a1, a2) }


#' @export
#' @rdname api_ar_operations
ar_expand <- tabExpand

#' @export
#' @rdname api_ar_operations
"%a^%" <- function(tab1, extra){tabExpand(tab1, extra)}

#' @export
#' @rdname api_ar_operations
ar_align <- tabAlign

#' @export
#' @rdname api_ar_operations
"%aa%" <- function(tab1, tab2){tabAlign(tab1, tab2)}

#' @export
#' @rdname api_ar_operations
ar_perm <- tabPerm

#' @export
#' @rdname api_ar_operations
"%ap%" <- function(tab1, perm){tabPerm(tab1, perm)}

#' @export
#' @rdname api_ar_operations
ar_marg <- tabMarg

#' @export
#' @rdname api_ar_operations
"%a_%" <- function(tab1, marg){tabMarg(tab1, marg)}

#' @export
#' @rdname api_ar_operations
ar_equal <- tabEqual

#' @export
#' @rdname api_ar_operations
"%a==%" <- function(tab1, tab2){tabEqual(tab1, tab2)}


##################################################################
#' @title Create multidimensional arrays
#' @description Alternative ways of creating arrays
#' @name api_ar_new
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
##################################################################
#' @inheritParams tabNew
#' @export
#' @rdname api_ar_new
ar_new <- tabNew


########################################################################
#' @title Array slices
#' @description Functions for extracting slices of arrays
#' @name api_ar_slice
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
########################################################################
#'
#' @seealso \code{\link{ar_perm}}, \code{\link{ar_marg}},
#'     \code{\link{ar_mult}}, \code{\link{ar_div}},
#'     \code{\link{ar_add}}, \code{\link{ar_subt}},
#'     \code{\link{ar_sum}}, \code{\link{ar_prod}}
#'
#' @inheritParams tabSlice
#' 
#' @examples
#' x = HairEyeColor
#' s = list(Hair=c("Black", "Brown"), Eye=c("Brown", "Blue"))
#'
#' ## ar_slice
#' s1 = ar_slice(x, slice=s); s1
#'
#' ## ar_slice_entries
#' ar_slice_entries(x, slice=s)
#' ar_slice_entries(x, slice=s, complement=TRUE)
#'
#' ## ar_slice_mult
#' s2 = ar_slice_mult(x, slice=s); s2
#'
#' # ar_slice_prim does the same as ar_slice - faster, but the
#' # function is less flexible
#' sp = list(c(1,2), c(1,2), TRUE)
#' ar_slice_prim(x, slice=sp)
#' ar_slice(x, slice=s)
NULL

#' @export
#' @rdname api_ar_slice
ar_slice <- tabSlice

#' @export
#' @rdname api_ar_slice
ar_slice_prim <- tabSlicePrim

#' @export
#' @rdname api_ar_slice
ar_slice_mult <- tabSliceMult

#' @export
#' @rdname api_ar_slice
ar_slice_entries <- tabSlice2Entries


#####################################################################
#' @title Marginalize and condition in multidimensional array.
#' @description Marginalize and condition in a multidimensional array
#'     which is assumed to represent a discrete multivariate
#'     distribution.
#' @name api_ar_dist
#####################################################################
#'
#' @inheritParams tabDist
#' @examples
#' ar_dist(HairEyeColor)
#' @export
ar_dist <- tabDist

#####################################################################
#' @title Normalize an array
#' @description Normalize an array in various ways.
#' @name api_ar_normalize
#####################################################################
#'
#' @inheritParams tabNormalize
#' @return An array
#' @examples
#' ar_normalize(HairEyeColor, type="first")
#' ar_normalize(HairEyeColor, type="all")

#' @export
#' @rdname api_ar_normalize
ar_normalize <- tabNormalize

#' @export
#' @rdname api_ar_normalize
ar_norm <- tabNormalize




## ####################################################################
## #' @title Operations on multidimensional arrays.
## #' @description Operations like marginalize, permute, slicing etc on
## #'     arrays A multidimensional table (an array) is here a vector
## #'     with a dim and a dimnames attribute.
## #' @name api_ar_operations
## #' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
## ####################################################################
## #' 
## #' @param tab,tab1,tab2 Multidimensional arrays.
## #' @param perm A vector of indices or dimnames or a right hand sided
## #'     formula giving the desired permutiation.
## #' @param marg A vector of indices or dimnames or a right hand sided
## #'     formula giving the desired marginal.
## #' @param eps Criterion for checking equality of two arrays.
## #' @param extra List defining the extra dimensions.
## #' @param aux Either a list with names and dimnames or a named array
## #'     from which such a list can be extracted.
## #' 
## #' @return Most functions return a multidimensional array.
## #' 
## #' @seealso \code{\link{aperm}}, \code{\link{ar_perm}},
## #'     \code{\link{ar_slice}}, \code{\link{ar_slice_entries}}
## #' @keywords utilities
## #' @examples
## #' 
## #' ar1 <- array(1:8, dim=c(2,2,2), dimnames=list("a"=1:2,"b"=1:2,"c"=1:2))
## #' ar2 <- array(1:8, dim=c(2,2,2), dimnames=list("b"=1:2,"c"=1:2,"d"=1:2))
## #' 
## #' ## ## armarg ##
## #' ## Marginalize down to the bc-array
## #' ar_marg(ar1, 2:3)
## #' ar_marg(ar1, c("b","c"))
## #' ar_marg(ar1, ~b + c)
## #'
## #' ## Marginalize over 'everything'
## #' ar_marg(ar1, NULL)
## #' ar_marg(ar1, ~1)
## #' ar_marg(ar1, character(0))
## #' ar_marg(ar1, integer(0))
## #' 
## #' ## This gives an error (as expected)
## #' ## ar_marg(ar1, c(2, 5))
## #' ## ar_marg(ar1, c("b","w"))
## #' ## ar_marg(ar1, ~b + w)
## #'  
## #' ## ## ar_perm ##
## #' ar_perm(ar1, 1:3)      ## No change - an abc-table
## #' ar_perm(ar1, c(2,3,1)) ## A bca-table
## #' ar_perm(ar1, ~b + c + a)
## #' 
## #' ## This gives error
## #' ## ar_perm(ar1, c(2,1))
## #' ## ar_perm(ar1, c(2,1,5))
## #' ## ar_perm(ar1, c(2,1,NA))
## #' 
## #' ## ## ar_mult etc ##
## #' ## Multiply two arrays
## #' out <- ar_mult(ar1, ar2)
## #' out <- ar_perm(out, ~a + b + c + d) ## Just for comparison below
## #' ftable(out)
## #'
## #' ## Alternative approch
## #' df1 <- as.data.frame.table(ar1)
## #' df2 <- as.data.frame.table(ar2)
## #' df3 <- merge(df1, df2, by=c("b","c"))
## #' df3 <- transform(df3, Freq=Freq.x*Freq.y)
## #' ar3 <- xtabs(Freq ~ a + b + c + d, data=df3)
## #' ftable(ar3)
## #' 
## #' ## ## ar_expand ##
## #' ar1.e <- ar_expand(ar1, ar2)
## #' ## ar1.e has dimnames b,c,d,a; values are simply replicated for each
## #' ## level of d.
## #' dimnames(ar1.e)
## #' ftable(ar1.e, row.vars="d")
## #' ## ar_expand:
## #' ar_expand(ar1, list(u=1:2))
## #' ar1 %a^% list(u=1:2)
## #' 
## #' ## ## aralign ##
## #' ar2.e <- ar_expand(ar2, ar1)
## #' names(dimnames(ar2.e))
## #' names(dimnames(ar1.e))
## #' out <- ar_align(ar1.e, ar2.e)
## #' names(dimnames(out)) ## Same as ar2.e
## #' 
## #' @examples
## #' 
## NULL
