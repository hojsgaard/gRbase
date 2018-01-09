## ####################################################################
##
#' @title Operations on multidimensional arrays.
#'
#' @description Operations like marginalize, permute, slicing etc on
#'     arrays A multidimensional table (an array) is here a vector
#'     with a dim and a dimnames attribute.
#'
#' @name array-operations
#'
## ####################################################################
#' 
#' @param tab,tab1,tab2 Multidimensional arrays.
#' 
#' @param perm A vector of indices or dimnames or a right hand sided
#'     formula giving the desired permutiation.
#' @param marg A vector of indices or dimnames or a right hand sided
#'     formula giving the desired marginal.
#' @param eps Criterion for checking equality of two arrays.
#' @param extra List defining the extra dimensions.
#' @param aux Either a list with names and dimnames or a named array
#'     from which such a list can be extracted.
#' 
#' @return Most functions return a multidimensional array.
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @seealso \code{\link{aperm}}, \code{\link{ar_perm}},
#'     \code{\link{ar_slice}}, \code{\link{ar_slice_entries}}
#' @keywords utilities
#' @examples
#' 
#' ar1 <- array(1:8, dim=c(2,2,2), dimnames=list("a"=1:2,"b"=1:2,"c"=1:2))
#' ar2 <- array(1:8, dim=c(2,2,2), dimnames=list("b"=1:2,"c"=1:2,"d"=1:2))
#' 
#' ## ## armarg ##
#' ## Marginalize down to the bc-array
#' ar_marg(ar1, 2:3)
#' ar_marg(ar1, c("b","c"))
#' ar_marg(ar1, ~b + c)
#'
#' ## Marginalize over 'everything'
#' ar_marg(ar1, NULL)
#' ar_marg(ar1, ~1)
#' ar_marg(ar1, character(0))
#' ar_marg(ar1, integer(0))
#' 
#' ## This gives an error (as expected)
#' ## ar_marg(ar1, c(2, 5))
#' ## ar_marg(ar1, c("b","w"))
#' ## ar_marg(ar1, ~b + w)
#'  
#' ## ## ar_perm ##
#' ar_perm(ar1, 1:3)      ## No change - an abc-table
#' ar_perm(ar1, c(2,3,1)) ## A bca-table
#' ar_perm(ar1, ~b + c + a)
#' 
#' ## This gives error
#' ## ar_perm(ar1, c(2,1))
#' ## ar_perm(ar1, c(2,1,5))
#' ## ar_perm(ar1, c(2,1,NA))
#' 
#' ## ## ar_mult etc ##
#' ## Multiply two arrays
#' out <- ar_mult(ar1, ar2)
#' out <- ar_perm(out, ~a + b + c + d) ## Just for comparison below
#' ftable(out)
#'
#' ## Alternative approch
#' df1 <- as.data.frame.table(ar1)
#' df2 <- as.data.frame.table(ar2)
#' df3 <- merge(df1, df2, by=c("b","c"))
#' df3 <- transform(df3, Freq=Freq.x*Freq.y)
#' ar3 <- xtabs(Freq ~ a + b + c + d, data=df3)
#' ftable(ar3)
#' 
#' ## ## ar_expand ##
#' ar1.e <- ar_expand(ar1, ar2)
#' ## ar1.e has dimnames b,c,d,a; values are simply replicated for each
#' ## level of d.
#' dimnames(ar1.e)
#' ftable(ar1.e, row.vars="d")
#' ## ar_expand:
#' ar_expand(ar1, list(u=1:2))
#' ar1 %a^% list(u=1:2)
#' 
#' ## ## aralign ##
#' ar2.e <- ar_expand(ar2, ar1)
#' names(dimnames(ar2.e))
#' names(dimnames(ar1.e))
#' out <- ar_align(ar1.e, ar2.e)
#' names(dimnames(out)) ## Same as ar2.e
#' 
#' @examples
#' 
NULL

#' @rdname array-operations
ar_expand <- tabExpand
#' @rdname array-operations
"%a^%" <- function(tab1, extra){tabExpand(tab1, extra)}

#' @rdname array-operations
ar_align <- tabAlign
#' @rdname array-operations
"%aa%" <- function(tab1, tab2){tabAlign(tab1, tab2)}

#' @rdname array-operations
ar_perm <- tabPerm
#' @rdname array-operations
"%ap%" <- function(tab1, perm){tabPerm(tab1, perm)}

#' @rdname array-operations
ar_marg <- tabMarg
#' @rdname array-operations
"%a_%" <- function(tab1, marg){tabMarg(tab1, marg)}

#' @rdname array-operations
ar_equal <- tabEqual
#' @rdname array-operations
"%a==%" <- function(tab1, tab2){tabEqual(tab1, tab2)}



















## #############################################################
##
## Extend and align arrays.
##
## #############################################################

## FIXME tabExt Need examples
## tabExt <- function(tab, ext){
##     if ( !is.named.array( tab ) )
##         stop("'tab' must be a named array\n")
##     if ( !(is.named.array(ext) || is.list(ext)) )
##         stop("'ext' is not valid\n")
## 
##     dn.tab <- dimnames( tab )
##     
##     if ( is.named.array( ext ) )
##         ext <- dimnames( ext )
## 
##     if (is.null( names( ext ) ) )
##         stop("need variable names in 'ext'\n")
## 
##     vn.tab <- names( dn.tab )
##     vn.ext <- names( ext )
## 
##     keep <- setdiff( vn.ext, vn.tab )
##     ext <- ext[ keep ]
## 
##     dn.new <- c( dn.tab, ext )
##     di.new <- unlist( lapply( dn.new, length ) )
##         
##     array(tab, dim=di.new, dimnames=dn.new)
## }
## 









## #' @rdname array-operations 
## .arexpand <- tabExpand

## #' @rdname array-operations
## .aralign <- tabAlign
## #' @rdname array-operations
## .arperm <- tabPerm



## #' @rdname array-operations
## arMarg <- tabMarg

## #' @rdname array-operations
## .armarg <- tabMarg

## #' @rdname array-operations
## .arequal <- tabEqual










