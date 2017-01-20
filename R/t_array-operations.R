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
#' @details \code{perm} in \code{arperm()} can be a vector of indices
#'     (as in Rs own \code{aperm()}) but also a vector of
#'     dimnames. Currently there is no checking that the dimnames are
#'     actually in the array, so please take care.
#' 
#' @aliases tabAdd__ tabSubt__ tabMult__ tabDiv__ tabDiv0__ tabOp__ aperm__
#'     tabMarg__ tabEqual__ tabMarg__ tabPerm__ tabAlign__ tabExpand__
#'     tabListAdd__ tabListMult__ tabListAdd tabListMult arExt tabExt
#'     tabEqual arEqual tabMarg tabCondProb tabAlign aralign tabPerm arPerm
#'     tabSlice tabSlice2 arSlice tabSlicePrim arSlicePrim tabSliceMult
#'     arSliceMult tabExpand arexpand tabSlice2Entries tabSlice2Entries_
#'
#' @param tab,tab1,tab2 Multidimensional arrays.
#' @param perm A vector of indices or dimnames giving the desired permutiation.
#' @param marg Specification of marginal; either a character vector, a numeric
#'     vector or a right hand sided formula For \code{arperm} and \code{ararg}
#'     it can also be a right hand sided formula.
#' @param eps Criterion for checking equality of two arrays.
#' @return Most functions here return a multidimensional array.
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @seealso \code{\link{aperm}}, \code{\link{arslice}},
#'     \code{\link{arslice_entries}}
#' @keywords utilities
#' @examples
#' 
#' ar1 <- array(1:8, dim=c(2,2,2), dimnames=list("a"=1:2,"b"=1:2,"c"=1:2))
#' ar2 <- array(1:8, dim=c(2,2,2), dimnames=list("b"=1:2,"c"=1:2,"d"=1:2))
#' 
#' ## ## armarg ##
#' ## Marginalize down to the bc-array
#' armarg(ar1, 2:3)
#' armarg(ar1, c("b","c"))
#' armarg(ar1, ~b + c)
#' 
#' ## This gives an error
#' ## armarg(ar1, c(2,5))
#' ## armarg(ar1, c("b","w"))
#' ## armarg(ar1, ~b + w)
#'  
#' ## ## arperm ##
#' arperm(ar1, 1:3)      ## No change - an abc-table
#' arperm(ar1, c(2,3,1)) ## A bca-table
#' arperm(ar1, ~b + c + a)
#' 
#' ## This gives error
#' ## arperm(ar1, c(2,1))
#' ## arperm(ar1, c(2,1,5))
#' ## arperm(ar1, c(2,1,NA))
#' 
#' ## ## armult etc ##
#' ## Multiply two arrays
#' out <- armult(ar1, ar2)
#' out <- arperm(out, ~a+b+c+d) ## Just for comparison below
#' ftable(out)
#' ## Alternative approch
#' df1 <- as.data.frame.table(ar1)
#' df2 <- as.data.frame.table(ar2)
#' df3 <- merge(df1, df2, by=c("b","c"))
#' df3 <- transform(df3, Freq=Freq.x*Freq.y)
#' ar3 <- xtabs(Freq ~ a + b + c + d, data=df3)
#' ftable(ar3)
#' 
#' ## ## arexpand ##
#' ar1.e <- arexpand(ar1, ar2)
#' ## ar1.e has dimnames b,c,d,a; values are simply replicated for each
#' ## level of d.
#' dimnames(ar1.e)
#' ftable(ar1.e, row.vars="d")
#' 
#' ## ## aralign ##
#' ar2.e <- arexpand(ar2, ar1)
#' names(dimnames(ar2.e))
#' names(dimnames(ar1.e))
#' out <- aralign(ar1.e, ar2.e)
#' names(dimnames(out)) ## Same as ar2.e
#' 
#' @examples
#' 
NULL

###
### FUNCTION RENAMING (from c++ to R)
###

tabMult 	<- tabMult__
tabDiv    <- tabDiv__
tabDiv0   <- tabDiv0__
tabAdd    <- tabAdd__
tabSubt   <- tabSubt__

tabEqual  <- tabEqual__
tabAlign  <- tabAlign__
tabExpand <- tabExpand__  ## Rethink this
tabListMult <- tabListMult__
tabListAdd  <- tabListAdd__

tabPerm <- function(tab, perm){
    if (!is.array(tab))
        stop("'tab' is not an array")
    if ( !(is.numeric(perm) || is.character(perm) || class(perm)=="formula"))
        stop("'perm' must be character or numeric vector or right hand sided formula")
    
    if ( is.numeric( perm ) ){
        aperm__(tab, perm) ## Call C-code here
    } else {
        if ( class(perm) == "formula" ){
            perm <- all.vars( perm[[2]] )
        }
        vn <- names(dimnames( tab ))
        p <- pmatch( perm, vn )
        perm <- vn[p]
        aperm__(tab, perm)  ## Call C-code here
    }
}

## #' @rdname array-operations
## arPerm <- tabPerm

#' @rdname array-operations
arperm <- tabPerm

tabMarg <- function(tab, marg){
    if (!is.array(tab))
        stop("'tab' is not an array")
    if (is.numeric(marg) || is.character(marg)){
        tabMarg__(tab, marg) ## Call C-code here
    } else {
        if (class(marg)== "formula"){
            marg <- all.vars(marg[[2]])
            tabMarg__(tab, marg) ## Call C-code here
        } else {
            stop("'marg' must be character or numeric vector or a right hand sided formula")
        }
        
    }
}

## #' @rdname array-operations
## arMarg <- tabMarg

#' @rdname array-operations
armarg <- tabMarg

#' @rdname array-operations
arequal <- tabEqual

#' @rdname array-operations
"%a==%" <- function(tab1, tab2){tabEqual(tab1,tab2)}

#' @rdname array-operations
"%a_%" <- function(tab1, marg){tabMarg(tab1,marg)}


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


#' @rdname array-operations
#' @examples
#' ## arexpand:
#' ar1 <- array(1:8, dim=c(2,2,2), dimnames=list("a"=1:2,"b"=1:2,"c"=1:2))
#' ar2 <- array(1:8, dim=c(2,2,2), dimnames=list("b"=1:2,"c"=1:2,"d"=1:2))
#'
#' arexpand(ar1, ar2) %>% ftable(row.vars=1) ## Same as
#' ## arexpand(ar1, dimnames(ar2)) %>% ftable(row.vars=1) 
arexpand <- tabExpand
 
#' @rdname array-operations
aralign <- tabAlign








