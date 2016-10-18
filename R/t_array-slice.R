
## ########################################################################
##
#' @title Array slices
#' 
#' @description Functions for extracting slices of arrays
#'
#' @name array-slice
##
## ########################################################################
#'
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#'
#' @seealso \code{\link{arperm}}, \code{\link{armarg}}, \code{\link{armult}},
#' \code{\link{ardiv}}, \code{\link{aradd}}, \code{\link{arsubt}}, \code{\link{arsum}},
#' \code{\link{arprod}}
#' 
NULL

tabSlice<- function(tab, slice=NULL, margin=names(slice), drop=TRUE, as.array=FALSE){

    if (!is.named.array(tab))
        stop("'tab' is not a named array")
    else if ( is.null( slice ) )
        tab
    else if (!( is.character( slice ) || is.numeric( slice ) || is.list( slice )))
        stop("'slice' is not valid \n")
    else if (is.null( margin ) || !( is.character( margin ) || is.numeric( margin )))
        stop("'margin' is not valid \n")
    else {
        margin.idx <-
            if ( is.character( margin ) )
                match( margin, names( dimnames( tab ) ) )
            else
                margin
        if ( any( is.na( margin.idx ) ) ) stop("invalid 'margin'")
        tabSlice2( tab, slice, margin.idx, drop=drop, as.array=as.array)
    }
}

tabSlice2 <- function(tab, slice, margin.idx, drop=TRUE, as.array=FALSE){

    z <- as.list(rep(TRUE,  length(dim(tab))))
    z[ margin.idx ] <- slice
    out <- do.call("[", c(list(tab), z, drop=drop))
    
    if (as.array && is.null( dim( out ) ) ){
        dn <- list(names(out))
        k  <- which(unlist(lapply(z, is.logical))) # idx of variables still in array
        names(dn) <- names( dimnames( tab ) )[ k ]
        array( out, dim=length(out), dimnames=dn )
    } else {
        out
    }
}

tabSlicePrim <- function(tab, slice, drop=TRUE){
    do.call("[", c(list(tab), slice, drop=drop))        
}

tabSliceMult <- function(tab, slice, val=1, comp=0){
    if ( !is.null(val) ){
        idx <- tabSlice2Entries(tab, slice)
        tab[idx] <- tab[idx] * val
    }
    if ( !is.null(comp) ){
        idx <- tabSlice2Entries(tab, slice, complement=TRUE)
        tab[idx] <- tab[idx] * comp
    }
    tab
}


#' @rdname array-slice
#'
#' @param tab An array with named dimnames.
#' @param slice A list defining the slice.
#' @param margin Names of variables in slice.
#' @param drop If TRUE then dimensions with only one level will be dropped
#'     from the output.
#' @param as.array If the resulting array is one-dimensional the
#'     result will by default be a vector with no dim attribute unless
#'     as.array is TRUE.
#' 
arslice <- tabSlice

#' @rdname array-slice
arslice_prim <- tabSlicePrim


#' @rdname array-slice
#' @param val The values that entries in the slice will be multiplied
#'     with.
#' @param comp The values that entries NOT in the slice will be
#'     multiplied with.

arslice_mult <- tabSliceMult

tabSlice2Entries <- function(tab, slice, complement=FALSE){
    tab[] <- 1:length(tab)
    out <- tabSlice(tab, slice,  margin=names(slice))
    if (complement)
        c(tab)[-c(out)]
    else
        c(out)
}

#' @rdname array-slice
#' @param complement If TRUE the complement of the entries are returned. 
#' 
#' @examples
#'
#' x = HairEyeColor
#' s = list(Hair=c("Black","Brown"), Eye=c("Brown", "Blue"))
#'
#' ## arslice
#' s1 = arslice(x, slice=s)
#' s1
#'
#' ## arslice_entries
#' arslice_entries(x, slice=s)
#' arslice_entries(x, slice=s, complement=TRUE)
#'
#' ## arslice_mult
#' s2 = arslice_mult(x, slice=s)
#' s2
#'
#' ## arslice_prim does the same as arslice - faster, but the function is less
#' # flexible
#' sp = list(c(1,2), c(1,2), TRUE)
#' arslice_prim(x, slice=sp)
#' arslice(x, slice=s)
#' if ( require(microbenchmark) ){
#'   microbenchmark(arslice_prim(x, slice=sp), arslice(x, slice=s))
#' }

arslice_entries <- tabSlice2Entries
