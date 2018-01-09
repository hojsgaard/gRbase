## ###################################################################
##
## Miscellaneous array functions
##
## ###################################################################

## ###################################################################
#' @title Convert dataframe to contingency table
#'
#' @description: Much like xtabs but with more flexibility
#'
#' @name df2xtabs
## ###################################################################
#' 
#' @param indata A dataframe.
#' @param names Names of variables defining table; a character vector or a right
#'     hand sided formula.
#' @param normalize Either "none", "first" or "all". Should result be
#'     normalized, see 'Details' below.
#' @param smooth Should values be smoothed, see 'Details' below.
#' 
#' @examples
#' ## Extract arrays from dataframe (much like xtabs() but with more flexibility)
#' data(cad1) 
#' df2xtabs(cad1, ~Sex:AngPec:AMI)
#' df2xtabs(cad1, c("Sex", "AngPec", "AMI"))
#' df2xtabs(cad1, c(1, 2, 3))

df2xtabs <- function(indata, names=NULL, normalize="none", smooth=0){

    if ( !( is.data.frame(indata) ) )
        stop("'indata' must a dataframe\n")
        
    if (!is.null( names )) {
        if (is.numeric( names )){
            if (min(names) < 1 || max(names) > ncol(indata)){
                stop("columns out of range \n")
            }
        } else {
            if (class(names) %in% c("formula", "character")){
                names <- rhsf2list(names)[[1]]
            } else {
                stop("don't know what to do\n")
            }
        }
    } 
    
    if (is.null(names))
        out <- xtabs(~., data=indata)
    else
        out <- xtabs(~., data=indata[, names, drop=FALSE])

    ## FIXME : There is no check on what smooth is
    if (smooth > 0)
        out <- out + smooth

    if (normalize != "none")
        tabNormalize( out, normalize )
    else
        out
}

#' @title Normalize an array
#' 
#' @description Normalize an array in various ways.
#'
#' @name array-normalize
#' 
#' @param tab A multidimensional array
#' @param type Either "none", "first", or "all"
#' @return An array
#' @examples
#' ar_normalize( HairEyeColor, type="first")
#' ar_normalize( HairEyeColor, type="all")
#' 
tabNormalize <- function(tab, type="none"){
    switch(type,
           "first"={
               if (length(dim(tab))>1){
                   tab <- tabPerm(tabDiv(tab, tabMarg(tab, 2:length(dim(tab)))),
                                  names(dimnames(tab)))
               } else {
                   tab <- tab / sum(tab)
               }
           },
           "all"  = { tab <- tab / sum(tab) },
           "none" = {}
           )
    #attr(tab, "call") <- NULL
    tab
}

#' @rdname array-normalize
ar_normalize <- tabNormalize

#' @rdname array-normalize
ar_norm <- tabNormalize

## FIXME: Document arDist 
## marg and cond: Disjoint sets, possibly NULL. Given either as
## character vectors or integer vectors or rhs-formulae.
## Returns p( marg | cond ).
## There is one other option for cond: A named list with a simple conditioning set.
## In this case, the array is sent to a arSlice.

## ########################################################################
#' @title Marginalize and condition in multidimensional array.
#' 
#' @description Marginalize and condition in a multidimensional
#' array which is assumed to represent a discrete multivariate
#' distribution.
#'
#' @name array-distribution
#' 
## ########################################################################
#'
#' @aliases tabDist 
#' 
#' @note \code{ar_dist} is a recent addition and its functionality (and name) may
#'     change. \code{ar_dist} is based on calling \code{ar_marg} and
#'     \code{ar_slice}.
#' 
#' @param tab Multidimensional array with dimnames.
#' @param marg A specification of the desired margin; a character vector, a
#'     numeric vector or a right hand sided formula.
#' @param cond A specification of what is conditioned on. Can take two forms:
#'     Form one is a a character vector, a numeric vector or a right hand sided
#'     formula. Form two is as a simple slice of the array, which is a list of
#'     the form var1=value1, var2=value2 etc.
#' @param normalize Should the result be normalized to sum to 1.
#' @return A multidimensional array.
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @seealso \code{\link{ar_new}}, \code{\link{ar_marg}}, \code{\link{ar_slice}} etc.
#' @keywords utilities
#' @examples
#' 
#' hec <- HairEyeColor
#' 
#' is.named.array( hec )
#' ## We need dimnames, and names on the dimnames
#' 
#' ## Marginalize:
#' ar_dist(hec, marg= ~Hair + Eye)
#' ar_dist(hec, marg= ~Hair:Eye)
#' ar_dist(hec, marg= c("Hair", "Eye"))
#' ar_dist(hec, marg= 1:2)
#' 
#' ar_dist(hec, marg= ~Hair + Eye, normalize=FALSE)
#' 
#' ## Condition
#' ar_dist(hec, cond= ~Sex + Hair)
#' ar_dist(hec, cond= ~Sex:Hair)
#' ar_dist(hec, cond= c("Sex", "Hair"))
#' ar_dist(hec, cond= c(3,1))
#' 
#' ar_dist(hec, cond= list(Hair="Black"))
#' ar_dist(hec, cond= list(Hair=1))
#' 
#' \dontrun{
#' ## This will fail
#' ar_dist(hec, cond= list(Hair=c("Black", "Brown")))
#' ar_dist(hec, cond= list(Hair=1:2))
#' }
#' ## But this will do the trick
#' a <- ar_slice(hec, slice=list(Hair=c("Black", "Brown")))
#' ar_dist(a, cond=~Hair)
#' 
#' ## Combined
#' ar_dist(hec, marg=~Hair+Eye, cond=~Sex)
#' ar_dist(hec, marg=~Hair+Eye, cond="Sex")
#' 
#' ar_dist(hec, marg=~Hair+Eye, cond=list(Sex="Male"))
#' ar_dist(hec, marg=~Hair+Eye, cond=list(Sex="Male"), normalize=FALSE)
#' 
#' ar_dist(hec, cond=list(Sex="Male"))
#' ar_dist(hec, cond=list(Sex="Male"), normalize=FALSE)
#' 
NULL

tabDist <- function(tab, marg=NULL, cond=NULL, normalize=TRUE){

    .is.simple.cond <- function( cond ){
        z <- unlist(lapply(cond, is.logical), use.names=FALSE)
        has.logical <- any( z )
        u <- unlist(lapply(cond, length), use.names=FALSE)
        is.short <- all( u == 1 )
        if ( !has.logical && is.short ) TRUE
        else if (!all( unlist( cond[ z ] ) ) )
            stop("'cond' is not simple but contains FALSE values; not allowed")
        else FALSE 
    }

    .is.named.list <- function(x){
        is.list( x ) && !is.null( names( x ) )
    }

    .spec2char <- function(x){
        if (is.null( x )) x
        else if ( is.character( x ) ) x
        else if ( is.numeric( x ) ) x
        else if ( class( x ) != "formula" )
            stop("'x' must be NULL, character vector or formula")
        else {
            if (length( x ) != 2) stop("Formula must be a right hand sided formula")
            else x <- all.vars( x[[2]] )
        }
        x            
    }
    
    if (!is.named.array(tab))
        stop("'tab' must be a named array")
    else if (any( tab < 0 ))
        stop("'tab' must be non-negative")
    else if (is.null(marg) && is.null(cond)){
        if (normalize) tab / sum( tab ) else tab
    } else if ( .is.named.list( cond ) ){
        if ( !.is.simple.cond( cond ) )
            stop("'cond' is not 'simple'; can not proceed\n")
        else {
            #message("calling tabDist again")
            tab <- tabSlice( tab, slice = cond, as.array = TRUE )
            tabDist( tab, marg = marg, normalize = normalize )
        }
    } else {
        vset <- names(dimnames( tab ))
        cset <- mset <- NULL

        if ( !is.null( cond ) )
            cset <- .spec2char( cond )
        if ( is.numeric( cset ) )
            cset <- vset[ cset ]
        
        
        if (is.null(marg)){
            mset <- setdiff( vset, cset )
        } else {             ## FIXME check that mset is valid
            mset <- .spec2char( marg )
            if ( length( mset ) == 0 ) stop("Invalid margin specification\n")
        }


        mcset <- c( mset, cset )
#        str(list(marg=marg, cond=cond, mset=mset, cset=cset, mcset=mcset))
        
        if ( !is.null( mcset ) ){
            tab <- tabMarg( tab, marg = mcset)
        }
        
        if ( is.null( cset ) ){
            if ( normalize )
                tab <- tab / sum(tab)            
        } else {
            mtab <- tabMarg( tab, marg=cset)
            tab <- tabDiv( tab, mtab )
        } 
        
        if ( length( mcset ) > 0 )
            if ( !is.null( mcset ) )
                tabPerm(tab, mcset)
            else
                tab
    }
}

#' @rdname array-distribution
ar_dist <- tabDist
































