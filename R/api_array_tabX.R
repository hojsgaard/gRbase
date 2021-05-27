## ####################################################################
#'
#' @title Interface - operations on multidimensional arrays.
#' @description Interface functions and minor extensions to cpp functions.
#' @name api-tabX
#'
## ####################################################################
#'
#' @param tab,tab1,tab2,... Arrays with named dimnames (we call them
#'     'named arrays').
#' @param type One of "none", "first" and "all".
#' 
#' @param lst List of arrays.
#'
#' @param perm,marg A vector of indices or dimnames or a right hand
#'     sided formula giving the desired permutation/margin.
#' @param eps Criterion for checking equality of two arrays.
#' @param aux Either a list with names and dimnames or a named array
#'     from which such a list can be extracted.
#' @param type If 0 then entries are duplicated. If 3 then averages
#'     are computed. If 2 then 0 slices are inserted.
#' @param op The algebraic operation to be carried out.
#'
## #' @aliases 
## #'     tab_align_
## #' tab_expand_ tab_marg_ tab_perm_
#' 

## ------------------------
## Aliases for cpp functions
## -------------------------

#' @export
#' @rdname api-tabX
tabAdd      <- tab_add_

#' @export
#' @rdname api-tabX
tabAlign  <- tab_align_

#' @export
#' @rdname api-tabX
tabDiv      <- tab_div_

#' @export
#' @rdname api-tabX
tabDiv0     <- tab_div0_

#' @export
#' @rdname api-tabX
tabOp     <- tab_op_

#' @export
#' @rdname api-tabX
tabEqual  <- tab_equal_

#' @export
#' @rdname api-tabX
tabExpand <- function(tab, aux, type=0L){  ## FIXME Rethink this

    if (is.list(aux))
        aux <- lapply(aux, rhsf2vec)
    
    tab_expand_(tab, aux, type)
}


## tabMult used by grain; 
#' @export
#' @rdname api-tabX
tabMult     <- tab_mult_

#' @export
#' @rdname api-tabX
tabSubt     <- tab_subt_

#' @export
#' @rdname api-tabX
tabListMult <- tab_list_mult_

#' @export
#' @rdname api-tabX
tabListAdd  <- tab_list_add_


## -------------------------
## Additional functionality
## -------------------------

#' @export
#' @rdname api-tabX
tabPerm <- function(tab, perm){
    if (!is.named.array(tab)) stop("'tab' not a named array")
    if (!(is.numeric(perm) || is.character(perm) || inherits(perm, "formula")))
        stop("'perm' must be character/numeric vector or right hand sided formula")
    
    perm <- .get_perm_or_marg(tab, perm)
    ##cat("perm : ", toString(perm), "\n")
    tab_perm_(tab, perm)
}

#' @export
#' @rdname api-tabX
tabMarg <- function(tab, marg=NULL){
    if (!is.named.array(tab)) stop("'tab' not a named array")
    if (!is.null(marg))
        if (!(is.numeric(marg) || is.character(marg) || inherits(marg, "formula")))
            stop("'marg' must be character/numeric vector or right hand sided formula")

    marg <- .get_perm_or_marg(tab, marg)
    ##cat("perm : ", toString(perm), "\n")
    tab_marg_(tab, marg)
}



#' @export
#' @rdname api-tabX
tabSum <- function(tab, ...){
    if (missing(tab)) return(0)
    args <- c(list(tab), list(...))
    tabListAdd(listify_dots(args))
}

#' @export
#' @rdname api-tabX
tabProd <- function(tab, ...){
    if (missing(tab)) return(0)
    args <- c(list(tab), list(...))
    tabListMult(listify_dots(args))
}


## #' @export
## #' @rdname api-tabX
## tabSum <- function(...){
##     args <- list(...)
##     ##message("args:"); print(args); message("-------")
##     if (length(args) == 0) 0
##     else if (length(args) == 1 && is.array(args[[1]])) args[[1]]
##     else tabListAdd( args )
## }

## #' @export
## #' @rdname api-tabX
## tabProd <- function(...){
##     args <- list(...)
##     ##message("args:"); print(args); message("-------")
##     if (length(args) == 0) 1
##     else if (length(args) == 1 && is.array(args[[1]])) args[[1]]
##     else tabListMult( args )
## }

#' @export
#' @rdname api-tabX                 
tabNormalize <- function(tab, type="none"){
    switch(type,
           "first"={
               if (length(dim(tab)) > 1){
                   tab <- tabPerm(tabDiv(tab, tabMarg(tab, 2:length(dim(tab)))),
                                  names(dimnames(tab)))
               } else {
                   tab <- tab / sum(tab)
               }
           },
           "all"  = { tab <- tab / sum(tab) },
           "none" = {}
           )
    tab
}






## FIXME: Document tabDist 
## marg and cond: Disjoint sets, possibly NULL. Given either as
## character vectors or integer vectors or rhs-formulae.
## Returns p( marg | cond ).
## There is one other option for cond: A named list with a simple conditioning set.
## In this case, the array is sent to a arSlice.

## ####################################################################
##
#' @title Marginalize and condition in multidimensional array.
#' @description Marginalize and condition in a multidimensional array
#'     which is assumed to represent a discrete multivariate
#'     distribution.
#' @name api-tabDist
##
########################################################################
#'
#' @aliases tabDist 
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
#' @keywords utilities
#' @examples
#' 
#' hec <- HairEyeColor
#' 
#' is.named.array( hec )
#' ## We need dimnames, and names on the dimnames
#' 
#' ## Marginalize:
#' tabDist(hec, marg= ~Hair + Eye)
#' tabDist(hec, marg= ~Hair:Eye)
#' tabDist(hec, marg= c("Hair", "Eye"))
#' tabDist(hec, marg= 1:2)
#' 
#' tabDist(hec, marg= ~Hair + Eye, normalize=FALSE)
#' 
#' ## Condition
#' tabDist(hec, cond= ~Sex + Hair)
#' tabDist(hec, cond= ~Sex:Hair)
#' tabDist(hec, cond= c("Sex", "Hair"))
#' tabDist(hec, cond= c(3,1))
#' 
#' tabDist(hec, cond= list(Hair="Black"))
#' tabDist(hec, cond= list(Hair=1))
#' 
#' \dontrun{
#' ## This will fail
#' tabDist(hec, cond= list(Hair=c("Black", "Brown")))
#' tabDist(hec, cond= list(Hair=1:2))
#' }
#' ## But this will do the trick
#' a <- tabSlice(hec, slice=list(Hair=c("Black", "Brown")))
#' tabDist(a, cond=~Hair)
#' 
#' ## Combined
#' tabDist(hec, marg=~Hair+Eye, cond=~Sex)
#' tabDist(hec, marg=~Hair+Eye, cond="Sex")
#' 
#' tabDist(hec, marg=~Hair+Eye, cond=list(Sex="Male"))
#' tabDist(hec, marg=~Hair+Eye, cond=list(Sex="Male"), normalize=FALSE)
#' 
#' tabDist(hec, cond=list(Sex="Male"))
#' tabDist(hec, cond=list(Sex="Male"), normalize=FALSE)
#' 
NULL

#' @export
#' @rdname api-tabDist
tabDist <- function (tab, marg = NULL, cond = NULL, normalize = TRUE) {

    if (!is.list(cond))
        .tabDist(tab, marg=marg, cond=cond, normalize=normalize)
    else{
        ## Are there formulae in cond?
        idx <- sapply(cond, function(x) inherits(x, "formula"))
        ## If yes, turn these into a vector
        cond1 <- sapply(cond[idx], rhsf2list)
        cond1 <- unlist(cond1)
        cond1

        ## Look at the rest 
        cond2 <- cond[!idx]
        cond2
        ## Are there names in the rest?
        if (is.null(names(cond2))){
            ## No, so the rest is just a list (of characters, hopefully)
            cond3 <- unlist(cond2)
            cond2  <- NULL
            condnv <- NULL ## nv means name=value
        } else {
            ## Yes, and take the elements with names and put into
            ## condnv; put the rest into cond3
            idx2  <- nchar(names(cond2)) == 0
            cond3 <- unlist(cond2[idx2])
            condnv <- cond2[!idx2]
        }
        
        condset <- c(cond1, cond3)


        if (!is.null(condnv))    
            tab <- .tabDist(tab, marg=marg, cond=condnv, normalize=normalize)
        if (!is.null(condset))
            tab <- .tabDist(tab, marg=marg, cond=condset, normalize=normalize)
        return(tab)
    }
}

## ' @export
## ' @rdname api-tabDist
.tabDist <- function(tab, marg=NULL, cond=NULL, normalize=TRUE){
    
    if (!is.named.array(tab))
        stop("'tab' must be a named array")
    if (any(tab < 0))
        stop("'tab' must be non-negative")

    if ((length(marg)==0) && (length(cond)==0)){
        if (normalize) return(tab / sum(tab)) else return(tab)
    }

    if (.is.named.list( cond )){
        if (!.is.simple.cond( cond ))
            stop("'cond' is not 'simple'; can not proceed\n")
        else {
            ##message("calling tabDist again")
            tab <- tabSlice(tab, slice = cond, as.array = TRUE)
            tabDist(tab, marg=marg, normalize=normalize)
        }
    } else {
        vset <- names(dimnames( tab ))
        cset <- mset <- NULL
        
        if (!is.null(cond))
            cset <- .spec2char( cond )
        if (is.numeric(cset))
            cset <- vset[cset]
                
        if (is.null(marg)){
            mset <- setdiff( vset, cset )
        } else {             ## FIXME check that mset is valid
            mset <- .spec2char( marg )
            if (length(mset) == 0) stop("Invalid margin specification\n")
        }

        mcset <- c(mset, cset)
        ##str(list(marg=marg, cond=cond, mset=mset, cset=cset, mcset=mcset))
        
        if (!is.null(mcset)){
            tab <- tabMarg(tab, marg = mcset)
        }
        
        if (length(cset) == 0){
            if (normalize) tab <- tab / sum(tab)            
        } else {
            mtab <- tabMarg(tab, marg=cset)
            tab  <- tabDiv(tab, mtab)
        } 
        
        if (length(mcset) > 0)
            if (!is.null(mcset)) tabPerm(tab, mcset) else tab
    }
}





########################################################################
#'
#' @title Array slices
#' @description Functions for extracting slices of arrays
#' @name api_tabSlice
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' 
## ########################################################################
#'
#' @param tab An array with named dimnames.
#' @param slice A list defining the slice.
#' @param margin Names of variables in slice.
#' @param margin.idx Indec of variables in slice.
#' @param drop If TRUE then dimensions with only one level will be
#'     dropped from the output.
#' @param as.array If the resulting array is one-dimensional the
#'     result will by default be a vector with no dim attribute unless
#'     as.array is TRUE.
#' @param val The values that entries in the slice will be multiplied
#'     with.
#' @param comp The values that entries NOT in the slice will be
#'     multiplied with.
#' @param complement If TRUE the complement of the entries are
#'     returned.
#' 
#' @examples
#'
#' x = HairEyeColor
#' s = list(Hair=c("Black", "Brown"), Eye=c("Brown", "Blue"))
#'
#' s1 = tabSlice(x, slice=s); s1
#'
#' tabSlice2Entries(x, slice=s)
#' tabSlice2Entries(x, slice=s, complement=TRUE)
#'
#' ## tabSliceMult 
#' s2 = tabSliceMult(x, slice=s); s2
#'
#' sp = list(c(1,2), c(1,2), TRUE)
#' tabSlicePrim(x, slice=sp)
#' tabSlice(x, slice=s)
NULL

#' @export
#' @rdname api_tabSlice
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
    dn <- names(dimnames(tab))
    margin.idx <- if (is.character(margin)){
      match(margin, dn)
    } else margin
    
    if (any(idx <- is.na(margin.idx))){
      cat("Error: Names not in domain : ", toString(margin[idx]), "\n")
      stop("Invalid 'margin'")      
    }
    tabSlice2(tab, slice, margin.idx, drop=drop, as.array=as.array)
  }
}

#' @export
#' @rdname api_tabSlice
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

#' @export
#' @rdname api_tabSlice
tabSlicePrim <- function(tab, slice, drop=TRUE){
    do.call("[", c(list(tab), slice, drop=drop))        
}

#' @export
#' @rdname api_tabSlice
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

#' @export
#' @rdname api_tabSlice
tabSlice2Entries <- function(tab, slice, complement=FALSE){
  tab[] <- 1:length(tab)
  out <- tabSlice(tab, slice, margin=names(slice))
  if (complement)
    c(tab)[-c(out)]
  else
    c(out)
}





## ########################################################
##
## dot-functions below here
##
## ########################################################

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

.get_perm_or_marg <- function(tab, perm){
    if (inherits(perm, "formula")){  ## A right hand sided formula
        perm <- all.vars(perm[[2]])
    }
    
    if (is.character(perm)){ ## Allow for name abbreviation
        vn <- names(dimnames( tab ))
        p <- pmatch(perm, vn)
        perm <- vn[p]
    }
    perm
}




## #' ###################################################################
## #'
## #' @title Convert dataframe to contingency table
## #' @description: Much like xtabs but with more flexibility
## #' @name df2xtabs
## #'
## ##  ###################################################################
## #' 
## #' @param indata A dataframe.
## #' @param names Names of variables defining table; a character vector
## #'     or a right hand sided formula.
## #' @param normalize Either "none", "first" or "all". Should result be
## #'     normalized, see 'Details' below.
## #' @param smooth Should values be smoothed, see 'Details' below.
## #' 
## #' @examples
## #' ## Extract arrays from dataframe (much like xtabs() but with more flexibility)
## #' data(cad1) 
## #' df2xtabs(cad1, ~Sex:AngPec:AMI)
## #' df2xtabs(cad1, c("Sex", "AngPec", "AMI"))
## #' df2xtabs(cad1, c(1, 2, 3))

## df2xtabs <- function(indata, names=NULL, normalize="none", smooth=0){

##     if ( !( is.data.frame(indata) ) )
##         stop("'indata' must a dataframe\n")
    
##     if (!is.null( names )) {
##         if (is.numeric( names )){
##             if (min(names) < 1 || max(names) > ncol(indata)){
##                 stop("columns out of range \n")
##             }
##         } else {
##             if (class(names) %in% c("formula", "character")){
##                 names <- rhsf2list(names)[[1]]
##             } else {
##                 stop("don't know what to do\n")
##             }
##         }
##     } 
    
##     out <- if (is.null(names)) xtabs(~., data=indata)
##            else xtabs(~., data=indata[, names, drop=FALSE])
    
##     ## FIXME : There is no check on what smooth is
##     if (smooth > 0)
##         out <- out + smooth
    
##     if (normalize != "none")
##         tabNormalize( out, normalize )
##     else out
## }








## if (!is.named.array(tab)) stop("'tab' not a named array")
## if (!is.null(aux))
##     if (!(is.numeric(aux) || is.character(aux) || inherits(aux, "formula")))
##         stop("'aux' must be character/numeric vector or right hand sided formula")

## aux <- .get_perm_or_marg(tab, aux)

