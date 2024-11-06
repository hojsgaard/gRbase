##################################################################
#' @title Create multidimensional tables (arrays)
#' @description Alternative ways of creating tables (arrays)
#' @name api_tab_new
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
##################################################################
#'
#' @details A multidimensional table of numbers is represented by a
#'     multidimensional array, so we can use the terms 'table' and
#'     'array' interchangeably. In this context, 'table' refers
#'     specifically to numerical data structured in multiple
#'     dimensions, similar to how arrays are used in programming. An
#'     alternative representation of a multidimensional table would be
#'     as a dataframe.
#' 
#' 1. If \code{normalize="first"} then for each configuration of all
#'     other variables than the first, the probabilities are
#'     normalized to sum to one.  Thus f(a, b, c) becomes a
#'     conditional probability table of the form p(a | b, c).
#'
#' 1. If \code{normalize="all"} then the sum over all entries of
#'     f(a,b,c) is one.
#' 
#' 1.  If \code{smooth} is positive then \code{smooth} is added to
#'     \code{values} BEFORE normalization takes place.
#' 
#' @param names Names of variables defining table; either a character
#'     vector or a right hand sided formula.
#' @param levels 1) a list with specification of the levels of the
#'     factors in \code{names} or 2) a vector with number of levels of
#'     the factors in \code{names}. See 'examples' below.
#' @param values values to go into the array.
#' @param normalize Either "none", "first" or "all". Should result be
#'     normalized, see 'Details' below.
#' @param smooth Should values be smoothed, see 'Details' below.
#' @return An array.
#' @keywords utilities
#' @examples
#' 
#' universe <- list(gender=c('male', 'female'),
#'                  answer=c('yes', 'no'),
#'                  rain=c('yes', 'no'))
#' t1 <- tab_new(c("gender", "answer"), levels=universe, values=1:4)
#' t1
#' t2 <- tab_new(~gender:answer, levels=universe, values=1:4)
#' t2
#' t3 <- tab_new(~gender:answer, c(2, 2), values=1:4)
#' t3
NULL

#' @export
#' @rdname api_tab_new             
tab_new <- function(names, levels, values, normalize="none", smooth=0) {

    make_dimnames <- function(names, levels){
        if ( !(is.atomic(names) && is.numeric(levels)) )
            stop("Can not create dimnames")
        
        if (length(names) != length(levels))
            stop("'names' and 'levels' must have the same length")
        
        dn <- lapply(seq_along(levels),
                  function(i){ 1:levels[i] })
        
        names(dn) <- names
        return(dn)
    }
    
    if (missing(values))
        values <- 1
    if (smooth > 0)
        values <- values + smooth

    normalize <- match.arg(normalize, choices=c("none", "first", "all"))
    nms <- rhsFormula2list(names)[[1]]

    if (is.list(levels))
    {
        if (length(levels) == 0) {
            stop("Can not create table\n")
        }              
        if (length(levels) == 1) {
            vn <- names(levels)
            levels <- rep(levels, length(nms))
            names(levels) <- nms
            dn <- lapply(levels, function(d) rhsf2list(d)[[1]])
            di <- unlist(lapply(dn, length), use.names=FALSE)            
        }
        else if (length(levels) > 0) {
            if (!is_named_list(levels)) {
                stop("not all elements in 'levels' are named\n")                    
            }
            vn <- names(levels)            
            idx <- match(nms, vn)
            if (any((b <- is.na(idx)))) {
                stop(sprintf("Levels for variable(s): %s not found\n",
                             toString(nms[b])))           
            }
            levels  <- levels[idx] ## those used
            dn <- lapply(levels, function(d) rhsf2list(d)[[1]])
            di <- unlist(lapply(dn, length), use.names=FALSE)            
        }

    }
    else if (is.numeric(levels))
    {
        di <- levels
        dn <- make_dimnames(nms, levels)
    }
    else if (is.character(levels))
    {
        dn <- rep(list(levels), length(nms))
        names(dn) <- nms
        di <- unlist(lapply(dn, length), use.names=FALSE)
    } else {
        stop("Can not create 'tab' object")
    }

    
    if (is.atomic(values) && !is.object(values)) {
        out <- array(values, dim=di, dimnames=dn)
    }
    tab_normalize(out, normalize)
}





## ####################################################################
#'
#' @title Interface - operations on multidimensional tables (arrays).
#' @description Interface functions and minor extensions to cpp functions.
#' @name api-tabX
#'
## ####################################################################
#'
#' @param tab,tab1,tab2,... Arrays with named dimnames (we call them
#'     'named arrays').
#' @param type One of "none", "first" and "all".
#' @param lst List of arrays.
#' @param perm,marg A vector of indices or dimnames or a right hand
#'     sided formula giving the desired permutation/margin.
#' @param eps Criterion for checking equality of two arrays.
#' @param aux Either a list with names and dimnames or a named array
#'     from which such a list can be extracted.
#' @param type If 0 then entries are duplicated. If 3 then averages
#'     are computed. If 2 then 0 slices are inserted.
#' @param op The algebraic operation to be carried out.
#'
NULL

## ------------------------
## Aliases for cpp functions
## -------------------------

#' @export
#' @rdname api-tabX
tab_add      <- tab_add_


#' @export
#' @rdname api-tabX
tab_align  <- tab_align_

#' @export
#' @rdname api-tabX
tab_div      <- tab_div_



#' @export
#' @rdname api-tabX
tab_div0     <- tab_div0_



#' @export
#' @rdname api-tabX
tab_op     <- tab_op_

#' @export
#' @rdname api-tabX
tab_equal  <- tab_equal_

## tabMult used by grain; 
#' @export
#' @rdname api-tabX
tabMult     <- tab_mult_

#' @export
#' @rdname api-tabX
tab_mult     <- tab_mult_

#' @export
#' @rdname api-tabX
tab_subt     <- tab_subt_

#' @export
#' @rdname api-tabX
tab_list_mult <- tab_list_mult_

#' @export
#' @rdname api-tabX
tab_list_add  <- tab_list_add_



## -------------------------
## Additional functionality
## -------------------------

#' @export
#' @rdname api-tabX
tab_expand <- function(tab, aux, type=0L) {  ## FIXME Rethink this

    if (is.list(aux))
        aux <- lapply(aux, rhsf2vec)
    
    tab_expand_(tab, aux, type)
}

#' @export
#' @rdname api-tabX
tab_perm <- function(tab, perm) {
    stopifnot_named_array(tab)
    
    perm <- set_spec_to_char(tab, perm)
    tab_perm_(tab, perm)
}


#' @export
#' @rdname api-tabX
tab_marg <- function(tab, marg=NULL) {
    stopifnot_named_array(tab)

    marg <- set_spec_to_char(tab, marg)
    tab_marg_(tab, marg)
}

#' @export
#' @rdname api-tabX
tabMarg <- tab_marg

#' @export
#' @rdname api-tabX
tab_sum <- function(tab, ...) {
    if (missing(tab)) return(0)
    args <- c(list(tab), list(...))
    tab_list_add(listify_dots(args))
}

#' @export
#' @rdname api-tabX
tab_prod <- function(tab, ...) {
    if (missing(tab)) return(0)
    args <- c(list(tab), list(...))
    tab_list_mult(listify_dots(args))
}



#' @export
#' @rdname api-tabX                 
tab_normalize <- function(tab, type="none") {
    ## cat("tab_normalize\n"); print(tab)
    switch(type,
           "first"={
               if (length(dim(tab)) > 1){
                   denom <- rep(tab_marg(tab, 2:length(dim(tab))), each=dim(tab)[1])                   
                   tab <- tab / denom
               } else {
                   tab <- tab / sum(tab)
               }
           },
           "all"  = { tab <- tab / sum(tab) },
           "none" = {}
           )
    tab
}




## ####################################################################
##
#' @title Marginalize and condition in multidimensional array.
#' @description Marginalize and condition in a multidimensional array
#'     which is assumed to represent a discrete multivariate
#'     distribution.
#' @name api_tab_dist
##
########################################################################
#'
#' @aliases tab_dist_worker 
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
#' is_named_array( hec )
#' ## We need dimnames, and names on the dimnames
#' 
#' ## Marginalize:
#'tabDist(hec, marg= ~Hair:Eye)
#'tabDist(hec, marg= c("Hair", "Eye"))
#'tabDist(hec, marg= 1:2)
#' 
#'tabDist(hec, marg= ~Hair + Eye, normalize=FALSE)
#' 
#' ## Condition
#'tabDist(hec, cond= ~Sex + Hair)
#'tabDist(hec, cond= ~Sex:Hair)
#'tabDist(hec, cond= c("Sex", "Hair"))
#'tabDist(hec, cond= c(3,1))
#' 
#'tabDist(hec, cond= list(Hair="Black"))
#'tabDist(hec, cond= list(Hair=1))
#' 
#' \dontrun{
#' ## This will fail
#'tabDist(hec, cond= list(Hair=c("Black", "Brown")))
#'tabDist(hec, cond= list(Hair=1:2))
#' }
#' ## But this will do the trick
#' a <- tab_slice(hec, slice=list(Hair=c("Black", "Brown")))
#'tabDist(a, cond=~Hair)
#' 
#' ## Combined
#'tabDist(hec, marg=~Hair+Eye, cond=~Sex)
#'tabDist(hec, marg=~Hair+Eye, cond="Sex")
#' 
#'tabDist(hec, marg=~Hair+Eye, cond=list(Sex="Male"))
#'tabDist(hec, marg=~Hair+Eye, cond=list(Sex="Male"), normalize=FALSE)
#' 
#'tabDist(hec, cond=list(Sex="Male"))
#'tabDist(hec, cond=list(Sex="Male"), normalize=FALSE)
#' 
NULL

#' @export
#' @rdname api_tab_dist
tabDist <- function (tab, marg = NULL, cond = NULL, normalize = TRUE) {

    if (!is.list(cond))
        tab_dist_worker(tab, marg=marg, cond=cond, normalize=normalize)
    else {
        ## Are there formulae in cond?
        ## print(cond)
        idx <- sapply(cond, function(x) inherits(x, "formula"))
        ## If yes, turn these into a vector
        cond1 <- sapply(cond[idx], rhsf2list)
        cond1 <- unlist(cond1)
        ## cond1

        ## Look at the rest 
        cond2 <- cond[!idx]
        ## cond2
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

        ##str(list(marg=marg, condset=condset, condnv=condnv))
        if (!is.null(condnv))    
            tab <- tab_dist_worker(tab, cond=condnv, normalize=normalize)

        tab <- tab_dist_worker(tab, marg=marg, cond=condset, normalize=normalize)
        return(tab)
    }
}

## ' @export
## ' @rdname api_tab_dist
tab_dist_worker <- function(tab, marg=NULL, cond=NULL, normalize=TRUE) {

    .is.simple.cond <- function( cond ) {  ## What is this
        z <- unlist(lapply(cond, is.logical), use.names=FALSE)
        has.logical <- any( z )
        u <- unlist(lapply(cond, length), use.names=FALSE)
        is.short <- all( u == 1 )
        if ( !has.logical && is.short ) TRUE
        else if (!all( unlist( cond[ z ] ) ) )
            stop("'cond' is not simple but contains FALSE values; not allowed")
        else FALSE 
    }

    ## str(list(marg=marg, cond=cond))
    stopifnot_named_array(tab)
    if (any(tab < 0))
        stop("'tab' must be non-negative")

    if ((length(marg)==0) && (length(cond)==0)){
        if (normalize) return(tab / sum(tab)) else return(tab)
    }

    if (.is.named.list( cond )){
        if (!.is.simple.cond( cond ))
            stop("'cond' is not 'simple'; can not proceed\n")
        else {
            tab <- tab_slice(tab, slice = cond, as.array = TRUE)
            tab_dist_worker(tab, marg=marg, normalize=normalize)
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
            tab <- tab_marg(tab, marg = mcset)
        }
        
        if (length(cset) == 0){
            if (normalize) tab <- tab / sum(tab)            
        } else {
            mtab <- tab_marg(tab, marg=cset)
            tab  <- tab_div(tab, mtab)
        } 
        
        if (length(mcset) > 0)
            if (!is.null(mcset)) tab_perm(tab, mcset) else tab
    }
}





########################################################################
#'
#' @title Array slices
#' @description Functions for extracting slices of arrays
#' @name api_tab_slice
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' 
## ########################################################################
#'
#' @param tab An array with named dimnames.
#' @param slice A list defining the slice.
#' @param margin Names of variables in slice.
## #' @param margin.idx Indec of variables in slice.
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
#' s1 = tab_slice(x, slice=s); s1
#'
#' tab_slice_to_entries(x, slice=s)
#' tab_slice_to_entries(x, slice=s, complement=TRUE)
#'
#' ## tab_slice_mult 
#' s2 = tab_slice_mult(x, slice=s); s2
#'
#' sp = list(c(1,2), c(1,2), TRUE)
#' tab_slice_prim(x, slice=sp)
#' tab_slice(x, slice=s)
NULL

#' @export
#' @rdname api_tab_slice
tab_slice <- function(tab, slice=NULL, margin=names(slice), drop=TRUE, as.array=FALSE) {

    stopifnot_named_array(tab)

    if ( is.null( slice ) )
        return(tab)
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
        tab_slice_worker(tab, slice, margin.idx, drop=drop, as.array=as.array)
    }
}

tab_slice_worker <- function(tab, slice, margin.idx, drop=TRUE, as.array=FALSE) {

    z <- as.list(rep(TRUE,  length(dim(tab))))
    z[ margin.idx ] <- slice
    out <- do.call("[", c(list(tab), z, drop=drop))
    
    if (as.array && is.null(dim( out ))) {
        dn <- list(names(out))
        k  <- which(unlist(lapply(z, is.logical))) # idx of variables still in array
        names(dn) <- names( dimnames( tab ) )[ k ]
        array( out, dim=length(out), dimnames=dn )
    } else {
        out
    }
}


#' @export
#' @rdname api_tab_slice
tab_slice_prim <- function(tab, slice, drop=TRUE) {
    do.call("[", c(list(tab), slice, drop=drop))        
}

#' @export
#' @rdname api_tab_slice
tab_slice_mult <- function(tab, slice, val=1, comp=0) {
    if ( !is.null(val) ){
        idx <- tab_slice_to_entries(tab, slice)
        tab[idx] <- tab[idx] * val
    }
    if ( !is.null(comp) ){
        idx <- tab_slice_to_entries(tab, slice, complement=TRUE)
        tab[idx] <- tab[idx] * comp
    }
    tab
}


#' @export
#' @rdname api_tab_slice
tab_slice_to_entries <- function(tab, slice, complement=FALSE) {
  tab[] <- 1:length(tab)
  out <- tab_slice(tab, slice, margin=names(slice))
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


## FIXME: gør disse to funktioner mon ikke næsten det samme?

.spec2char <- function(x) {
    if (is.null( x )) x
    else if ( is.character( x ) ) x
    else if ( is.numeric( x ) ) x
    else if ( !inherits( x, "formula" ))
        stop("'x' must be NULL, character vector or formula")
    else {
        if (length( x ) != 2) stop("Formula must be a right hand sided formula")
        else x <- all.vars( x[[2]] )
    }
    x            
}

set_spec_to_char <- function(tab, set_spec) {

    if (!(is.numeric(set_spec) || is.character(set_spec) || inherits(set_spec, "formula")))
        stop("'set_spec' must be character/numeric vector or right hand sided formula")

    if (inherits(set_spec, "formula")){  ## A right hand sided formula
        set_spec <- all.vars(set_spec[[2]])
    }
    
    if (is.character(set_spec)){ ## Allow for name abbreviation
        vn <- names(dimnames( tab ))
        p <- pmatch(set_spec, vn)
        set_spec <- vn[p]
    }
    set_spec
}


########################################################
#' @title Array algebra
#' @description Addition, subtraction etc. of arrays
#' @name api_ops_pct
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
########################################################
#' 
#' @param a,a1,a2 Arrays (with named dimnames)
## #' @param lst List of arrays.
#' @param tab1,tab2 Multidimensional arrays with named dimnames
#'     (we call them 'named arrays').
#' @param perm A vector of indices or dimnames or a right hand sided
#'     formula giving the desired permutiation.
#' @param marg A vector of indices or dimnames or a right hand sided
#'     formula giving the desired marginal.
#' @param slice A list of the form name=value. 
## #' @param eps Criterion for checking equality of two arrays.
#' @param extra List defining the extra dimensions.
## #' @param aux Either a list with names and dimnames or a named array
## #'     from which such a list can be extracted.
#' @aliases %a+% %a-% %a*% %a/% %a/0%
#'
#' @examples
#' hec <- HairEyeColor
#' a1 <- tab_marg(hec, c("Hair", "Eye"))
#' a2 <- tab_marg(hec, c("Hair", "Sex"))
#' a3 <- tab_marg(hec, c("Eye", "Sex"))
#'
#' ## Binary operations
#' a1 %a+% a2
#' a1 %a-% a2
#' a1 %a*% a2
#' a1 %a/% a2

#' @export
#' @rdname api_ops_pct
"%a+%" <- function(a1, a2){tab_add(a1,a2)}

#' @export
#' @rdname api_ops_pct
"%a-%" <- function(a1, a2){tab_subt(a1,a2)}

#' @export
#' @rdname api_ops_pct
"%a*%" <- function(a1, a2){tab_mult(a1,a2)}

#' @export
#' @rdname api_ops_pct
"%a/%" <- function(a1, a2){tab_div(a1,a2)}

#' @export
#' @rdname api_ops_pct
"%a/0%" <- function(a1, a2){tab_div0(a1,a2)}

#' @export
#' @rdname api_ops_pct
"%a_%" <- function(tab1, marg){tab_marg(tab1, marg)}

#' @export
#' @rdname api_ops_pct
"%a==%" <- function(tab1, tab2){tab_equal(tab1, tab2)}

#' @export
#' @rdname api_ops_pct
"%a^%" <- function(tab1, extra){tab_expand(tab1, extra)}

#' @export
#' @rdname api_ops_pct
"%aperm%" <- function(tab1, perm){tab_perm(tab1, perm)}

#' @export
#' @rdname api_ops_pct                   
"%aalign%" <- function(tab1, tab2){tab_align(tab1, tab2)}

#' @export
#' @rdname api_ops_pct                   
"%aslice%" <- function(tab1, slice){tab_slice(tab1, slice)}

#' @export
#' @rdname api_ops_pct                   
"%aslice*%" <- function(tab1, slice){tab_slice_mult(tab1, slice)}

#' @export
#' @rdname api_ops_pct
"%amarg%" <- function(tab1, marg){tab_marg(tab1, marg)}






### ----------------------------------------------------------
### COMPATIBILITY with gRain and gRim
### ----------------------------------------------------------


## FIXME used in gRain
#' @export
#' @rdname api_tab_new             
tabNew <- tab_new

## FIXME grain
#' @export
#' @rdname api_tab_slice
tabSliceMult <- tab_slice_mult


## FIXME gRain
#' @export
#' @rdname api-tabX
tabDiv      <- tab_div_

## FIXME gRain
#' @export
#' @rdname api-tabX
tabDiv0     <- tab_div0_

## FIXME gRain
#' @export
#' @rdname api_tab_slice
tabSlice <- tab_slice

## FIXME gRain
#' @export
#' @rdname api-tabX
tabPerm <- tab_perm


## FIXME gRain
#' @export
#' @rdname api-tabX
tabProd <- tab_prod

## FIXME gRain
#' @export
#' @rdname api-tabX                 
tabNormalize <- tab_normalize


#' @export
#' @rdname api-tabX
tabListMult <- tab_list_mult_

