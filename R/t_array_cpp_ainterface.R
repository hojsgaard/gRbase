## FIXME Need is.rhs.formula function...

## ####################################################################
##
#' @title Interface - operations on multidimensional arrays.
#'
#' @description Interface functions and minor extensions to c++ functions.
#'
#' @name array-interface
#'
## ####################################################################
#' 
## #' @aliases tabAdd__ tabSubt__ tabMult__ tabDiv__ tabDiv0__ tabOp__
## #'     tabMarg__ tabEqual__ tabMarg__ tabPerm__ tabAlign__ tabExpand__
## #'     tabListAdd__ tabListMult__ tabListAdd tabListMult tabExt
## #'     tabEqual tabMarg tabCondProb tabAlign tabPerm
## tabSlice tabSlice2 tabSlicePrim
## #'     tabSliceMult tabExpand tabSlice2Entries tabSlice2Entries_

#' @aliases
## c++ functions
#' tab_align_ tab_expand_ tab_marg_ tab_perm_ 
## for gRain compatibility FIXME REMOVE LATER
#' tabAdd__ tabDiv__ tabDiv0__ tabMarg__ tabMult__ tabSubt__
#' 
#' @param tab,tab1,tab2,... Arrays with named dimnames (we call them
#'     'named arrays').
#' @param lst List of arrays.
#'
#' @param perm A vector of indices or dimnames or a right hand sided
#'     formula giving the desired permutiation.
#' @param marg A vector of indices or dimnames or a right hand sided
#'     formula giving the desired marginal.
#' @param eps Criterion for checking equality of two arrays.
#' @param aux Either a list with names and dimnames or a named array
#'     from which such a list can be extracted.

## for gRain compatibility FIXME REMOVE LATER
tabAdd__  <- tab_add_
tabDiv__  <- tab_div_
tabDiv0__ <- tab_div0_
tabMarg__ <- tab_marg_
tabMult__ <- tab_mult_
tabSubt__ <- tab_subt_

## #'     tabAdd tabDiv tabDiv0 tabMult tabSubt  
## #'     tabSum tabProd

#' tabSubt__ tabMult__ tabOp__
#'     tabEqual__ tabMarg__ tabPerm__ tabAlign__ tabExpand__
#'     tabListAdd__ tabListMult__ tabListAdd tabListMult tabExt
#' 



## ------------------------
## Aliases for cpp functions
## -------------------------

#' @rdname array-interface
tabEqual  <- tab_equal_
#' @rdname array-interface
tabAlign  <- tab_align_
#' @rdname array-interface
tabExpand <- tab_expand_  ## Rethink this
## --- END ---

## -------------------------
## Additional functionality
## -------------------------

#' @rdname array-interface
tabPerm <- function(tab, perm){
    if (!is.named.array(tab)) stop("'tab' not a named array")
    if (!(is.numeric(perm) || is.character(perm) || inherits(perm, "formula")))
        stop("'perm' must be character/numeric vector or right hand sided formula")

    if (inherits(perm, "formula")){  ## A right hand sided formula
        perm <- all.vars( perm[[2]])
    }

    if (is.character(perm)){ ## Allow for name abbreviation
        vn <- names(dimnames( tab ))
        p <- pmatch( perm, vn )
        perm <- vn[p]
    }
    ##cat("perm : ", toString(perm), "\n")
    tab_perm_(tab, perm)
}

#' @rdname array-interface
tabMarg <- function(tab, marg=NULL){
    if (!is.named.array(tab)) stop("'tab' not a named array")
    if (!is.null(marg))
        if (!(is.numeric(marg) || is.character(marg) || inherits(marg, "formula")))
            stop("'marg' must be character/numeric vector or right hand sided formula")

    if (inherits(marg, "formula")){ ## A right hand sided formula
        marg <- all.vars(marg[[2]])
    }

    if (is.character(marg)){ ## Allow for name abbreviation
        vn <- names(dimnames( tab ))
        p <- pmatch( marg, vn )
        marg <- vn[p]
    }
    ##cat("perm : ", toString(perm), "\n")
    tab_marg_(tab, marg)
}


## ------------------------
## Aliases for cpp functions
## -------------------------
#' @rdname array-interface
tabAdd      <- tab_add_
#' @rdname array-interface
tabDiv      <- tab_div_
#' @rdname array-interface
tabDiv0     <- tab_div0_
#' @rdname array-interface
tabMult     <- tab_mult_
#' @rdname array-interface
tabSubt     <- tab_subt_
#' @rdname array-interface
tabListMult <- tab_list_mult_
#' @rdname array-interface
tabListAdd  <- tab_list_add_
## --- END ---

## -------------------------
## Additional functionality
## -------------------------

#' @rdname array-interface
tabSum <- function(...){
    args <- list(...)
    ##message("args:"); print(args); message("-------")
    if (length(args)==0) 0
    else if (length(args)==1 && is.array(args[[1]])) args[[1]]
    else tabListAdd( args )
}

#' @rdname array-interface
tabProd <- function(...){
    args <- list(...)
    ##message("args:"); print(args); message("-------")
    if (length(args)==0) 1
    else if (length(args)==1 && is.array(args[[1]])) args[[1]]
    else tabListMult( args )
}
