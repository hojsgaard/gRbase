## ####################################################################

#' @title Interface - operations on multidimensional arrays.
#'
#' @description Interface functions and minor extensions to cpp functions.
#'
#' @name array-api
#' 
#' @param tab,tab1,tab2,... Arrays with named dimnames (we call them
#'     'named arrays').
#' 
#' @param lst List of arrays.
#'
#' @param perm,marg A vector of indices or dimnames or a right hand sided
#'     formula giving the desired permutation/margin.
#' @param eps Criterion for checking equality of two arrays.
#' @param aux Either a list with names and dimnames or a named array
#'     from which such a list can be extracted.
#'
#' @aliases tabOp__
#'     tabEqual__  tabPerm__ tabAlign__ tabExpand__
#'     tabListAdd__ tabListMult__ tabListAdd tabListMult tabExt
#'     tab_align_ tab_expand_ tab_marg_ tab_perm_
#' 

## ####################################################################

## ------------------------
## Aliases for cpp functions
## -------------------------

#' @rdname array-api
tabAdd      <- tab_add_
#' @rdname array-api
tabAlign  <- tab_align_
#' @rdname array-api
tabDiv      <- tab_div_
#' @rdname array-api
tabDiv0     <- tab_div0_
#' @rdname array-api
tabEqual  <- tab_equal_
#' @rdname array-api
tabExpand <- tab_expand_  ## Rethink this
#' @rdname array-api
tabMult     <- tab_mult_
#' @rdname array-api
tabSubt     <- tab_subt_
#' @rdname array-api
tabListMult <- tab_list_mult_
#' @rdname array-api
tabListAdd  <- tab_list_add_

## -------------------------
## Additional functionality
## -------------------------

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

#' @rdname array-api
tabPerm <- function(tab, perm){
    if (!is.named.array(tab)) stop("'tab' not a named array")
    if (!(is.numeric(perm) || is.character(perm) || inherits(perm, "formula")))
        stop("'perm' must be character/numeric vector or right hand sided formula")
    
    perm <- .get_perm_or_marg(tab, perm)
    ##cat("perm : ", toString(perm), "\n")
    tab_perm_(tab, perm)
}

#' @rdname array-api
tabMarg <- function(tab, marg=NULL){
    if (!is.named.array(tab)) stop("'tab' not a named array")
    if (!is.null(marg))
        if (!(is.numeric(marg) || is.character(marg) || inherits(marg, "formula")))
            stop("'marg' must be character/numeric vector or right hand sided formula")

    marg <- .get_perm_or_marg(tab, marg)
    ##cat("perm : ", toString(perm), "\n")
    tab_marg_(tab, marg)
}

#' @rdname array-api
tabSum <- function(...){
    args <- list(...)
    ##message("args:"); print(args); message("-------")
    if (length(args) == 0) 0
    else if (length(args) == 1 && is.array(args[[1]])) args[[1]]
    else tabListAdd( args )
}

#' @rdname array-api
tabProd <- function(...){
    args <- list(...)
    ##message("args:"); print(args); message("-------")
    if (length(args) == 0) 1
    else if (length(args) == 1 && is.array(args[[1]])) args[[1]]
    else tabListMult( args )
}

