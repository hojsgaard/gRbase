
##################################################################
#' @title Create multidimensional arrays
#' @description Alternative ways of creating arrays
#' @name api-tabNew
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
##################################################################
#'
#' @details
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
#' t1 <- tabNew(c("gender", "answer"), levels=universe, values=1:4)
#' t1
#' t2 <- tabNew(~gender:answer, levels=universe, values=1:4)
#' t2
#' t3 <- tabNew(~gender:answer, c(2, 2), values=1:4)
#' t3
NULL

#' @export
#' @rdname api-tabNew             
tabNew <- function(names, levels, values, normalize="none", smooth=0) {

    ## print("tabNew")
    
    normalize <- match.arg(normalize, choices=c("none", "first", "all"))
    names <- rhsFormula2list(names)[[1]]

    if (is.list(levels))
    {
        if (length(levels) == 0){
            stop("Can not create table\n")
        }              
        if (length(levels) == 1){
            vn <- names(levels)
            levels <- rep(levels, length(names))
            names(levels) <- names
            dn <- lapply(levels, function(d) rhsf2list(d)[[1]])
            di <- unlist(lapply(dn, length), use.names=FALSE)            
            
        } else if (length(levels) > 0){
            if (!is_named_list(levels)){
                stop("not all elements in 'levels' are named\n")                    
            }
            vn <- names(levels)            
            idx <- match(names, vn)
            if (any((b <- is.na(idx)))){
                stop(sprintf("Levels for variable(s): %s not found\n",
                             toString(names[b])))           
            }
            levels  <- levels[idx] ## those used
            dn <- lapply(levels, function(d) rhsf2list(d)[[1]])
            di <- unlist(lapply(dn, length), use.names=FALSE)            
        }

    }
    else if (is.numeric(levels))
    {
        di <- levels
        dn <- make_dimnames(names, levels)
    }
    else if (is.character(levels))
    {
        dn <- rep(list(levels), length(names))
        names(dn) <- names
        di <- unlist(lapply(dn, length), use.names=FALSE)
    } else {
        stop("Can not create 'tab' object")
    }

    if (missing(values))
        values <- 1
    if (smooth > 0)
        values <- values + smooth
    
    if (is.atomic(values) && !is.object(values)){
        out <- array(values, dim=di, dimnames=dn)
    }
    tabNormalize(out, normalize)
}
    

is_named_list <- function(x) {
    if (!inherits(x, "list"))
        return(FALSE)
    vn <- names(x)
    if (is.null(vn))
        return(FALSE)
    all(nchar(vn) > 0)
}


## FIXME make_dimnames findes også (cirka) i parray. Redundans
make_dimnames <- function(names, levels){
    if ( !(is.atomic(names) && is.numeric(levels)) )
        stop("Can not create dimnames")

    if (length(names) != length(levels))
        stop("'names' and 'levels' must have the same length")
    
    dn <- lapply(seq_along(levels),
                  function(i){
                      1:levels[i]
                  })
    
    names(dn) <- names
    dn
}





                
        ## print(vn)
        ## if (!all(sapply(vn, nchar) > 0)){
            ## print(vn)
            ## stop("not all elements in 'levels' are named\n")
        ## }
        ## idx <- match(names, vn)
        ## if (any((b <- is.na(idx))))
            ## stop(sprintf("Levels for variable(s): %s not found\n",
                         ## toString(names[b])))
        ## else {
            ## levels  <- levels[idx] ## those used
            ## dn <- lapply(levels, function(d) rhsf2list(d)[[1]])
            ## di <- unlist(lapply(dn, length), use.names=FALSE)
        ## }
