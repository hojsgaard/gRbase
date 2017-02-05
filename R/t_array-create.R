## ##################################################################
##
## Multiway arrays
##
## ##################################################################

#' @title Create multidimensional arrays
#'
#' @description Alternative ways of creating arrays
#' 
#' @details If \code{normalize="first"} then for each configuration of
#'     all other variables than the first, the probabilities are
#'     normalized to sum to one.  Thus f(a,b,c) becomes a conditional
#'     probability table of the form p(a|b,c).  If
#'     \code{normalize="all"} then the sum over all entries of
#'     f(a,b,c) is one.
#' 
#'     If \code{smooth} is positive then
#'     \code{smooth} is added to \code{values} before normalization
#'     takes place.
#'
#' @name array-create
#' 
#' @aliases df2xtabs tab
#' @param names Names of variables defining table; a character vector or a right
#'     hand sided formula.
#' @param levels 1) a list with specification of the levels of the factors in
#'     \code{names} or 2) a vector with number of levels of the factors in
#'     \code{names}. See 'examples' below.
#' @param values values to go into the parray
#' @param normalize Either "none", "first" or "all". Should result be
#'     normalized, see 'Details' below.
#' @param smooth Should values be smoothed, see 'Details' below.
#' @return An array.
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @seealso \code{\link{ar_perm}}, \code{\link{ar_add}}, \code{\link{ar_prod}} etc.
#' @keywords utilities
#' @examples
#' 
#' universe <- list(gender=c('male','female'),
#'                  answer=c('yes','no'),
#'                  rain=c('yes','no'))
#' t1 <- ar_new(c("gender","answer"), levels=universe, values=1:4)
#' t1
#' t2 <- ar_new(~gender:answer, levels=universe, values=1:4)
#' t2
#' t3 <- ar_new(~gender:answer, c(2,2), values=1:4)
#' t3
#' 
#' ## Extract arrays from dataframe (much like xtabs() but with more flexibility)
#' data(cad1) 
#' df2xtabs(cad1, ~Sex:AngPec:AMI)
#' df2xtabs(cad1, c("Sex","AngPec","AMI"))
#' df2xtabs(cad1, c(1,2,3))
#' 
#' @export tab
#' @examples
#' universe <- list(gender=c('male','female'),
#'                  answer=c('yes','no'),
#'                  rain=c('yes','no'))
#' t1 <- ar_new(c("gender","answer"), levels=universe, values=1:4)
#'
#' t1
#' t2 <- ar_new(~gender:answer, levels=universe, values=1:4)
#' t2
#' t3 <- ar_new(~gender:answer, c(2,2), values=1:4)
#' t3
NULL

tab <- function(names, levels, values, normalize="none", smooth=0){
    normalize <- match.arg(normalize, choices=c("none","first","all"))
    names <- rhsFormula2list(names)[[1]]
    if (is.numeric( levels )){
        di <- levels
        dn <- .make.dimnames(names, levels)
    } else {
        if (is.list( levels ) ){
            vn <- names( levels )
            idx <- match( names, vn )
            if( any( (b<-is.na( idx )) ) )
                stop(sprintf("Levels for variable(s) %s not found\n", toString( names[b])))
            else {
                dn <- levels[ idx ]
                di <- unlist(lapply(dn, length), use.names=FALSE)
            }
        }
        else
            stop("Can not create 'tab' object")
    }
    
    if (missing(values))
        values <- 1
    if (smooth > 0)
        values <- values + smooth
    
    if (is.atomic(values) && !is.object(values)){
        out <- array(values, dim=di, dimnames=dn)
    }
    tabNormalize( out, normalize )
}

.make.dimnames <- function(names, levels){
    if ( !(is.atomic(names) && is.numeric(levels)) )
        stop("Can not create dimnames")

    if (length(names) != length(levels))
        stop("'names' and 'levels' must have the same length")
    
    dn <- lapply(seq_along(levels),
                  function(i){
                      1:levels[i]
                  })
    
    dn <- lapply(seq_along(levels),
                 function(i){
                     paste(names[i], dn[[i]], sep="")
                 })

    names(dn) <- names
    dn
}


## FIXME newar is used in gRain. Next gRain version will use tab in stead of newar, and then it can go from here
#' @rdname array-create
newar <- tab

#' @rdname array-create
ar_new <- tab

#' Convert dataframe to contingency table
#'
#' @rdname array-create
#' @param indata A dataframe.
#' @examples
#' ## Extract arrays from dataframe (much like xtabs() but with more flexibility)
#' data(cad1) 
#' df2xtabs(cad1, ~Sex:AngPec:AMI)
#' df2xtabs(cad1, c("Sex","AngPec","AMI"))
#' df2xtabs(cad1, c(1,2,3))

df2xtabs <- function(indata, names=NULL, normalize="none", smooth=0){

    if ( !( is.data.frame(indata) ) )
        stop("'indata' must a dataframe\n")
        
    if (!is.null( names )) {
        if (is.numeric( names )){
            if (min(names)<1 || max(names)>ncol(indata)){
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
        out <- xtabs(~., data=indata[,names, drop=FALSE])
    
    if (smooth > 0)
        out <- out + smooth

    if (normalize != "none")
        tabNormalize( out, normalize )
    else
        out
}





