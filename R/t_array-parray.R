############################################################################
####
#### A named array is an array in the sense that it is a
#### vector with a dim and a dimnames attribute.
####
############################################################################

#' @title Representation of and operations on multidimensional arrays
#' 
#' @description General representation of multidimensional arrays (with named
#'     dimnames, also called named arrays.)
#' 
#' @name old-parray
#' 
#' @details A named array object represents a table defined by a set of variables and
#' their levels, together with the values of the table. E.g. f(a,b,c) can be a
#' table with a,b,c representing levels of binary variable
#' 
#' If \code{normalize="first"} then for each configuration of all other
#' variables than the first, the probabilities are normalized to sum to one.
#' Thus f(a,b,c) becomes a conditional probability table of the form p(a|b,c).
#' If \code{normalize="all"} then the sum over all entries of f(a,b,c) is one.
#' 
#' If \code{smooth} is positive then \code{smooth} is added to \code{values}
#' before normalization takes place.
#' 
#' @aliases parray as.parray data2parray
#' @param varNames Names of variables defining table; can be a right hand sided
#'     formula.
#' @param levels Either 1) a vector with number of levels of the factors in
#'     varNames or 2) a list with specification of the levels of the factors in
#'     varNames. See 'examples' below.
#' @param values Values to go into the array
#' @param normalize Either "none", "first" or "all". Should result be
#'     normalized, see 'Details' below.
#' @param smooth Should values be smoothed, see 'Details' below.
#' @return A a named array.
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @seealso \code{\link{is.named.array}}, \code{\link{ar_marg}}
#' @keywords utilities
#' @examples
#'  
#' t1 <- parray(c("gender","answer"), list(c('male','female'),c('yes','no')), values=1:4)
#' t1 <- parray(~gender:answer, list(c('male','female'),c('yes','no')), values=1:4)
#' t1 <- parray(~gender:answer, c(2,2), values=1:4)
#' 
#' t2 <- parray(c("answer","category"), list(c('yes','no'),c(1,2)), values=1:4+10)
#' t3 <- parray(c("category","foo"), c(2,2), values=1:4+100)
#' 
#' varNames(t1)
#' nLevels(t1)
#' valueLabels(t1)
#' 
#' ## Create 1-dimensional vector with dim and dimnames
#' x1 <- 1:5
#' as.parray(x1)
#' x2 <- parray("x", levels=length(x1), values=x1)
#' dim(x2)
#' dimnames(x2)
#' 
#' ## Matrix
#' x1 <- matrix(1:6, nrow=2)
#' as.parray(x1)
#' parray(~a:b, levels=dim(x1), values=x1)
#' 
#' ## Extract parrays from data
#' ## 1) a dataframe
#' data(cad1) 
#' data2parray(cad1, ~Sex:AngPec:AMI)
#' data2parray(cad1, c("Sex","AngPec","AMI"))
#' data2parray(cad1, c(1,2,3))
#' ## 2) a table
#' data2parray(UCBAdmissions,c(1,2), normalize="first")
#' 
#' @export parray
parray <- function(varNames, levels, values=1, normalize="none", smooth=0){

  normalize <- match.arg(normalize, choices=c("none","first","all"))
  varNames  <- rhsFormula2list(varNames)[[1]]
  if (smooth>0){
    values <- values + smooth
  }

  dn   <- makeDimNames(varNames, levels)
  nlev <- unlist(lapply(dn, length))
  ans  <- array(values, dim=nlev, dimnames=dn)

  ## Normalize if requested
  switch(normalize,
         "first" = {
           ##cat("first\n")
           if (length(nlev)>1){
             tmp   <- matrix(ans, ncol=dim(ans)[1], byrow=TRUE)
             ans[] <- t.default(tmp/rowSumsPrim(tmp))
           } else {
             ans <- ans / sum(ans)
           }},
         "all"  = {
           ans <- ans / sum(ans)
         },
         "none" = { } )
  class(ans) <- c("parray","array")
  return(ans)
}


#' @rdname old-parray
as.parray  <- function(values, normalize="none", smooth=0){

  normalize <- match.arg(normalize, choices=c("none","first","all"))

  if (!inherits(values, c("array","matrix","integer","double","table"))){
    stop("arg must be array, matrix, table, integer or double\n")
  }

  if (smooth>0){
    values <- values + smooth
  }

  if (is.null(dimnames(values))){
    if (!is.null(dim(values)))
      nLevels <- dim(values)
    else
      nLevels <- length(values)

    varNames <- paste("V", 1:length(nLevels),sep='')
    dimnames <- makeDimNames(varNames, nLevels)
    ans <- array(values, dim = nLevels, dimnames = dimnames)
  } else {
    ans <- values
  }
    ##class(ans) <- c("parray","array")

  switch(normalize,
    "first"={
      if (length(dim(ans))>1){
        marg  <- 2:length(dim(ans))
        ma    <- apply(ans, marg, sum)
        ans   <- sweep(ans, marg, ma, "/")
      } else {
        ans <- ans / sum(ans)
      }
    },
    "all"={ans <- ans / sum(ans)
    },
    "none"={}
    )
  attr(ans, "call") <- NULL
  return(ans)
}

data2parray <- function(data, varNames=NULL, normalize="none", smooth=0){
  cls <- match(class(data), c("data.frame","table", "xtabs", "matrix"))[1]
  if (is.na(cls)){
    stop("'data' must be one of  dataframe, table, xtabs, matrix")
  }

  .set.varNames <- function(varNames, dataNames){
    if (is.null(varNames)){
      if (is.null(dataNames))
        stop("'data' has no variable names")
      varNames <- dataNames
    } else {
      if (class(varNames) %in% c("formula", "character")){
        varNames <- rhsf2list(varNames)[[1]]
      }
    }
    varNames
  }

  switch(as.character(cls),
         "1"={
           dataNames <- names(data)
           varNames <- .set.varNames(varNames, dataNames)
           val  <- xtabs(~., data = data[, varNames, drop = FALSE])
         },
         "2"=, "3"=, "4"={
           dataNames <- names(dimnames(data))
           varNames <- .set.varNames(varNames, dataNames)
           val  <- tableMargin(data, varNames)
         }
         )
  attr(val, "call") <- NULL
  res <- as.parray(val, normalize = normalize, smooth = smooth)
  res
}



makeDimNames <- function(varNames, levels, sep=''){
    if (!is.character(varNames))
        stop("'varNames' is not character vector")
    if (missing(levels))
        stop("'levels' is missing")
    if (! (is.list(levels) || is.numeric(levels)) )
        stop("'levels' must be a list or a numeric vector")
    
    if ( is.list(levels) ) {
        if ( is.null(names(levels)) ){
            if (length(varNames) != length(levels))
                stop("kkk")
            names(levels) <- varNames
        } else {        
            n <- match(varNames, names(levels))
            if (any(is.na(n)))
                stop("ppp")
            levels <- levels[n]
        }
    } else {
        levels <- .make_vn( varNames, levels ) # Levels is numeric vector
    }    
    levels
}


.make_vn <- function(varNames, levels){
    if (!is.character(varNames))
        stop("'varNames' must be character")
    if (!is.numeric(levels))
        stop("'levels' must be numeric")
    if (length(varNames) != length(levels))
        stop("'varNames' and 'levels' does not have the same length")
    out <- lapply(seq_along(varNames), function(i)
    {
        ll <- levels[[ i ]]
        vv <- varNames[ i ]
        lev <- paste(vv, 1:ll, sep="")
        lev
    })
    names(out) <- varNames
    out
}




## makeDimNames <- function(varNames, levels, sep=''){
##   if (missing(varNames) || is.null(varNames))
##     return(lapply(levels, seq))
##   mapply(function(vv,ll){
##     if (!is.character(ll)){
##       if (length(ll)==1){
##         ll <- 1:ll
##       }
##       ll <- paste(vv,ll,sep="")
##     }
##     ll
##   }, varNames, levels, SIMPLIFY=FALSE)
## }
## 
## makeDimNames <- function(varNames, levels, sep=''){
##     if (missing(varNames) || is.null(varNames))
##         return(lapply(levels, seq))
##     if (length(varNames) != length(levels))
##         stop("'varNames' and 'levels' must have the same length")
##     if (is.list(levels)){
##         names(levels) <- varNames
##         return( levels )
##     }
##     out <- lapply(seq_along(varNames), function(i)
##     {
##                       ll <- levels[[ i ]]
##                       vv <- varNames[ i ]
##                       if (!is.character(ll)){
##                           if (length(ll)==1){
##                               ll <- 1:ll
##                           }
##                           ll <- paste(vv,ll,sep="")
##                       }
##                       ll
##                   })
##     names(out) <- varNames
##     out
## }
##


## ptable <- function(varNames, levels, values=1, normalize=c("none","first","all"), smooth=0){

##   normalize <- match.arg(normalize, choices=c("none","first","all"))
##   varNames  <- rhsFormula2list(varNames)[[1]]

##   if (is.list(levels)){
##     dimnames        <- levels
##     names(dimnames) <- varNames
##     levels          <- sapply(dimnames, length)
##   } else {
##     dimnames <- makeDimNames(varNames, levels)
##   }

##   if (smooth>0){
##     values <- values + smooth
##   }

##   ans <- array(values, dim=levels, dimnames=dimnames)

##   ## Normalize if requested
##   switch(normalize,
##          "first" = {
##            if (length(dim(ans))>1){
##              marg  <- 2:length(dim(ans))
##              ma2   <- tableMargin(ans, marg)
##              ans   <- tablePerm(.tableOp2(ans, ma2, op=`/`), names(dimnames(ans)))
##            } else {
##              ans <- ans / sum(ans)
##            }},
##          "all"  = {ans <- ans / sum(ans) },
##          "none" = { } )
##   class(ans) <- "ptable"
##   return(ans)
## }

## ## Create list with dimension names
## ##
## makeDimNames <- function(varNames, levels, sep=''){
##   if (missing(varNames) || is.null(varNames))
##     return(lapply(levels, seq))
##   lev <- lapply(levels, function(a) c(1:a))
##   mapply(function(n,v) paste(n,v,sep=sep), varNames, lev, SIMPLIFY=FALSE)
## }
## Accessors
##












