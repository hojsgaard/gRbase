## ###################################################################
##
#' @title gRbase utilities
#' @description Various utility functions for gRbase. Includes 'faster
#'     versions' of certain standard R functions.
#' @name grbase-utilities
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
##
## ###################################################################
#'
#' @aliases matrix2list rowmat2list colmat2list pairs2num
#' @param X A matrix.
#' @param byrow Should the split be by row or by column.
#' @param form Formula specification (a right-hand sided formula, a
#'     numeric/character vector or a list of vectors).
#' @param dots dot-arguments to be turned into a list 

## Turn a right-hand-sided formula into a list (anything on the left
## hand side is ignored)

#' @rdname grbase-utilities
#' @export
rhsFormula2list <- function(form){
    if (is.character(form)) return(list(form))
    if (is.numeric(form)) return(lapply(list(form), "as.character"))
    if (is.list(form)) return(lapply(form, "as.character"))

    .xxx. <- form[[ length( form ) ]]
    form1 <- unlist(strsplit(paste(deparse(.xxx.), collapse="")," *\\+ *"))
    form2 <- unlist(lapply(form1, strsplit, " *\\* *| *: *| *\\| *"),
                 recursive=FALSE)
    form2
}

#' @export
#' @rdname grbase-utilities
rhsf2list  <-  rhsFormula2list

#' @export
#' @rdname grbase-utilities
rhsf2vec   <- function(form){
    rhsf2list(form)[[1]]
}

#' @export
#' @rdname grbase-utilities
listify_dots <- function(dots){
    dots <- lapply(dots, function(a) if (!is.list(a)) list(a) else a)
    unlist(dots, recursive=FALSE)    
}


## Turn list into right-hand-sided formula
##
## July 2008
#' @export
#' @rdname grbase-utilities
list2rhsFormula <- function(form){
  if (inherits(form, "formula")) return(form)
  as.formula(paste("~",paste(unlist(lapply(form,paste, collapse='*')), collapse="+")),
             .GlobalEnv)
}

#' @export
#' @rdname grbase-utilities
list2rhsf <- list2rhsFormula

#' @export
#' @rdname grbase-utilities
rowmat2list <- rowmat2list__

#' @export
#' @rdname grbase-utilities
colmat2list <- colmat2list__

#' @export
#' @rdname grbase-utilities
matrix2list <- function(X, byrow=TRUE){
  if (byrow) rowmat2list__(X) # cpp implementation
  else colmat2list__(X) # cpp implementation
}

## FIXME: which.arr.ind: Fails on sparse matrices!!
## FIXME: -> remove after check downstram!!
## FIXME: -> which_matrix_index is Cpp implementation

#' @export
#' @rdname grbase-utilities
#' 
#' @details \code{which.arr.ind}: Returns matrix n x 2 matrix with
#'     indices of non-zero entries in matrix \code{X}. Notice
#'     \code{which_matrix_index__} is cpp implementation.
#'
#'
#' 
which.arr.index <- function(X){
  nr  <- nrow(X)
  nc  <- ncol(X)
  rr <- rep.int(1:nr, nc)
  cc <- rep(1:nc, each=nr)
  cbind(rr[X!=0L], cc[X!=0L])
}

#' @export
#' @rdname grbase-utilities
which_matrix_index <- which_matrix_index__

#' @export
#' @rdname grbase-utilities
rowSumsPrim <- function(X){
    .Call("R_rowSums", X, PACKAGE="gRbase")}

#' @export
#' @rdname grbase-utilities
colSumsPrim <- function(X){
    .Call("R_colSums", X, PACKAGE="gRbase")}
          
#' @rdname grbase-utilities
#' @param v A vector.
#' @param X A matrix.
#' @details \code{colwiseProd}: multiplies a vector v and a matrix X
#'     columnwise (as opposed to rowwise which is achieved by
#'     \code{v * X}). Hence \code{colwiseProd} does the same as
#'     \code{t(v * t(X))} - but it does so faster for numeric values.
#'
#' @examples
#' ## colwiseProd
#' X <- matrix(1:16, nrow=4)
#' v <- 1:4
#' t(v * t(X))
#' colwiseProd(v, X)
#' \dontrun{
#' system.time(for (ii in 1:100000)  t(v * t(X)))
#' system.time(for (ii in 1:100000)  colwiseProd(v, X))
#' }
#' 

#' @export
colwiseProd <- function(v, X){
    .Call("R_colwiseProd", v, X, PACKAGE="gRbase")}


#' @rdname grbase-utilities
#' @param setlist A list of atomic vectors
#' @param item An atomic vector
#' @details
#' * lapplyV2I: same as but much faster than `lapply(setlist, function(elt) match(elt, item))`
#'
#' * lapplyI2V: same as but faster than `lapply(setlist, function(elt) item[elt])`
#'
#' @examples
#'
#' setlist <- list(c(1,2,3), c(2,3,4), c(2,4,5))
#' item <- c(2,3)
#' 
#' lapplyV2I(setlist, item)
#' lapply(setlist, function(gg) match(gg, item))
#'
#' lapplyI2V(setlist, item)
#' lapply(setlist, function(x) item[x])
#'
#' if (require(microbenchmark)){
#' microbenchmark(
#'   lapplyV2I(setlist, item),
#'   lapply(setlist, function(elt) match(elt, item)))
#'
#' microbenchmark::microbenchmark(
#'   lapplyI2V(setlist, item),
#'   lapply(setlist, function(elt) item[elt]))
#' }

#' @export
lapplyV2I <- function(setlist, item){lapply(setlist, function(elt) match(elt, item))}

#' @rdname grbase-utilities
#' @export
lapplyI2V <- function (setlist, item) {lapply(setlist, function(elt) item[elt])}


## Codes a p x 2 matrix of characters or a list with pairs
## of characters into a vector of numbers.
## FIXME: pairs2num: Cpp implementation

## #' @export
## pairs2num <- function(x, vn, sort=TRUE){
##     if (is.null(x)) return(NULL)

##     if (inherits(x, "matrix")){
##         if (dim(x)[2L] != 2)
##             stop("matrix does not have two colums")
##     }
##     else if (inherits(x, "list")){        
##         if (!(all(sapply(x, length) == 2)) )
##             stop("Not all elements in x have length 2")        
##         x <- do.call(rbind, x)
##     }
##     else if (inherits(x, "character")){
##         if (length(x) != 2)
##             stop("x does not have length 2")
##         x <- matrix(x, nrow=1)    
##     }

##   ## From here x should be a p x 2 matrix

##   dd <- dim(x)
##   if (dd[1L] == 0){
##       return(numeric(0))
##   } else {
##       if (sort){
##           i     <- x[, 2L]< x[, 1L]
##           c1    <- i + 1L
##           c2    <- -1L * (i - 1L) + 1L
##           x  <- cbind(x[cbind(seq_along(c1), c1)],
##                       x[cbind(seq_along(c2), c2)])
##         }
##       ans       <- match(x, vn)
##       dim(ans)  <- dim(x)
##       colSumsPrim(t.default(ans) * c(100000, 1))
##       ## ans[,1L] <- ans[,1L] * 100000L
## ##       rowSumsPrim(ans)
##     }
## }

## OLD VERSION
## Codes a p x 2 matrix of characters or a list with pairs
## of characters into a vector of numbers.

## FIXME: pairs2num: Cpp implementation

#' @aliases pairs2num
#' @export
pairs2num <- function(x, vn, sort=TRUE){
    if (!inherits(x, "matrix")){
        if (is.null(x))
            return(NULL)
        
        if (inherits(x,"list"))
            x <- do.call(rbind,x)
        else {
            if (inherits(x,"character"))
                x <- matrix(x,nrow=1)
        }
    }
    ## From here x should be a p x 2 matrix
    
    dd <- dim(x)
    if (dd[1L] == 0){
        return(numeric(0))
    } else {
        if (sort){
            i     <- x[, 2L]< x[, 1L]
            c1    <- i+1L
            c2    <- -1L * (i - 1L) + 1L
            x  <- cbind(
                x[cbind(seq_along(c1), c1)],
                x[cbind(seq_along(c2), c2)])
        }
        ans       <- match(x, vn)
        dim(ans)  <- dim(x)
        colSumsPrim(t.default(ans) * c(100000, 1))
        ## ans[,1L] <- ans[,1L] * 100000L
        ##       rowSumsPrim(ans)
    }
}






