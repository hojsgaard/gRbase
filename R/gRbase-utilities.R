## ###################################################################
##
#' @title gRbase utilities
#' @description Various utility functions for gRbase. Includes 'faster
#'     versions' of certain standard R functions.
#' @name gRbase_utilities
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
##
## ###################################################################
#'
#' @aliases matrix2list rowmat2list colmat2list
#' @param XX_ A matrix.
#' @param byrow Should the split be by row or by column.

#' @rdname gRbase_utilities
rowmat2list <- rowmat2list__

#' @rdname gRbase_utilities
colmat2list <- colmat2list__

#' @rdname gRbase_utilities
matrix2list <- function(XX_, byrow=TRUE){
  if (byrow) rowmat2list__(XX_) # cpp implementation
  else colmat2list__(XX_) # cpp implementation
}

## FIXME: which.arr.ind: Fails on sparse matrices!!
## FIXME: -> remove after check downstram!!
## FIXME: -> which_matrix_index is Cpp implementation

#' @rdname gRbase_utilities
#' 
#' @details \code{which.arr.ind}: Returns matrix n x 2 matrix with
#'     indices of non-zero entries in matrix \code{XX_}. Notice
#'     \code{which_matrix_index__} is cpp implementation.
#' 
which.arr.ind <- function(XX_){
  nr  <- nrow(XX_)
  nc  <- ncol(XX_)
  rr <- rep.int(1:nr, nc)
  cc <- rep(1:nc, each=nr)
  cbind(rr[XX_!=0L], cc[XX_!=0L])
}

#' @rdname gRbase_utilities
which_matrix_index <- which_matrix_index__


#' @rdname gRbase_utilities
rowSumsPrim <- function(XX_){
    .Call("R_rowSums", XX_, PACKAGE="gRbase")}

#' @rdname gRbase_utilities
colSumsPrim <- function(XX_){
    .Call("R_colSums", XX_, PACKAGE="gRbase")}
          
#' @rdname gRbase_utilities
#' @param v A vector.
#' @param M A matrix.
#' @details \code{colwiseProd}: multiplies a vector v and a matrix M
#'     columnwise (as opposed to rowwise which is achieved by
#'     \code{v * M}). Hence \code{colwiseProd} does the same as
#'     \code{t(v * t(M))} - but it does so faster for numeric values.
#'
#' @examples
#' ## colwiseProd
#' M <- matrix(1:16, nrow=4)
#' v <- 1:4
#' t(v * t(M))
#' colwiseProd(v, M)
#' \dontrun{
#' system.time(for (ii in 1:100000)  t(v * t(M)))
#' system.time(for (ii in 1:100000)  colwiseProd(v, M))
#' }
#' 
colwiseProd <- function(v, M){
    .Call("R_colwiseProd", v, M, PACKAGE="gRbase")}


## .dgCMatrix <- function(data=NA, nrow=1, ncol=1, byrow=FALSE, dimnames=NULL,
##                       sparse = TRUE, doDiag = TRUE, forceCheck = FALSE){
##   as(Matrix(data=data, nrow=nrow, ncol=ncol, dimnames=dimnames, sparse=TRUE), "dgCMatrix")
## }


## lapplyMatch: same as but much faster than
## lapply(xlist, function(gg) match(gg, set))
##
lapplyV2I <- lapplyMatch <- function(xlist, set){lapply(xlist, function(gg) match(gg, set))}

## lapplyI2C: same as but faster than
## lapply(xlist, function(x) set[x])
lapplyI2V <- function (xlist, set) {lapply(xlist, function(xx) set[xx])}

## Codes a p x 2 matrix of characters or a list with pairs
## of characters into a vector of numbers.

## FIXME: pairs2num: Cpp implementation
pairs2num <- function(x, vn, sort=TRUE){
    if (is.null(x)) return(NULL)

    if (inherits(x, "matrix")){
        if (dim(x)[2L] != 2)
            stop("matrix does not have two colums")
    }
    else if (inherits(x, "list")){        
        if (!(all(sapply(x, length) == 2)) )
            stop("Not all elements in x have length 2")        
        x <- do.call(rbind, x)
    }
    else if (inherits(x, "character")){
        if (length(x) != 2)
            stop("x does not have length 2")
        x <- matrix(x, nrow=1)    
    }

  ## From here x should be a p x 2 matrix

  dd <- dim(x)
  if (dd[1L] == 0){
      return(numeric(0))
  } else {
      if (sort){
          i     <- x[, 2L]< x[, 1L]
          c1    <- i + 1L
          c2    <- -1L * (i - 1L) + 1L
          x  <- cbind(x[cbind(seq_along(c1), c1)],
                      x[cbind(seq_along(c2), c2)])
        }
      ans       <- match(x, vn)
      dim(ans)  <- dim(x)
      colSumsPrim(t.default(ans) * c(100000, 1))
      ## ans[,1L] <- ans[,1L] * 100000L
##       rowSumsPrim(ans)
    }
}




## OLD VERSION
## Codes a p x 2 matrix of characters or a list with pairs
## of characters into a vector of numbers.

## FIXME: pairs2num: Cpp implementation
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
    if (dd[1L]==0){
        return(numeric(0))
    } else {
        if (sort){
            i     <- x[,2L]< x[,1L]
            c1    <- i+1L
            c2    <- -1L*(i-1L) + 1L
            x  <- cbind(
                x[cbind(seq_along(c1),c1)],
                x[cbind(seq_along(c2),c2)])
        }
        ans       <- match(x,vn)
        dim(ans)  <- dim(x)
        colSumsPrim(t.default(ans) * c(100000,1))
        ## ans[,1L] <- ans[,1L] * 100000L
        ##       rowSumsPrim(ans)
    }
}






