#' @title gRbase utilities
#' @description Various utility functions for gRbase. Includes 'faster
#'     versions' of certain standard R functions.
#'
#' @name gRbase-utilities
#' 
#' @aliases matrix2list rowmat2list colmat2list
 
### rowmat2list and colmat2list:
### ----------------------------
## Turns a matrix into a list, either by row or by column.
## Notice: finding unique rows in a matrix can be speeded up this way.

#' @rdname gRbase-utilities
rowmat2list <- rowmat2list__

#' @rdname gRbase-utilities
colmat2list <- colmat2list__

#' @rdname gRbase-utilities
#' @param XX_ A matrix.
#' @param byrow Should the split be by row or by column.
matrix2list <- function(XX_, byrow=TRUE){
  if (byrow) rowmat2list__(XX_) # cpp implementation
  else colmat2list__(XX_) # cpp implementation
}

##  FIXME: which.arr.ind: Fails on sparse matrices!!  FIXME: -> remove
## after check downstram!!  FIXME: -> which_matrix_index is Cpp
## implementation

#' @rdname gRbase-utilities
#' @details Returns matrix n x 2 matrix with indices of non-zero
#'     entries in matrix \code{XX_}. Notice
#'     \code{which_matrix_index__} is cpp implementation.
#' 
which.arr.ind <- function(XX_){
  nr  <- nrow(XX_)
  nc  <- ncol(XX_)
  rr <- rep.int(1:nr, nc)
  cc <- rep(1:nc, each=nr)
  cbind(rr[XX_!=0L], cc[XX_!=0L])
}

#' @rdname gRbase-utilities
which_matrix_index <- which_matrix_index__


#' @rdname gRbase-utilities
rowSumsPrim <- function(XX_){
	.Call("R_rowSums", XX_
	,PACKAGE="gRbase"
	)
}

#' @rdname gRbase-utilities
colSumsPrim <- function(XX_){
	.Call("R_colSums", XX_
	,PACKAGE="gRbase"
	)
}

#' @rdname gRbase-utilities
#' @param v A vector.
#' @param M A matrix.
#' @details \code{colwiseProd} multiplies a vector v and a matrix M
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
#' system.time(for (ii in 1:100000)  t(v * t(M)))
#' system.time(for (ii in 1:100000)  colwiseProd(v, M))
colwiseProd <- function(v, M){
	.Call("R_colwiseProd", v, M
	,PACKAGE="gRbase"
	)
}


#' @rdname gRbase-utilities
#' @param set,set2 Character vectors
#' @param setlist List of charactervectors
#' @param all Logical. 
get_subset <- get_subset__

#' @rdname gRbase-utilities
get_superset <- get_superset__

#' @rdname gRbase-utilities
is_subsetof <- is_subsetof__


## ###################################################################
##
#' @title Create all possible pairs
#'
#' @description Create all possible pairs of two character vectors.
#'
#' @name all-pairs
#'
## ###################################################################
#' @param x,y Character vectors.
#' @param sort Logical.
#' @param result A list or a matrix.
#'
#' @details NOTICE: If y is not NULL then x and y must be disjoint (no
#'     checks are made); otherwise pairs of identical elements wil also be obtained. 
#'
#' @examples
#'
#' x <- letters[1:4]
#' y <- letters[5:7]
#'
#' all_pairs(x)
#' all_pairs(x, result="matrix")
#'
#' all_pairs(x, y)
#' all_pairs(x, y, result="matrix")
#' 
#' @rdname all-pairs
all_pairs <- all_pairs__


## FIXME names2pairs should be deprecated and replaced by all_pairs
#' @rdname all-pairs
names2pairs <- function(x, y=NULL, sort=TRUE, result="list"){
  result <- match.arg(result, c("list", "matrix"))
  lenx <- length(x)
  leny <- length(y)

  if (leny == 0){
    if (lenx == 1){
      if (result == "matrix")
        return(matrix(nrow=0, ncol=2))
      else
        return(list())
    } else {
      cc   <- combnPrim(1:length(x), 2)
      out  <- x[cc]
      dim(out) <- dim(cc)
      if (sort){
        idx <- out[1,] > out[2, ]
        out[1:2,idx] <- out[2:1, idx]
      }
      if (result == "matrix")
        return(t.default(out))
      else
        return(colmat2list(out))
    }
  } else {
    out <- cbind(rep(x, each=leny), rep(y, times=lenx))
    if (sort){
      idx <- out[,1] > out[,2]
      out[idx, 1:2] <- out[idx, 2:1]
    }
    if (result == "matrix")
      return(out) 
    else 
      rowmat2list__(out)
  }
}


## ###################################################################
#' @title Create all subsets
#' @description Create all subsets of a vector
#' @name all-subsets
##
## Issues: deprecate allSubsets; use all_subsets instead
## ###################################################################
#' @param x Vector
#' 
#' @rdname all-subsets
all_subsets <- allSubsets__

#' @rdname all-subsets
all_subsets0 <- allSubsets0__

#' @rdname all-subsets
#' @param g.sep Pick a value which is not in x
allSubsets <- function(x, g.sep="+"){
  if (length(x)==1)
    return(x)
  else {
    val <- x[1]
    for (i in 2:length(x)){
      v <- paste(val,x[i],sep=g.sep)
      val <- c(val,x[i],v)
    }
    val <- strsplit(val,paste("\\",g.sep,sep=""))
    return(val)
  }
}











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

##
## Calculate logL for N(0,\Sigma) model.
##
## Sigma = Covariance matrix parameter
## K     = Sigma inverse
## S     = sample covariance matrix
## n     = sample size
##
ell <- function(Sigma, S, n){

  shdet <- function(Sigma){
    prod(eigen(Sigma)[[1]])
  }
  p <- dim(S)[1]
  const <- -n*p/2*log(2*pi)
  return(const-n/2*log(shdet(Sigma))
         -n/2*sum(diag( solve(Sigma)%*%S )) )
}

ellK <- function (K, S, n)
{
    value <- (n/2) * (log(det(K)) - sum(rowSums(K * S)))
    return(value)
}






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
                                        # From here x should be a p x 2 matrix
    
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






