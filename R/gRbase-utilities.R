
.dgCMatrix <- function(data=NA, nrow=1, ncol=1, byrow=FALSE, dimnames=NULL,
                      sparse = TRUE, doDiag = TRUE, forceCheck = FALSE){
  as(Matrix(data=data, nrow=nrow, ncol=ncol, dimnames=dimnames, sparse=TRUE), "dgCMatrix")
}


### rowmat2list and colmat2list:
### ----------------------------
## Turns a matrix into a list, either by row or by column.
## Notice: finding unique rows in a matrix can be speeded up this way.

matrix2list <- function(x, byrow=TRUE){
  if (byrow)
    rowmat2list(x) # cpp implementation
  else
    colmat2list(x) # cpp implementation
}

## Returns matrix n x 2 matrix with indices of non-zero
## entries in matrix m
## FIXME: which.arr.ind: Fails on sparse matrices!!
## FIXME: -> remove after check downstram!!
## FIXME: -> which_matrix_index is Cpp implementation
which.arr.ind<-function(m){
  nr  <- nrow(m)
  nc  <- ncol(m)
  rr <- rep.int(1:nr, nc)
  cc <- rep(1:nc, each=nr)
  cbind(rr[m!=0L], cc[m!=0L])
}


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
  if (class(x)!="matrix"){
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





####################################################
####
#### Create all possible pairs from a vector
#### or all possible pairs combining one element
#### from each of two vectors
####
#### NOTICE: If y is not NULL then x and y must be disjoint
####
####################################################

## FIXME: names2pairs: Cpp implementation (code is in mail somewhere)

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
      rowmat2list(out)
  }
}

