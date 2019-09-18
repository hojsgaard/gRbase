
#' @rdname internal
uniquePrim <- function(x){    #OK
  unique.default(x)
}

#' @rdname internal
unlistPrim <- function(l){    #OK
  unlist(l, use.names=FALSE)
}

#' @rdname internal
setdiffPrim <- function (x, y){
    unique.default(
        if (length(x) || length(y))
            x[match(x, y, 0L) == 0L]
        else x
    )
}

#' @rdname internal
intersectPrim <- function (x, y){
  unique.default(y[match(x, y, 0L)])
}

#' @rdname internal
outerPrim <- function(X,Y){
  nX  <- length(X)
  nY  <- length(Y)
  Y   <- rep(Y, rep.int(length(X), length(Y)))
  X   <- rep(X, times = ceiling(length(Y)/length(X)))
  ans <-X*Y
  dim(ans)<-c(nX,nY)
  ans
}

#' @rdname internal
matchPrim<-function(x,table){ # Never used
  match(x, table)
}
