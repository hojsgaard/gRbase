#' @name internal
#' 
#' @aliases setdiffPrim unlistPrim intersectPrim outerPrim uniquePrim matchPrim
NULL


setdiffPrim <- function (x, y){
    unique.default(
        if (length(x) || length(y))
            x[match(x, y, 0L) == 0L]
        else x
    )
}

unlistPrim <- function(x){    #OK
  unlist(x, use.names=FALSE)
}

intersectPrim <- function (x, y){
  unique.default(y[match(x, y, 0L)])
}

outerPrim <- function(X,Y){
  nX  <- length(X)
  nY  <- length(Y)
  Y   <- rep(Y, rep.int(length(X), length(Y)))
  X   <- rep(X, times = ceiling(length(Y)/length(X)))
  ans <-X*Y
  dim(ans)<-c(nX,nY)
  ans
}


uniquePrim <- function(x){    #OK
  unique.default(x)
}


matchPrim<-function(x,table){ # Never used
  match(x, table)
}
