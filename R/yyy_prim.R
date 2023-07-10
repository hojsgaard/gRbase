#' @name internal
#' 
#' @aliases setdiffPrim unlistPrim intersectPrim outerPrim uniquePrim matchPrim
NULL

#' @export
setdiffPrim <- function (x, y) {
    unique.default(
        if (length(x) || length(y))
            x[match(x, y, 0L) == 0L]
        else x
    )
}

#' @export
unlistPrim <- function(x) {    #OK
  unlist(x, use.names=FALSE)
}

#' @export
intersectPrim <- function (x, y) {
  unique.default(y[match(x, y, 0L)])
}

#' @export
outerPrim <- function(X,Y) {
  nX  <- length(X)
  nY  <- length(Y)
  Y   <- rep(Y, rep.int(length(X), length(Y)))
  X   <- rep(X, times = ceiling(length(Y)/length(X)))
  ans <-X*Y
  dim(ans)<-c(nX,nY)
  ans
}

#' @export
uniquePrim <- function(x) {    #OK
  unique.default(x)
}

#' @export
matchPrim<-function(x,table) { # Never used
  match(x, table)
}
