#' @title Partial correlation (matrix)
#' 
#' @description \code{cov2pcor} calculates the partial correlation
#'     matrix from an (empirical) covariance matrix while
#'     \code{conc2pcor} calculates the partial correlation matrix from
#'     a concentration matrix (inverse covariance matrix).
#' 
#' @name cov2pcor
#' 
#' @aliases cov2pcor conc2pcor
#' @param V Covariance matrix
#' @param K Concentration matrix
#' @return A matrix with the same dimension as V.
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @keywords utilities
#' @examples
#' 
#' data(math)
#' S <- cov.wt(math)$cov
#' cov2pcor(S)
#' 
#' @export cov2pcor
cov2pcor <- function(V){
  ans <- -cov2cor(solve(V))
  diag(ans) <- -diag(ans)
  ans
  }

#' @export
#' @rdname cov2pcor
conc2pcor <- function(K){
  ans <- -cov2cor(K)
  diag(ans)<-1
  ans
}
