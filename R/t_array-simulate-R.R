## ##############################################################
##
## Simulate n observations from the array x conditional on
## the variables in margin (a vector of indices) takes values
## given by margin.value
##
## ##############################################################

#' @title Simulate data from array.
#' 
#' @description Simulate data (slice of) an array.
#'
#' @name array-simulate
#' 
#' @param x An array
#' @param nsim Number of cases to simulate
#' @param margin,value.margin Specification of slice of array to
#'     simulate from
#' @return A matrix
#' @note The current implementation is fragile in the sense that it is
#'     not checked that the input argument \code{x} is an array.
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @keywords utilities
#' @examples
#' 
#' 
#' ## 2x2 array
#' x <- parray(c("a","b"), levels=c(2,2), values=1:4)
#' 
#' ## Simulate from entire array
#' s <-simulateArray(x,1000)
#' xtabs(~., as.data.frame(s))
#' 
#' ## Simulate from slice defined by that dimension 1 is fixed at level 2
#' s <-simulateArray(x, 6000, 1, 2)
#' xtabs(~., as.data.frame(s))
#' 
#' ## 2x2x2 array
#' x <- parray(c("a", "b", "c"), levels=c(2, 2, 2), values=1:8)
#' 
#' ## Simulate from entire array
#' s <-simulateArray(x, 36000)
#' xtabs(~., as.data.frame(s))
#' 
#' ## Simulate from slice defined by that dimension 3 is fixed at level 1
#' s <-simulateArray(x, 10000, 3, 1)
#' xtabs(~., as.data.frame(s))
#' 
#' 
#' @export simulateArray
simulateArray <- function(x, nsim=1, margin, value.margin){
    if(missing(margin)) {
        rhs       <- NULL
        lhs.dim   <- dim(x)
        lhs.names <- names(dimnames(x))
    } else {
        rhs       <- margin
        idx       <- (1:length(dim(x)))[-rhs]
        lhs.dim   <- (dim(x))[idx]
        lhs.names <- names(dimnames(x))[-rhs]
    }
    ##cat(sprintf("rhs=%s, lhs.dim=%s, lhs.names=%s\n",
    ##            toString(rhs), toString(lhs.dim), toString(lhs.names)))
    llhs.dim <- length(lhs.dim)
    ## FIXME: simulateArray uses tableSlice(); not a big issue but still
    pp   <- tableSlice(x, margin=rhs, level=value.margin)
    ##print(pp)
    samp <- sample(length(pp), size=nsim, replace=TRUE, prob=pp)
    ##print(samp)
    cp   <- cumprod(c(1, lhs.dim[-llhs.dim]))
    res  <- matrix(0, nrow=nsim, ncol=llhs.dim)
    for(j in 1:nsim){
        res[j,] <- 1 + ( (samp[j] - 1) %/% cp ) %% lhs.dim
    }
    colnames(res) <- lhs.names
    res
}


#' @rdname array-simulate
tabSim <- simulateArray

#' @rdname array-simulate
ar_sim <- simulateArray
