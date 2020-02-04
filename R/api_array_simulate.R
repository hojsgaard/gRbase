## ##############################################################
#'
#' @title Simulate data from array.
#' @description Simulate data (slice of) an array: Simulate n
#'     observations from the array x conditional on the variables in
#'     margin (a vector of indices) takes values given by margin.value
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @name array_simulate
#'
## ##############################################################
#'
#' @param object An array.
#' @param nsim Number of cases to simulate.
#' @param margin,value.margin Specification of slice of array to
#'     simulate from.
#' @param seed Seed to be used for random number generation.
#' @param ... Additional arguments, currently not used.
#' 
#' @return A matrix.
#' 
#' @note The current implementation is fragile in the sense that it is
#'     not checked that the input argument \code{x} is an array.
#' 
#' @keywords utilities
#' @examples 
#' ## 2x2 array
#' x <- parray(c("a", "b"), levels=c(2, 2), values=1:4)
#' 
#' ## Simulate from entire array
#' s <- simulateArray(x, 1000)
#' xtabs(~., as.data.frame(s))
#' 
#' ## Simulate from slice defined by that dimension 1 is fixed at level 2
#' s <-simulateArray(x, 6000, 1, 2)
#' xtabs(~., as.data.frame(s))
#' 
#' ## 2 x 2 x 2 array
#' x <- parray(c("a", "b", "c"), levels=c(2, 2, 2), values=1:8)
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
simulateArray <- function(object, nsim=1, margin, value.margin, seed=NULL){
    if(missing(margin)) {
        rhs       <- NULL
        lhs.dim   <- dim(object)
        lhs.names <- names(dimnames(object))
    } else {
        rhs       <- margin
        idx       <- (1:length(dim(object)))[-rhs]
        lhs.dim   <- (dim(object))[idx]
        lhs.names <- names(dimnames(object))[-rhs]
    }
    ##cat(sprintf("rhs=%s, lhs.dim=%s, lhs.names=%s\n",
    ##            toString(rhs), toString(lhs.dim), toString(lhs.names)))
    llhs.dim <- length(lhs.dim)
    ## FIXME: simulateArray uses tableSlice(); not a big issue but still
    pp   <- tableSlice(object, margin=rhs, level=value.margin)
    ##print(pp)

    set.seed(seed)
    samp <- sample(length(pp), size=nsim, replace=TRUE, prob=pp)
    ##print(samp)
    cp   <- cumprod(c(1, lhs.dim[-llhs.dim]))
    res  <- matrix(0, nrow=nsim, ncol=llhs.dim)
    for(j in 1:nsim){
        res[j,] <- 1 + ((samp[j] - 1) %/% cp) %% lhs.dim
    }
    colnames(res) <- lhs.names
    res
}

#' @export
#' @rdname array_simulate
simulate.table <- function(object, nsim=1, seed=NULL, margin, value.margin, ...){
    simulateArray(object, nsim=nsim, margin=margin, value.margin=value.margin, seed=seed)
}

#' @export
#' @rdname array_simulate
simulate.xtabs  <- simulate.table

#' @export
#' @rdname array_simulate
simulate.array  <- simulate.table
