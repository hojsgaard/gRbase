#' @title Generate All Combinations of n Elements Taken m at a Time
#' 
#' @description Generate all combinations of the elements of x taken m
#'     at a time.  If x is a positive integer, returns all
#'     combinations of the elements of seq(x) taken m at a time.
#'
#' @name fastcombn
#' 
#' @param x vector source for combinations, or integer n for x <-
#'     seq(n).
#' @param m number of elements to choose.
#'
#' @param FUN function to be applied to each combination; default ‘NULL’
#'          means the identity, i.e., to return the combination (vector
#'          of length ‘m’).
#' 
#' @param simplify logical indicating if the result should be
#'     simplified to a matrix; if FALSE, the function returns a list.
#'
#' @param ... Further arguments passed on to `FUN`.
#' 
#' @return A matrix or a list.
#'
#' @details
#' *  Factors `x` are accepted.
#' 
#' * `combn_prim` is a simplified (but faster) version of the `combn`
#'    function. Does nok take the `FUN` argument.
#'
#' * `fastcombn` is intended to be a faster version of the `combn`
#'    function.
#' 
#' @author Søren Højsgaard
#' @seealso \code{\link{combn}}
#' @keywords utilities
#' @examples
#' 
#' x <- letters[1:5]; m <- 3
#'
#' fastcombn(x, m)
#' combn(x, m)
#' combn_prim(x, m)
#'
#' x <- letters[1:4]; m <- 3
#' fastcombn(x, m, simplify=FALSE)
#' combn(x, m, simplify=FALSE)
#' combn_prim(x, m, simplify=FALSE)
#' 
#' x <- 1:10; m <- 3
#' fastcombn(x, m, min)
#' combn(x, m, min)
#' 
#' x <- factor(letters[1:8]); m <- 5
#'
#' if (require(microbenchmark)){
#'   microbenchmark(
#'     combn(x, m, simplify=FALSE),
#'     combn_prim(x, m, simplify=FALSE),
#'     fastcombn(x, m, simplify=FALSE),
#'     times=50
#'   )
#' }
#' 

#' @export
#' @rdname fastcombn
fastcombn <- function(x, m, FUN=NULL, simplify=TRUE, ...){
    if (is.null(FUN))
        combn_prim(x, m, simplify=simplify)
    else {
        comb <- combn_prim(x, m, simplify=FALSE)
        if (simplify) sapply(comb, FUN, ...)
        else lapply(comb, FUN, ...)
    }
}

#' @export
#' @rdname fastcombn
combn_prim <- function(x, m, simplify=TRUE){
    if (length(x)==1 && is.numeric(x))
        x <- seq_len(x)
    if (length(x) < m)
        stop("Error in combn_prim: n < m\n")

    x0 <- x
    NCAND <- length(x)
    NSEL  <- as.integer(m)
    NSET  <- as.integer(choose(NCAND, NSEL))

    #res <- do_combn(NCAND, NSEL)
    res <- x[do_combn(NCAND, NSEL)]

    if (is.factor(x0)) {
        levels(res) <- levels(x0)
        class(res) <- class(x0)
    }

    dim(res) <- c(NSEL, NSET)
    
    if (!simplify){
        res <- split(res, col(res))
        names(res) <- NULL
    }
    res
}


## ## USES OLD C-CODE FOUND ON THE WEB WHILE IN BRISTOL
## #' @export
## #' @rdname fastcombn
## combn_prim_old <- function(x, m, simplify=TRUE){
##     if (length(x)==1 && is.numeric(x))
##         x <- seq(x)
##     if (length(x) < m)
##         stop("Error in combn_prim: n < m\n")
    
##     NCAND <- length(x)
##     NSEL  <- as.integer(m)
##     NSET <- as.integer(choose(NCAND,NSEL))
##     ANS  <- rep.int(0L, NSET*NSEL)
##     res <- .C("combnC", NSEL, NCAND, NSET, ANS
##              ,PACKAGE="gRbase")[[4]]
    
##     if (simplify){
##         matrix(x[res], nrow=NSEL, ncol=NSET)
##     } else {
##         res <- matrix(x[res], nrow=NSEL, ncol=NSET)
##         ##res <- split(res, col(res))
##         res <- colmat2list( res )
##         names(res) <- NULL
##         res
##     }
## }
