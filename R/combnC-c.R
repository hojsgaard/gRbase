#' @title Generate All Combinations of n Elements Taken m at a Time
#' 
#' @description Generate all combinations of the elements of x taken m
#'     at a time.  If x is a positive integer, returns all
#'     combinations of the elements of seq(x) taken m at a time.
#' 
#' @param x vector source for combinations, or integer n for x <-
#'     seq(n).
#' @param m number of elements to choose.
#' @param simplify logical indicating if the result should be
#'     simplified to a matrix; if FALSE, the function returns a list.
#' @return A matrix or a list.
#' @note The combn_prim function is a simplified version of the combn
#'     function.  However, combn_prim is implemented in C and is
#'     considerably faster than combn.
#' @author P. T. Wallace and Søren Højsgaard
#' @seealso \code{\link{combn}}
#' @keywords utilities
#' @examples
#' 
#' x <- letters[1:5]
#' m <- 3
#' 
#' combn(x, m)
#' combn_prim(x, m)
#' 
#' combn(m, m)
#' combn_prim(m, m)
#' 
#' combn(x, m, simplify=FALSE)
#' combn_prim(x, m, simplify=FALSE)
#'
#' x <- letters[1:20]
#'
#' \dontrun{
#' system.time({ for (i in 1:100) { combn_prim(x,m) }})
#' system.time({ for (i in 1:100) { combn(x,m) }})
#' 
#' system.time({ for (i in 1:100) { combn_prim(x,m, simplify=FALSE) }})
#' system.time({ for (i in 1:100) { combn(x,m, simplify=FALSE) }})
#' }
#' 
#' @export combn_prim
#' 

combn_prim <- function(x, m, simplify=TRUE){
    ## FIXME: combn_prim: Could take a FUN argument.
    if (length(x)==1 && is.numeric(x))
        x <- seq(x)
    if (length(x) < m)
        stop("Error in combn_prim: n < m\n")
    
    ##   nofun <- is.null(FUN)
    ##   if (!nofun && !is.function(FUN))
    ##     stop("'FUN' must be a function or NULL")
    
    NCAND <- length(x)
    NSEL  <- as.integer(m)
    NSET <- as.integer(choose(NCAND,NSEL))
    ANS  <- rep.int(0L, NSET*NSEL)
    res <- .C("combnC", NSEL, NCAND, NSET, ANS
             ,PACKAGE="gRbase"
              )[[4]]
    
    if (simplify){
        matrix(x[res], nrow=NSEL, ncol=NSET)
    } else {
        res <- matrix(x[res], nrow=NSEL, ncol=NSET)
        ##res <- split(res, col(res))
        res <- colmat2list( res )
        names(res) <- NULL
        res
    }
}
