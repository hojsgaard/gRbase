## ################################################
##
#' @title Array algebra
#'
#' @description Addition, subtraction etc. of arrays
#'
#' @name array-algebra
#' 
## ###############################################
#' 
#' @param a,a1,a2,... Arrays (with named dimnames)
#' 
#' @aliases %a+% %a-% %a*% %a/% tabAdd tabSubt tabMult tabDiv tabDiv0
#'
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @examples
#' hec <- HairEyeColor
#' a1 <- armarg(hec, c("Hair", "Eye"))
#' a2 <- armarg(hec, c("Hair", "Sex"))
#' a3 <- armarg(hec, c("Eye", "Sex"))
#'
#' ## Binary operations
#' a1 %a+% a2
#' a1 %a-% a2
#' a1 %a*% a2
#' a1 %a/% a2
#' 
#' arsum(a1, a2, a3)
#' arprod(a1, a2, a3)
#' 



#' @rdname array-algebra
"%a+%" <- function(a1, a2){tabAdd(a1,a2)}
#' @rdname array-algebra
"%a-%" <- function(a1, a2){tabSubt(a1,a2)}
#' @rdname array-algebra
"%a*%" <- function(a1, a2){tabMult(a1,a2)}
#' @rdname array-algebra
"%a/%" <- function(a1, a2){tabDiv(a1,a2)}

#' @rdname array-algebra
aradd <- function(a1, a2){ tabAdd(a1, a2) }
#' @rdname array-algebra
arsubt <- function(a1, a2){ tabSubt(a1, a2) }
#' @rdname array-algebra
armult <- function(a1, a2){ tabMult(a1, a2) }
#' @rdname array-algebra
ardiv <- function(a1, a2){ tabDiv(a1, a2) }
#' @rdname array-algebra
ardiv0 <- function(a1, a2){ tabDiv0(a1, a2) }

#' @rdname array-algebra
arsum <- function(...){
    args <- list(...)
    ##message("args:"); print(args); message("-------")
    if (length(args)==0) 0
    else if (length(args)==1 && is.array(args[[1]])) args[[1]]
    else tabListAdd__( args )
}

#' @rdname array-algebra
arprod <- function(...){
    args <- list(...)
    ##message("args:"); print(args); message("-------")
    if (length(args)==0) 1
    else if (length(args)==1 && is.array(args[[1]])) args[[1]]
    else tabListMult__( args )
}

## #' @rdname array-algebra
## arAdd <- function(a1, a2){ tabAdd(a1, a2) }
## #' @rdname array-algebra
## arSubt <- function(a1, a2){ tabSubt(a1, a2) }
## #' @rdname array-algebra
## arMult <- function(a1, a2){ tabMult(a1, a2) }
## #' @rdname array-algebra
## arDiv <- function(a1, a2){ tabDiv(a1, a2) }
## #' @rdname array-algebra
## arDiv0 <- function(a1, a2){ tabDiv0(a1, a2) }
## 



