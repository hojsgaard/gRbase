## ###############################################################
##
## Generic functions used in gRbase, gRain, gRim
##
## ###############################################################

#' @title Compile and propagate functions
#' 
#' @description \code{compile} and \code{propagate} are generic
#'     functions which invoke particular methods which depend on the
#'     class of the first argument
#'
#' @name grbase_generics
#' 
#' @aliases compile propagate
#' @param object An object
#' @param \dots Additional arguments which depends on the class of the object
#' @return The value returned depends on the class of the first argument.
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
## #' @seealso \code{\link[gRain]{compile.grain}},
## #'     \code{\link[gRain]{propagate.grain}}
#' @references Højsgaard, Søren; Edwards, David; Lauritzen, Steffen (2012):
#'     Graphical Models with R, Springer
#' @keywords utilities

#' @rdname grbase_generics
fit <- function(object, ...)
{
  UseMethod("fit")
}

#' @rdname grbase_generics
compile <- function (object, ...)
{
    UseMethod("compile")
}

#' @rdname grbase_generics
propagate <- function (object, ...)
{
    UseMethod("propagate")
}

#' @rdname grbase_generics
stepwise <- function(object,...){
    UseMethod("stepwise")
}


#' @title Generic function for model comparison
#' 
#' @description \code{compareModels} is a generic functions which
#'     invoke particular methods which depend on the class of the
#'     first argument
#' 
#' @param object,object2 Model objects
#' @param \dots Additional arguments
#' @return The value returned depends on the class of the first
#'     argument.
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @keywords utilities
#' @export compareModels
compareModels <- function (object, object2, ...)
{
    UseMethod("compareModels")
}


#stepwise.default <- function(object,...) return(step(getFit(object)))
