#' Milk composition data
#' 
#' Data from an experiment on composition of sow milk. Milk composition is
#' measured on four occasions during lactation on a number of sows. The
#' treatments are different types of fat added to the sows feed.
#' 
#' \code{a} is the control, i.e. no fat has been added.
#' 
#' \code{fat} + \code{protein} + \code{lactose} almost add up to \code{dm} (dry
#' matter)
#' 
#' @name milkcomp
#' @aliases milkcomp milkcomp1
#' @docType data
#' 
#' @format A data frame with 214 observations on the following 7 variables.
#' \describe{
#'   \item{\code{sow}}{a numeric vector}
#'   \item{\code{lactime}}{a numeric vector}
#'   \item{\code{treat}}{a factor with levels \code{a} \code{b} \code{c} \code{d} \code{e} \code{f} \code{g}}
#'   \item{\code{fat}}{a numeric vector}
#'   \item{\code{protein}}{a numeric vector}
#'   \item{\code{dm}}{(dry matter) a numeric vector}
#'   \item{\code{lactose}}{a numeric vector}
#' }
#' 
#' @references Charlotte Lauridsen and Viggo Danielsen (2004):
#'     Lactational dietary fat levels and sources influence milk
#'     composition and performance of sows and their progeny Livestock
#'     Production Science 91 (2004) 95-105
#' @keywords datasets
#' @examples
#' 
#' data(milkcomp)
#' ## maybe str(milk) ; plot(milk) ...
#' 
"milkcomp"
"milkcomp1"
