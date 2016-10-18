#' Simulated data from the Chest Clinic example
#' 
#' Simulated data from the Chest Clinic example (also known as the Asia
#' example) from Lauritzen and Spiegelhalter, 1988.
#' 
#' @name chestSim
#'
#' @aliases chestSim chestSim500 chestSim1000 chestSim10000
#'     chestSim50000 chestSim100000
#' @docType data
#'
#' @format A data frame with 500 observations on the following 8 variables.
#'   \describe{
#'   \item{\code{asia}}{a factor with levels \code{yes} \code{no}}
#'   \item{\code{tub}}{a factor with levels \code{yes} \code{no}}
#'   \item{\code{smoke}}{a factor with levels \code{yes} \code{no}}
#'   \item{\code{lung}}{a factor with levels \code{yes} \code{no}}
#'   \item{\code{bronc}}{a factor with levels \code{yes} \code{no}}
#'   \item{\code{either}}{a factor with levels \code{yes} \code{no}}
#'   \item{\code{xray}}{a factor with levels \code{yes} \code{no}}
#'   \item{\code{dysp}}{a factor with levels \code{yes} \code{no}}
#' }
#' 
#' @references Lauritzen and Spiegelhalter (1988) Local Computations
#'     with Probabilities on Graphical Structures and their
#'     Application to Expert Systems (with
#'     Discussion). J. Roy. Stat. Soc. 50, p. 157-224.
#' @keywords datasets
#' @examples
#' 
#' data(chestSim500)
#' ## maybe str(chestSim500) ; plot(chestSim500) ...
#' 
"chestSim500"
"chestSim1000"
"chestSim10000"
"chestSim50000"
"chestSim100000"
