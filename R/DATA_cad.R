#' Coronary artery disease data
#' 
#' A cross classified table with observational data from a Danish
#' heart clinic.  The response variable is CAD (coronary artery
#' disease, some times called heart attack).
#'
#' @details
#'
#' Notice that data are collected at a heart clinic, so data do not
#' represent the population, but are conditional on patients having
#' ended up at the clinic.
#' 
#' * cad1: Complete dataset, 236 cases.
#'
#' * cad2: Incomplete dataset, 67 cases. Information on (some of) the
#'     variables 'Hyperchol', 'Smoker' and 'Inherit' is missing.
#' 
#' @name data_cad
#' @aliases cad1 cad2
#' @docType data
#' @format A data frame with 236 observations on the following 14 variables.
#'
#' \describe{
#'   \item{\code{Sex}}{Sex; a factor with levels \code{Female} \code{Male}}
#'
#'   \item{\code{AngPec}}{Angina pectoris (chest pain attacks); a
#'   factor with levels \code{Atypical} \code{None} \code{Typical}}
#'
#'   \item{\code{AMI}}{Acute myocardic infarct; a factor with
#'   levels \code{Definite} \code{NotCertain}}
#'
#'   \item{\code{QWave}}{A reading from an electrocardiogram; a
#'   factor with levels \code{No} \code{Yes}; Yes means pathological and is a sign of previous myocardial infarction. }
#'
#'   \item{\code{QWavecode}}{a factor with levels \code{Nonusable}
#'   \code{Usable}. An assesment of whether QWave is reliable.}
#'
#'   \item{\code{STcode}}{a factor with levels
#'   \code{Nonusable} \code{Usable}. An assesment of whether STchange is reliable.}
#'
#'   \item{\code{STchange}}{A reading from an electrocardiogram; a factor
#'   with levels \code{No} \code{Yes}. An STchange indicates a blockage of the coronary artery.}
#'
#'   \item{\code{SuffHeartF}}{Sufficient heart frequency; a factor with levels \code{No}, \code{Yes}}
#' 
#'   \item{\code{Hypertrophi}}{a factor with levels \code{No}, \code{Yes}. Hypertrophy refers to an
#'   increased size of the heart muscle due to exercise. }
#'
#'   \item{\code{Hyperchol}}{a factor with levels \code{No} \code{Yes}. Hypercholesterolemia, also called high cholesterol,
#'    is the presence of high levels of cholesterol in the blood.}
#'
#'   \item{\code{Smoker}}{Is the patient a smoker; a factor with levels \code{No}, \code{Yes}.}
#'
#'   \item{\code{Inherit}}{Hereditary predispositions for CAD; a factor with levels  \code{No}, \code{Yes}.}
#'
#'   \item{\code{Heartfail}}{Previous heart failures; a factor with  levels \code{No} \code{Yes}}
#'
#'   \item{\code{CAD}}{Coronary Artery Disease; a factor with levels
#'    \code{No} \code{Yes}}.  CAD refers to a reduction of blood flow
#'    to the heart muscle (commonly known as a heart attack). The
#'    diagnosis made from biopsies.
#'
#' }
#'
#' 
#' @references Hansen, J. F. (1980). The clinical diagnoisis of ichaeme heart disease du to
#' coronary artery disease. Danish Medical Bulletin
#'
#' Højsgaard, Søren and Thiesson, Bo (1995). BIFROST - Block
#' recursive models Induced From Relevant knowledge, Observations and
#' Statistical Techniques. Computational Statistics and Data Analysis, vol. 19,
#' p. 155-175
#' 
#' 
#'
#' @keywords datasets
#' @usage data(cad1)
#' 
#' @examples
#' 
#' data(cad1)
#' ## maybe str(cad1) ; plot(cad1) ...
#' 
"cad1"
"cad2"
