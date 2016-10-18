#' Gene expression signatures for p53 mutation status in 250 breast cancer
#' samples
#' 
#' 
#' Perturbations of the p53 pathway are associated with more aggressive and
#' therapeutically refractory tumours. We preprocessed the data using Robust
#' Multichip Analysis (RMA). Dataset has been truncated to the 1000 most
#' informative genes (as selected by Wilcoxon test statistics) to simplify
#' computation. The genes have been standardised to have zero mean and unit
#' variance (i.e. z-scored).
#' 
#' The factor \code{code} defines whether there was a mutation in the p53
#' sequence (code=case) or not (code=control).
#' 
#' @name breastcancer
#'
#' @docType data
#'
#' @format A data frame with 250 observations on 1001 variables. The
#'     first 1000 columns are numerical variables; the last column
#'     (named \code{code}) is a factor with levels \code{case} and
#'     \code{control}.
#'
#' @references Miller et al (2005, PubMed
#'     ID:16141321)
#'
#' @source Dr. Chris Holmes, c.holmes at stats
#'     dot. ox . ac .uk
#'
#' @keywords datasets
#'
#' @examples
#' 
#' data(breastcancer)
#' ## maybe str(breastcancer) ; plot(breastcancer) ...
#' 
"breastcancer"
