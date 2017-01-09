#' Crown dieback in ash trees
#' 
#' This dataset comes from a study of symptoms of crown dieback, cankers and
#' symptoms caused by other pathogens and pests in ash trees (Fraxinus
#' excelsior). In all 454 trees were observed in two plots. There are 8
#' categorical variables, 6 of which are binary and two are trichotomous with
#' values representing increasing severity of symptoms, and one continuous
#' variable, tree diameter at breast height (DBH).
#' 
#' 
#' @name ashtrees
#' @docType data
#' @format A data frame with 454 observations on the following 9 variables.
#' \describe{
#'   \item{\code{plot}}{a factor with levels \code{2} \code{6}}
#'   \item{\code{dieback}}{a factor with levels \code{0} \code{1} \code{2}}
#'   \item{\code{dead50}}{a factor with levels \code{0} \code{0.5} \code{1}}
#'   \item{\code{bushy}}{a factor with levels \code{0} \code{1}}
#'   \item{\code{canker}}{a factor with levels \code{BRNCH} \code{MAIN} \code{NONE}}
#'   \item{\code{wilt}}{a factor with levels \code{0} \code{1}}
#'   \item{\code{roses}}{a factor with levels \code{0} \code{1}}
#'   \item{\code{discolour}}{a factor with levels \code{0} \code{1}}
#'   \item{\code{dbh}}{a numeric vector}
#' }
#' 
#' @references Skovgaard JP, Thomsen IM, Skovgaard IM and Martinussen
#'     T (2009).  Associations among symptoms of dieback in even-aged
#'     stands of ash (Fraxinus excelsior L.). Forest Pathology.
#'
#' @keywords datasets
#'
#' @examples
#' data(ashtrees)
#' head(ashtrees)
#' 
"ashtrees"
