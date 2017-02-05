#' Lizard behaviour
#' 
#' In a study of lizard behaviour, characteristics of 409 lizards were
#' recorded, namely species (S), perch diameter (D) and perch height (H). The
#' focus of interest is in how the propensities of the lizards to choose perch
#' height and diameter are related, and whether and how these depend on
#' species.
#' 
#' 
#' @name lizard
#' @aliases lizard lizardRAW lizardAGG
#' @docType data
#' @format A 3--dimensional array with factors diam: "<=4" ">4" height: ">4.75"
#' "<=4.75" species: "anoli" "dist"
#' @references Schoener TW (1968) The anolis lizards of bimini: Resource
#' partitioning in a complex fauna. Ecology 49:704-726
#' @keywords datasets
#' @examples
#' 
#' data(lizard)
#' 
#' # Datasets lizardRAW and lizardDF are generated with the following code
#' #lizardAGG <- as.data.frame(lizard)
#' #f   <- lizardAGG$Freq
#' #idx <- unlist(mapply(function(i, n) rep(i, n), 1:8, f))
#' #set.seed(0805)
#' #idx <- sample(idx)
#' #lizardRAW <- as.data.frame(lizardAGG[idx, 1:3])
#' #rownames(lizardRAW) <- 1:NROW(lizardRAW)
#' 
#' 
"lizard"
"lizardRAW"
"lizardAGG"
