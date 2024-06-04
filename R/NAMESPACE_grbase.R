
#' @useDynLib gRbase

## Vanilla R imports (and exports)
## -------------------------------

#' @importFrom stats as.formula cov cov2cor delete.response deviance
#'     formula runif terms xtabs simulate xtabs runif terms addmargins
#'     as.formula cov.wt fitted formula ftable getCall logLik loglin
#'     na.omit pchisq pf pnorm r2dtable terms update update.formula
#' 
#' @importFrom utils combn str install.packages
#'
#' @importMethodsFrom stats4 plot
#' @exportMethod plot
#' 
#'
## ## ' @importFrom BiocManager install

## Miscellaneous
## -------------
#' @importFrom Rcpp evalCpp
#'
#' @import methods
#'
#' @importFrom igraph 
#'     get.adjacency V "V<-" E "E<-" is.directed layout.lgl
#'     layout.graphopt plot.igraph graph.adjacency is.dag as_adjacency_matrix
#'     make_graph make_empty_graph max_cliques is_igraph all_simple_paths subgraph is_chordal
#'     components as_adj_list add_edges add_vertices
#'
#' 

## Bioconductor imports/exports
## ----------------------------

## ' @importClassesFrom graph graphNEL
## ' @importFrom graph edges nodes edgeMatrix addEdge addNode removeEdge removeNode
## ' @export edges nodes edgeMatrix addEdge addNode removeEdge removeNode
## ' @importMethodsFrom Rgraphviz plot

#' @importFrom methods as new setOldClass
#'
#' @importMethodsFrom Matrix t isSymmetric diag
#' @importFrom Matrix Matrix

#' @export solveSPD MAT2ftM_ symMAT2ftM_
NULL

