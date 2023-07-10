#' @title Function for plotting graphs using the 'igraph' package.
#' 
#' @description Generic function for plotting graphs using the
#'     'igraph' package and a plot method for graphNEL objects.
#' 
#' @name graph-iplot
#' 
#' @param x A graph object to be plotted.
#' @param \dots Additional arguments
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @keywords graphics
#' @examples
#' 
#' UG <- ug(~a:b+b:c:d)
#' iplot(UG)
#' 

#' @export
iplot <- function(x,...){
  UseMethod("iplot")
}

#' @export
#' @rdname graph-iplot
iplot.graphNEL <- function(x,...){
  ig <- igraph::igraph.from.graphNEL(x)
  igraph::V(ig)$label <- igraph::V(ig)$name
  igraph::V(ig)$size  <- 50
  ig$cex   <-  4
                                        #ig$layout   <- layout.graphopt
                                        #ig$layout <- layout.kamada.kawai
  ig$layout <- igraph::layout.lgl
  plot(ig,
       vertex.label.family="Helvetica",
       edge.label.family="Helvetica",
       vertex.label.cex=2,
       edge.label.cex=2)
}


