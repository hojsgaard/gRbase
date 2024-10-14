#' @title Function for plotting graphs using the 'igraph' package.
#' 
#' @description Generic function for plotting graphs using the
#'     'igraph' package.
#' 
#' @name graph_iplot
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
#' @rdname graph_iplot
iplot.igraph <- function(x,...){
  plot(x, ...)
}

