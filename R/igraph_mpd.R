## #####################################################
##
## Maximal prime subgraph decomposition
##
## #####################################################

## R function returning a junction tree representation of the MPD of an undirected graph
## Author: Clive Bowsher

## Inputs:
## uG: igraph representation of the undirected graph

## Output: returns a junction tree representation of the MPD of uG
##         using the Recursive Thinning and Aggregate Cliques
##         algorithms of Olesen & Madsen 2002. Names(MPDTree[[r]])
##         retains the original clique numbers from the RIP ordering
##         of cliques of the minimal triangulated graph returned by
##         rip(TuG) below (for r=2,3,4)

## date: 23.03.09 checked: using the DAG of Figs 2(Asia) & 9 of Olesen
## & Madsen 2002, Fig1s and Fig2 of Leimer93 (CGB 09.03.09).  also
## using the known MPD of ugraph(kLIGnelChap), and line-by-line on
## 19.03.09 issues: none known


## #####################################################################
#'
#' @title Maximal prime subgraph decomposition
#' 
#' @description Finding a junction tree representation of the MPD
#'     (maximal prime subgraph decomposition) of an undirected graph
#'     The maximal prime subgraph decomposition of a graph is the
#'     smallest subgraphs into which the graph can be decomposed.
#'
#' @name graph-mpd
#' 
## #####################################################################
#' @aliases mpd mpd.default mpdMAT
#'
#' @param object An undirected graph; an igraph or
#'     an adjacency matrix.
#' @param tobject Any minimal triangulation of object; an igraph or an adjacency matrix.
#' @param amat An undirected graph; a symmetric adjacency matrix
#' @param tamat Any minimal triangulation of object; a symmetric
#'     adjacency matrix
#' @param details The amount of details to be printed.
#' 
#' @return A list with components "nodes", "cliques", "separators",
#'     "parents", "children", "nLevels". The component "cliques"
#'     defines the subgraphs.
#' @author Clive Bowsher \email{C.Bowsher@@statslab.cam.ac.uk} with
#'     modifications by Søren Højsgaard, \email{sorenh@@math.aau.dk}
#'
#' @seealso \code{\link{mcs}}, \code{\link{mcsMAT}},
#'     \code{\link{minimal_triang}}, \code{\link{minimal_triangMAT}},
#'     \code{\link{rip}}, \code{\link{ripMAT}}, \code{\link{triangulate}},
#'     \code{\link{triangulateMAT}}
#'
#' @references Kristian G. Olesen and Anders L. Madsen (2002): Maximal Prime
#'     Subgraph Decomposition of Bayesian Networks. IEEE TRANSACTIONS ON
#'     SYSTEMS, MAN AND CYBERNETICS, PART B: CYBERNETICS, VOL. 32, NO. 1,
#'     FEBRUARY 2002
#'
#' @keywords utilities
#' @examples
#' 
#' ## Maximal prime subgraph decomposition 
#' g1 <- ug(~ a:b + b:c + c:d + d:e + e:f + a:f + b:e)
#' if (interactive()) plot(g1)
#' x <- mpd(g1)
#' 
#' ## Maximal prime subgraph decomposition - an adjacency matrix
#' g1m <- ug(~ a:b + b:c + c:d + d:e + e:f + a:f + b:e, result="matrix")
#' x <- mpdMAT(g1m)
#' 
#' @export mpd
mpd <- function(object, tobject=minimal_triang(object), details=0) {
    UseMethod("mpd")
}

#' @export
#' @rdname graph-mpd
mpd.default <- function(object, tobject=triangulate(object), details=0){

    graph_class <- c("igraph", "matrix", "dgCMatrix")
    chk <- inherits(object, graph_class, which=TRUE)
    if (!any(chk)) stop("Invalid class of 'object'\n")

    .mpd(as(object, "igraph"), TuG=as(tobject, "igraph"), details=details)
    
}

#' @export
#' @rdname graph-mpd
mpdMAT <- function(amat, tamat=minimal_triangMAT(amat), details=0){
  .mpd(as(amat, "igraph"), TuG=as(tamat, "igraph"), details=details)
}

.mpd <- function(uG, TuG=minimal_triang(uG), details=0) {

  ##TuG <- MinimalTriang(uG)

  Tree <- rip(TuG) ## Tree arranged as [1]nodes,[2]cliques,[3]separators,[4]parents
  #print(Tree)

  MPDTree <- vector("list", length=4)

  if (length(Tree[[2]]) == 1) {
      MPDTree <- Tree		## Tree contains only 1 clique so cannot be decomposed
  } else {
      i <- length(Tree[[3]])	## indexes separators of Tree, starting with the last one (although the order of aggregation is immaterial)
      while (i > 1) {		## 1st clique never has a parent
          ## if(!(is.complete(graph::subGraph(c(Tree[[3]][i],recursive=TRUE),uG))))
          if(!(is.complete(gRbase::subGraph(c(Tree[[3]][i],recursive=TRUE),uG)))) {
              Tree[[3]][i] <- 0 
              ## set separator i to 'empty'
              parent.i <- Tree[[4]][[i]]
              ##cat(sprintf("i=%i parent.i=%i\n", i, parent.i))
              sel <- !(Tree[[2]][[i]] %in% Tree[[2]][[parent.i]])
              Tree[[2]][[parent.i]] <- c(Tree[[2]][parent.i],Tree[[2]][[i]][c(sel)],recursive=TRUE)	# merge clique i into its parent clique

              if (details>0)
                cat(sprintf("Clique %i merged into clique %i\n", i, parent.i))

              Tree[[2]][[i]] <- 0		# set clique i to 'empty'
              Tree[[4]][[i]] <- 0		# i has been emptied, hence has no parent
              if(i < length(Tree[[3]]))	# last cluster can have no children
		{
                  for (j in (i+1):length(Tree[[4]]))  		# determine previous children of i, and make them children of parent.i
                    {
                      if(Tree[[4]][[j]] == i)
                        {
                          Tree[[4]][[j]] <- parent.i
                        }
                    }
		}
            }
          i <- i - 1
        }

      MPDTree <- Tree

      sel <- matrix(0,length(MPDTree[[2]]),2)
      for (i in 1:length(MPDTree[[2]]))
        {
          if(0 == MPDTree[[2]][[i]][1])
            {
              sel[i,1] <- FALSE # no change to sel
            }
          else
            {
              sel[i,1] <- TRUE	## to select only those cluster numbers that are now not empty
              sel[i,2] <- i	## record original cluster number i
            }
        }

      ch    <- as.logical(sel[,1])
      Appel <- sel[,2][ch]	## original cluster numbers of non-empty clusters
      MPDTree[[2]] <- MPDTree[[2]][ch]
      names(MPDTree[[2]]) <- Appel
      MPDTree[[3]] <- MPDTree[[3]][ch]
      names(MPDTree[[3]]) <- Appel
      MPDTree[[4]] <- MPDTree[[4]][ch]
      names(MPDTree[[4]]) <- Appel

      return(MPDTree)
    }
}



