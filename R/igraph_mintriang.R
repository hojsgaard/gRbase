## #####################################################
##
## Minimal triangulation of an undirected graph
##
## #####################################################

## R function returning a minimal triangulation of an undirected graph
## by the recursive thinning method

## Author: Clive Bowsher

## Inputs:
## uG: igraph representation of the undirected graph

## Output: returns a minimal triangulation TuG of the undirected graph
## uG; the mcwh method is used to obtain the initial
## triangulation prior to applying the Recursive Thinning algorithm
## of Olesen & Madsen 2002

## date originated: 23.03.09

## checked:

## using the DAGs of Fig.1,2 of Olesen & Madsen 2002, Fig1a and Fig2
##			of Leimer93 (CGB 04.03.09).

## using TestMinTriang2.R (CGB 08.03.09), see comments on that file
##			issues: clearly the triangulation is not guranteed to be a
##			minimUM one, but this is not necessary for our purposes here

##############################################################################
#' @title Minimal triangulation of an undirected graph
#' @description An undirected graph uG is triangulated (or chordal) if
#'     it has no cycles of length >= 4 without a chord which is
#'     equivalent to that the vertices can be given a perfect
#'     ordering. Any undirected graph can be triangulated by adding
#'     edges to the graph, so called fill-ins which gives the graph
#'     TuG.  A triangulation TuG is minimal if no fill-ins can be
#'     removed without breaking the property that TuG is triangulated.
#' @name graph-min-triangulate
##############################################################################

#' @details For a given triangulation tobject it may be so that some
#'     of the fill-ins are superflous in the sense that they can be
#'     removed from tobject without breaking the property that tobject
#'     is triangulated. The graph obtained by doing so is a minimal
#'     triangulation.
#'
#'     Notice: A related concept is the minimum
#'     triangulation, which is the the graph with the smallest number
#'     of fill-ins. The minimum triangulation is unique. Finding the
#'     minimum triangulation is NP-hard.
#' 
#' @param object An undirected graph represented either as a \code{igraph}
#'     object, a (dense) \code{matrix}, a (sparse) \code{dgCMatrix}.
#' @param tobject Any triangulation of \code{object}; must be of the same
#'     representation.
#' @param result The type (representation) of the result. Possible values are
#'     \code{"igraph"}, \code{"matrix"}, \code{"dgCMatrix"}. Default is the
#'     same as the type of \code{object}.
#' @param amat The undirected graph which is to be triangulated; a symmetric
#'     adjacency matrix.
#' @param tamat Any triangulation of \code{object}; a symmetric adjacency
#'     matrix.
#' @param details The amount of details to be printed.
#' @return \code{minimal_triang()} returns an igraph object while
#'     \code{minimal_triangMAT()} returns an adjacency matrix.
#' @author Clive Bowsher <C.Bowsher@@statslab.cam.ac.uk> with modifications by
#'     Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @seealso \code{\link{mpd}}, \code{\link{rip}}, \code{\link{triangulate}}
#' @references Kristian G. Olesen and Anders L. Madsen (2002): Maximal Prime
#'     Subgraph Decomposition of Bayesian Networks. IEEE TRANSACTIONS ON
#'     SYSTEMS, MAN AND CYBERNETICS, PART B: CYBERNETICS, VOL. 32, NO. 1,
#'     FEBRUARY 2002
#' @keywords utilities
#' @examples
#' 
#' ## An igraph object
#' g1 <- ug(~a:b + b:c + c:d + d:e + e:f + a:f + b:e, result="igraph")
#' x <- minimal_triang(g1)
#'
#' tt <- ug(~a:b:e:f + b:e:c:d, result="igraph")
#' x <- minimal_triang(g1, tobject=tt)
#' 
#' ## g2 is a triangulation of g1 but it is not minimal
#' g2 <- ug(~a:b:e:f + b:c:d:e, result="igraph")
#' x <- minimal_triang(g1, tobject=g2)
#' 
#' ## An adjacency matrix
#' g1m <- ug(~a:b + b:c + c:d + d:e + e:f + a:f + b:e, result="matrix")
#' x <- minimal_triangMAT(g1m)
#' 
#' @export 
minimal_triang <- function(object, tobject=triangulate(object), result=NULL, details=0){
    UseMethod("minimal_triang")
}

#' @export 
## #' @rdname graph-min-triangulate
minimal_triang.default <- function(object, tobject=triangulate(object), result=NULL, details=0) {

    graph_class <- c("igraph", "matrix", "dgCMatrix")
    chk <- inherits(object, graph_class, which=TRUE)
    if (!any(chk)) stop("Invalid class of 'object'\n")
    cls <- graph_class[which(chk > 0)]
    
    if (is.null(result))
        result <- cls
    
    tt<-.minimal_triang(as(object, "igraph"),
                        TuG=as(tobject, "igraph"), details=details)

    as(tt, result)
}


#' @export
#' @rdname graph-min-triangulate
minimal_triangMAT <- function(amat, tamat=triangulateMAT(amat), details=0) {
    as.adjMAT(.minimal_triang(as(amat, "igraph"), TuG=as(tamat, "igraph"),
                              details=details))
}


.minimal_triang <- function(uG, TuG=triangulate(uG, method="mcwh"), details=0) {
    
    removed <- 0
    
    if (is.triangulated(uG)) { ## no triangulation of G is needed            
        if (details>0)
            cat(sprintf("Graph is already triangulated\nNumber of edges removed in recursive thinngs: %i\n", removed))
        return(uG)
    }

    uGmat  <- as(uG, "matrix")
    di     <- dimnames(uGmat)

    TuGmat <- as(TuG, "matrix")
    TuGmat <- TuGmat[di[[1]], di[[1]]]
    TT <- TuGmat - uGmat   ## edges added in triangulatn in adjacency representatn
    TT <- as(TT, "igraph") ## Added edges as igraph
    Tn <- edgeList(TT)     ## And as list
    
    if (details>0)
        cat(sprintf("Number of edges added in triangulation step: %i \n", length(Tn)))
    
    Rn <- Tn
    exclT <- ug(as.list(nodes(uG)))
    
    repeat {
        ## cat("iterating...\n")
        if (length(Rn) == 0) { ## if R' is empty so is TT' and so cannot execute the for loop below
            break
        }
        for(i in 1:length(Rn)){
            edge <- Rn[[i]]
            ## cat("edge to remove and add:\n"); print(edge)
            neX <- gRbase::adj(TuG, edge[1])
            neY <- gRbase::adj(TuG, edge[2])

            neXY <- neX[[1]][neX[[1]] %in% neY[[1]]] ## select elements of neX in neY to obtain intersection
            ## str(list(neX=neX, neY=neY, neXY=neXY))

            sg <- gRbase::subGraph(neXY, TuG)
            if (is.complete(sg)) {
                ## cat("updating...\n")
                TuG   <- gRbase::removeEdge(edge[1], edge[2], TuG)	
                exclT <- gRbase::addEdge(edge[1], edge[2], exclT)	
                removed <- removed + 1
            }
        }
        if (length(edgeList(exclT)) == 0) {
            break
        }
        TuGmat <- as(TuG, "matrix")  ## Needed because TuG is changed above
        TuGmat <- TuGmat[di[[1]], di[[1]]]
        TT     <- TuGmat - uGmat # edges added in triangulatn in adjacency representatn
        TT <- as(TT, "igraph")
        Tn <- edgeList(TT)
        
        ## form vector of nodes corresponding to the edge list (possibly with repeated elements)
        exclT <- c(edgeList(exclT), recursive=TRUE)
        Rn <- vector("list", length = 0)
        j <- 0
        ## for(k in 1:length(Tn)) {
        for(k in seq_along(Tn)) {
            if ((Tn[[k]][1] %in% exclT) | (Tn[[k]][2] %in% exclT)) {
                j <- j + 1
                Rn[[j]] <- Tn[[k]]
            }
        }
        exclT <- ug(as.list(nodes(uG)))            
    } ## repeat

    
    if (details > 0)
        cat(sprintf("Number of edges removed in recursive thinngs: %i\n", removed))
    if (!(is.triangulated(TuG))) {            
        cat("WARNING: minimal triangulation step failed\n")
    }
    
    return(TuG)
}


                
