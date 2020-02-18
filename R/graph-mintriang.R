## #####################################################
##
## Minimal triangulation of an undirected graph
##
## #####################################################

## R function returning a minimal triangulation of an undirected graph
## by the recursive thinning method

## Author: Clive Bowsher

## Inputs:
## uG: graphNEL representation of the undirected graph

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
#' @param object An undirected graph represented either as a \code{graphNEL}
#'     object, a (dense) \code{matrix}, a (sparse) \code{dgCMatrix}.
#' @param tobject Any triangulation of \code{object}; must be of the same
#'     representation.
#' @param result The type (representation) of the result. Possible values are
#'     \code{"graphNEL"}, \code{"matrix"}, \code{"dgCMatrix"}. Default is the
#'     same as the type of \code{object}.
#' @param amat The undirected graph which is to be triangulated; a symmetric
#'     adjacency matrix.
#' @param tamat Any triangulation of \code{object}; a symmetric adjacency
#'     matrix.
#' @param details The amount of details to be printed.
#' @return \code{minimal_triang()} returns a graphNEL object while
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
#' ## A graphNEL object
#' g1 <- ug(~a:b + b:c + c:d + d:e + e:f + a:f + b:e)
#' x <- minimal_triang(g1)
#' 
#' ## g2 is a triangulation of g1 but it is not minimal
#' g2 <- ug(~a:b:e:f + b:c:d:e)
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
minimal_triang.default <- function(object, tobject=triangulate(object), result=NULL, details=0){

    graph_class <- c("graphNEL", "igraph", "matrix", "dgCMatrix")
    chk <- inherits(object, graph_class, which=TRUE)
    if (!any(chk)) stop("Invalid class of 'object'\n")
    cls <- graph_class[which(chk > 0)]
    
    if (is.null(result))
        result <- cls
    
    ## cls <- match.arg(class( object ),
    ##                  c("graphNEL","matrix","dgCMatrix"))
    ## if (is.null(result))
    ##     result <- cls
    
    ## switch(cls,
    ##        "graphNEL" ={tt<-.minimal_triang(object, TuG=tobject, details=details) },
    ##        "dgCMatrix"=,
    ##        "matrix"   ={ #FIXME: minimal_triang: Not sure if this is correct...
    ##            object2 <- as(object,  "graphNEL")
    ##            tobject2<- as(tobject, "graphNEL")
    ##            tt<-.minimal_triang(object2, TuG=tobject2, details=details)
    ##        })

    
    tt<-.minimal_triang(as(object, "graphNEL"),
                        TuG=as(tobject, "graphNEL"), details=details)

    as(tt, result)
}


#' @export
#' @rdname graph-min-triangulate
minimal_triangMAT <- function(amat, tamat=triangulateMAT(amat), details=0){
    as.adjMAT(.minimal_triang(as(amat, "graphNEL"), TuG=as(tamat, "graphNEL"),
                              details=details))
}


.minimal_triang <- function(uG, TuG=triangulate(uG, method="mcwh"), details=0) {
    
    removed <- 0
    
    if (RBGL::is.triangulated(uG)) # no triangulation of G is needed
    {
        if (details>0)
            cat(sprintf("Graph is already triangulated\nNumber of edges removed in recursive thinngs: %i\n", removed))
        return(uG)
    }
    else
    {
        ##TuG <- triangulate(uG, method="mcwh")	# calls C implementation of mcwh method
        
        ## Soren modification
        uGmat  <- as(uG,"matrix")
        di     <- dimnames(uGmat)
        TuGmat <- as(TuG,"matrix")
        TuGmat <- TuGmat[di[[1]],di[[1]]]
        TT <- TuGmat - uGmat # edges added in triangulatn in adjacency representatn
        ## ends here...
        
        ## TT <- as(TuG,"matrix") - as(uG,"matrix")
        ## edges added in triangulation in adjacency representation
        TT <- as(TT,"graphNEL")
        Tn <- edgeList(TT)
        
        if (details>0)
            cat(sprintf("Number of edges added in triangulation step: %i \n", length(Tn)))
        
        Rn <- Tn
        exclT <- new("graphNEL", nodes = graph::nodes(uG), edgeL = vector("list", length = 0))
        
        repeat{
            if (length(Rn) == 0) {	# if R' is empty so is TT' and so cannot execute the for loop below
                break
            }
            for(i in 1:length(Rn)){
                neX <- graph::adj(TuG,Rn[[i]][1])
                neY <- graph::adj(TuG,Rn[[i]][2])
                neXY <- neX[[1]][neX[[1]] %in% neY[[1]]] 	#select elements of neX in neY to obtain intersection
                
                ##           sg <<- subGraph(neXY,TuG)
                ##           print(sg)
                if(is.complete(graph::subGraph(neXY,TuG)))
                {
                    TuG   <- graph::removeEdge(Rn[[i]][1],Rn[[i]][2],TuG)	# directly updates TuG
                    exclT <- graph::addEdge(Rn[[i]][1],Rn[[i]][2],exclT)	# keep track of excluded edges as a graph
                    removed <- removed + 1
                }
            }
            if (length(edgeList(exclT)) == 0){
                break
            }
            ## Soren modification
            uGmat  <- as(uG,"matrix")
            di <- dimnames(uGmat)
            TuGmat <- as(TuG,"matrix")
            TuGmat <- TuGmat[di[[1]],di[[1]]]
            TT <- TuGmat - uGmat # edges added in triangulatn in adjacency representatn
            ## ends here...
            ##TT <- as(TuG,"matrix") - as(uG,"matrix")		# recompute the triangulation edges retained
            
            TT <- as(TT,"graphNEL")
            Tn <- edgeList(TT)
            
            ## form vector of nodes corresponding to the edge list (possibly with repeated elements)
            exclT <- c(edgeList(exclT),recursive=TRUE)
            
            Rn <- vector("list", length = 0)
            j <- 0
            for(k in 1:length(Tn)) {
                if((Tn[[k]][1] %in% exclT) | (Tn[[k]][2] %in% exclT)) {
                    j <- j+1
                    Rn[[j]] <- Tn[[k]]
                }
            }
            
            ## NOTE: TuG already updated at line 43
            exclT <- new("graphNEL", nodes = graph::nodes(uG), edgeL = vector("list", length = 0))
        }
        if (details>0)
            cat(sprintf("Number of edges removed in recursive thinngs: %i\n", removed))
        if (!(RBGL::is.triangulated(TuG))){
            cat("WARNING: minimal triangulation step failed\n")
        }
        
        return(TuG)
    }
}
