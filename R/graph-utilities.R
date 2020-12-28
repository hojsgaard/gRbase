## #######################################################################
##
#' @title Find edges in a graph and edges not in a graph.
#'
#' @description Returns the edges of a graph (or edges not in a graph)
#'     where the graph can be either a `graphNEL` object, an `igraph`
#'     object or an adjacency matrix.
#'
#' @name graph-edgeList
#'
## Issues: Check that it works on undirected and DAGs.
##
## #######################################################################

#' @param object A `graphNEL` object, an `igraph` object, a dense
#'     matrix or a sparse `dgCMatrix` (the two latter representing an
#'     adjacency matrix).
#' @param adjmat An adjacency matrix. 
#' @param matrix If TRUE the result is a matrix; otherwise the result
#'     is a list.
#' 
#' @examples
#' ## A graph with edges
#' g  <- ug(~a:b + b:c + c:d)
#' gm <- as(g, "matrix")

#' edgeList(g)
#' edgeList(gm)
#' edgeListMAT(gm)

#' edgeList(g, matrix=TRUE)
#' edgeList(gm, matrix=TRUE)
#' edgeListMAT(gm, matrix=TRUE)

#' nonEdgeList(g)
#' nonEdgeList(gm)
#' nonEdgeListMAT(gm)

#' ## A graph without edges
#' g  <- ug(~a + b + c)
#' gm <- as(g, "matrix")

#' edgeList(g)
#' edgeList(gm)
#' edgeListMAT(gm)

#' edgeList(g, matrix=TRUE)
#' edgeList(gm, matrix=TRUE)
#' edgeListMAT(gm, matrix=TRUE)

#' nonEdgeList(g)
#' nonEdgeList(gm)
#' nonEdgeListMAT(gm)
#' 

#' @export
edgeList <- function(object, matrix=FALSE)
  UseMethod("edgeList")

#' @export
## #' @rdname graph-edgeList
edgeList.default <- function(object, matrix=FALSE){
    if (inherits(object, c("graphNEL", "igraph")))
        return(edgeListMAT(as(object, "matrix"), matrix=matrix))
    if (inherits(object, c("dgCMatrix", "matrix")))
        return(edgeListMAT(object, matrix=matrix))
    stop("Can not find edge list for this type of object\n")
}

#' @export
#' @rdname graph-edgeList
edgeListMAT <- function(adjmat, matrix=FALSE){
    out <- if (issymMAT_(adjmat)) symMAT2ftM_(adjmat)
           else MAT2ftM_(adjmat)

    ## cat("edgeListMAT\n")
    ## print(adjmat)
    
    di  <- dim(out)
    out <- colnames(adjmat)[out]
    dim(out) <- di

    if (!matrix) rowmat2list__(out)
    else out
    
}

#' @export
#' @rdname graph-edgeList
nonEdgeList <- function(object, matrix=FALSE)
  UseMethod("nonEdgeList")

#' @export
## #' @rdname graph-edgeList
nonEdgeList.default <- function(object, matrix=FALSE){
    if (inherits(object, c("graphNEL", "graphNEL")))
        return(nonEdgeListMAT(as(object, "matrix"), matrix=matrix))
    if (inherits(object, c("dgCMatrix", "matrix")))
        return(nonEdgeListMAT(object, matrix=matrix))
    stop("Can not find non-edge list for this type of object\n")
}

#' @export
#' @rdname graph-edgeList
nonEdgeListMAT <- function(adjmat, matrix=FALSE){
    if (!issymMAT_( adjmat )) stop("'adjmat' must be symmetric")
    if (inherits(adjmat, "dgCMatrix")){
        adjmat <- as( ((-1 * adjmat) + 1), "dgCMatrix")
    } else {
        adjmat <- -1 * adjmat + 1
    }
    edgeListMAT(adjmat, matrix=matrix)
}


## ---------------------------------------------------------------
##
## Coerce between undirected and directed graphs when possible
##
## ---------------------------------------------------------------
## FIXME dag2ug is missing
## FIXME ug2dag: should allow for other graph representations

#' @title Coerce between undirected and directed graphs when possible
#'
#' @description An undirected graph G can be converted to a dag if G
#'     is chordal. A dag D can be converted to an undirected graph if
#'     D can be moralized without adding edges.
#'
#' @title graph-ug2dag
#'
#' @param gn A graphNEL object or an object that can be converted to a
#'     graphNEL object.

#' @export
#' @rdname graph-ug2dag
ug2dag <- function(gn){
    if (!inherits(gn, "graphNEL")) stop("'gn' not a graphNEL object...")        
    if (graph::edgemode(gn) != "undirected") stop("Graph must have undirected edges")

    if (length( m <- mcs(gn) ) == 0) stop("Graph is not chordal")

    adjList  <- graph::adj(gn, m)
    vparList <- vector("list", length(m))
    names(vparList) <- m

    vparList[[1]] <- m[1]
    if (length(m) > 1){
        for (i in 2:length(m)){
            vparList[[ i ]] <- c(m[ i ],
                                intersectPrim(adjList[[ i ]], m[ 1:i ]))
        }
    }
    dagList(vparList)
}


















## ##########################################################
##
## vpar implemented for graphNEL, matrix and dgCMatrix
##
## ##########################################################

#' @title List of vertices and their parents for graph.
#'
#' @description Get list of vertices and their parents for graph.
#'
#' @name graph-vpar
#'
#' @param object An object representing a graph. Valid objects are an
#'     adjacency matrix or as a graphNEL.
#' @param getv The result is by default a list of vectors of the form
#'     \code{(v, pa1, pa2, ... paN)} where \code{pa1, pa2, ... paN}
#'     are the parents of \code{v}. If \code{getv} is \code{FALSE}
#'     then the vectors will have the form \code{(pa1, pa2, ... paN)}
#' @param forceCheck Logical indicating if it should be checked that
#'     the object is a DAG.
#' @return A list of vectors where each vector will have the form
#'     \code{(v, pa1, pa2, ... paN)} where \code{pa1, pa2, ... paN}
#'     are the parents of \code{v}.
#' @seealso \code{\link{dag}}, \code{\link{ug}}
#' @examples
#' 
#' ## DAGs
#' dagMAT <- dag(~a:b:c + c:d:e, result="matrix")
#' dagNEL <- dag(~a:b:c + c:d:e, result="graphNEL")

#' vpar(dagMAT)
#' vpar(dagNEL)
#' vpar(dagMAT, getv=FALSE)
#' vpar(dagNEL, getv=FALSE)

#' ## Undirected graphs
#' ugMAT <- ug(~a:b:c + c:d:e, result="matrix")
#' ugNEL <- ug(~a:b:c + c:d:e, result="graphNEL")

#' \dontrun{
#' ## This will fail because the adjacency matrix is symmetric and the
#' ## graphNEL has undirected edges
#' vpar(ugMAT)
#' vpar(ugNEL)
#' }

#' ## When forceCheck is FALSE, it will not be detected that the graphs are undirected.
#' vpar(ugMAT, forceCheck=FALSE)
#' vpar(ugNEL, forceCheck=FALSE)

#' ## Bidirected graphs
#' ## This is, for graphNELs, the same as working with bidirected edges:
#' if (require(graph)){
#' graph::edgemode(ugNEL)
#' graph::edgemode(ugNEL) <- "directed"
#' graph::edgemode(ugNEL)
#' vpar(ugNEL,FALSE)
#' }

#' @export
vchi <- function(object, getv=TRUE, forceCheck=TRUE){
  UseMethod("vchi")
}

#' @export
#' @rdname graph-vpar
vchiMAT <- function(object, getv=TRUE, forceCheck=TRUE){
  if (forceCheck && !is.adjMAT(object))
    stop("Matrix is not adjacency matrix... \n")
  if (forceCheck && isSymmetric(object))
    stop("Graph is undirected; (v, pa(v)) does not exist...\n")

  vn <- rownames(object)
  out <- lapply(seq_along(vn),
                function(j) vn[c(j, which(object[j, ]!=0))])
  names(out) <- vn

  if (!getv) # Don't want v, just pa(v)
      lapply(out, function(x)x[-1])
  else
      out
}

#' @export
## #' @rdname graph-vpar
vchi.graphNEL <- function(object, getv=TRUE, forceCheck=TRUE){
    if (forceCheck && graph::edgemode(object)=="undirected")
        stop("Graph is undirected; (v,pa(v)) does not exist...\n")

    ch <- graph::edges(object) ## Nodes and their children
    vn <- names(ch)


    out <- lapply(seq_along(ch), function(i) c(vn[i], ch[[i]]))
    names(out) <- vn
    out

    if (!getv) # Don't want v, just pa(v)
        lapply(out, function(x)x[-1])
    else
        out
}
#' @export
## #' @rdname graph-vpar
vchi.Matrix <- vchiMAT

#' @export
## #' @rdname graph-vpar
vchi.matrix <- vchiMAT

#' @export
#' @rdname graph-vpar
vpar <- function(object, getv=TRUE, forceCheck=TRUE){
  UseMethod("vpar")
}

#' @export
#' @rdname graph-vpar
vparMAT <- function(object, getv=TRUE, forceCheck=TRUE){
  if (forceCheck && !is.adjMAT(object))
    stop("Matrix is not adjacency matrix... \n")

  only_zeros <- sum(abs(object)) < 1e-12
  
  if (!only_zeros){
      if (forceCheck && isSymmetric(object))
          stop("Graph is undirected; (v, pa(v)) does not exist...\n")
  }
  
  vn <- rownames(object)
  out <- lapply(seq_along(vn),
                function(j) vn[c(j, which(object[, j]!=0))])
  names(out) <- vn

  if (!getv) # Don't want v, just pa(v)
      lapply(out, function(x)x[-1])
  else
      out
}

#' @export
## #' @rdname graph-vpar
vpar.graphNEL <- function(object, getv=TRUE, forceCheck=TRUE){

    ## ## FIXME: PERHAPS NOT A GOOD IDEA:
    ## no_edges  <- all(sapply(graph::edges(object), length) == 0)

    ##    if (!no_edges){
        if (forceCheck && graph::edgemode(object)=="undirected")
            stop("Graph is undirected; (v,pa(v)) does not exist...\n")
    ##}
    
    ch <- graph::edges(object) ## Nodes and their children
    vn <- names(ch)
    tf <- lapply(seq_along(ch),
                 function(i)
                     all_pairs( ch[[i]], vn[i],
                                  sort=FALSE, result="matrix"))

    tf <- do.call(rbind, tf) # matrix in to-from form
    out <- lapply(seq_along(ch),
                  function(i)
                      c(vn[i], tf[tf[, 1] == vn[i], 2]))
    names(out) <- vn

    if (!getv) # Don't want v, just pa(v)
        lapply(out, function(x)x[-1])
    else
        out
}

#' @export
## #' @rdname graph-vpar
vpar.Matrix <- vparMAT

#' @export
## #' @rdname graph-vpar
vpar.matrix <- vparMAT


#############################################################################
#'
#' @title Get cliques of an undirected graph 
#' @description Return a list of (maximal) cliques of an undirected graph.
#' @name graph-clique
#' 
#############################################################################
#' 
#' @details In graph theory, a clique is often a complete subset of a
#'     graph. A maximal clique is a clique which can not be
#'     enlarged. In statistics (and that is the convention we follow
#'     here) a clique is usually understood to be a maximal clique.
#' 
#' Finding the cliques of a general graph is an NP complete problem. Finding
#' the cliques of triangualted graph is linear in the number of cliques.
#' 
#' The workhorse is the \code{max_cliqueMAT} function which calls the
#' \code{maxClique} function in the \code{RBGL} package.
#' 
#' @param object An undirected graph represented either as a \code{graphNEL}
#'     object, an `igraph` object, a (dense) \code{matrix}, a (sparse) \code{dgCMatrix}
#' @param amat An adjacency matrix.
#' @return A list.
#' 
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @seealso \code{\link{ug}}, \code{\link{dag}}, \code{\link{mcs}},
#'     \code{\link{mcsMAT}}, \code{\link{rip}}, \code{\link{ripMAT}},
#'     \code{\link{moralize}}, \code{\link{moralizeMAT}}
#' @keywords utilities
#' @examples
#' ## graphNEL
#' uG0 <- ug(~a:b + b:c + c:d + d:e + e:f + f:a) # a graphNEL object
#' get_cliques(uG0)
#'
#' uG1 <- as(uG0, "igraph")
#' get_cliques(uG1)
#' 
#' uG2 <- as(uG0, "matrix") 
#' get_cliques(uG2)
#' 
#' uG3 <- as(uG1, "dgCMatrix") 
#' get_cliques(uG3)

#' @export 
#' @rdname graph-clique
get_cliques <- function(object){
    UseMethod("get_cliques")
}

#' @export 
get_cliques.graphNEL <- function(object){
    max_cliqueMAT(as(object, "matrix"))[[1]]
}

#' @export 
get_cliques.igraph <- function(object){
    max_cliqueMAT(as(object, "matrix"))[[1]]
}

#' @export 
get_cliques.default <- function(object){
    max_cliqueMAT(object)[[1]]
}

## FIXME: Should check that it is undirected.
#' @export 
#' @rdname graph-clique
max_cliqueMAT <- function(amat){
    .check.is.matrix(amat)
    vn <- dimnames(amat)[[2L]]
    em <- t.default(MAT2ftM_(amat))
    RBGL::maxClique(nodes=vn, edgeMat=em)
}

#' @rdname graph-clique
#' @section Synonymous functions:
#'
#' For backward compatibility with downstream packages we have the
#' following synonymous functions:
#'
#' * getCliques = get_cliques
#'
#' * maxCliqueMAT = max_cliqueMAT 

#' @export
getCliques    <- get_cliques

#' @rdname graph-clique
#' @aliases maxCliqueMAT

#' @export
maxCliqueMAT  <- max_cliqueMAT



##################################################################
#'
#' @title Random directed acyclic graph
#' @description Generate a random directed acyclic graph (DAG)
#' @name graph-randomdag
#' 
##################################################################
#'
#' @details If the maximum number of parents for a node is, say 3 and
#'     wgt=0.1, then the probability of the node ending up with
#'     0,1,2,3 parents is proportional to 0.1^0, 0.1^1, 0.1^2, 0.1^3.
#' 
#' @param V The set of vertices.
#' @param maxpar The maximum number of parents each node can have
#' @param wgt A parameter controlling how likely it is for a node to
#'     have a certain number of parents; see 'Details'.
#' 
#' @return A graphNEL object.
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @keywords utilities
#'
#' @examples
#' dg   <- random_dag(1:1000, maxpar=5, wgt=.9)
#' table(sapply(vpar(dg),length))
#' 
#' dg   <- random_dag(1:1000, maxpar=5, wgt=.5)
#' table(sapply(vpar(dg),length))
#' 
#' dg   <- random_dag(1:1000, maxpar=5, wgt=.1)
#' table(sapply(vpar(dg),length))
#' 
#' @export random_dag
random_dag <- function(V, maxpar=3, wgt=0.1){
    V <- as.character(V)
    vparList <- vector("list", length(V))
    names(vparList) <- V
    for (ii in 1:length(V)){
        rest <- V[-(1:ii)]
        zz <- 0:(min(maxpar, length(rest)))
        ## zz <- 0:(min(maxpar, length(rest))-1)
        if (min(zz) < 0)
            zz <- 0
        pp <- wgt^zz
        npar <- sample(zz, 1, prob=pp)
        vparList[[ii]] <- c(V[ii], sample(rest, npar, replace=FALSE))
    }
    dg <- dagList(vparList)
    dg
}









##
## SHD version of DED's dual rep; based on faster set operations
##
#' @export
.dual.rep <- function(glist, S, minimal=TRUE) {
    ## S is total varset - often but by no means always given by unique(unlist(g.list))
    list.save <- list()
    ##if (length(glist)==0) list.save <- list(S)
    if (length(glist) == 1 & is.logical(glist[[1]]))
        list.save <- list(S)
    else {
        for (v in 1:length(glist)) {
            m1 <- list.save
            if (minimal)
                m2 <- as.list( setdiffPrim(S, glist[[v]]) )
            else
                m2 <- as.list( glist[[v]] )

            if (v==1)
                list.save <- m2
            else {
                ##aaa <- unlist(lapply(m1, function(g)
                ##                     lapply(m2, union, g)),recursive=FALSE)
                aaa <- unlist(lapply(m1, function(g)
                                     lapply(m2, function(o){unique.default(c(o, g))})),
                              recursive=FALSE)
                list.save <- remove_redundant(aaa, FALSE)
            }
        }
        if (!minimal)
            list.save <- lapply(list.save, function(g) setdiffPrim(S, g))}
    list.save
}





