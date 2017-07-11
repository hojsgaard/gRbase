## ###############################################################
##
## Get list of edges in graph and list of edges not in graph
##
## FIXME: Check that it works on undirected and DAGs
##
## ###############################################################

#' @title Find edges in a graph and edges not in a graph.
#'
#' @description Returns the edges of a graph (or edges not in a graph) where the
#'     graph can be either a graphNEL object or an adjacency matrix.
#'
#' @name graph-edgeList
#'
#' @param object A graphNEL object or an adjacency matrix.
#' @param matrix If TRUE the result is a matrix; otherwise the result is a list.
#' @param adjmat An adjacency matrix.
#' @examples
#'
#'
#' ## A graph with edges
#' g  <- ug(~a:b + b:c + c:d)
#' gm <- graphNEL2M(g)

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
#' gm <- graphNEL2M(g)

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
edgeList <- function(object, matrix=FALSE)
  UseMethod("edgeList")

#' @rdname graph-edgeList
edgeList.default <- function(object, matrix=FALSE){
    cls <- match.arg(class( object ),
                     c("graphNEL","matrix","dgCMatrix"))
    switch(cls,
           "graphNEL"={edgeListMAT(graphNEL2M(object), matrix=matrix)},
           "dgCMatrix"=,
           "matrix"={edgeListMAT( object, matrix=matrix )})
}

#' @rdname graph-edgeList
edgeListMAT <- function(adjmat, matrix=FALSE){
    ans <-
        if (issymMAT_(adjmat)){
            symMAT2ftM_( adjmat )
        } else {
            MAT2ftM_( adjmat )
    } 

    di <- dim(ans)
    ans <- colnames(adjmat)[ans]
    dim(ans) <- di

    if (!matrix){
        rowmat2list(ans)
    } else {
        ans
    }
}

#' @rdname graph-edgeList
nonEdgeList <- function(object, matrix=FALSE)
  UseMethod("nonEdgeList")

#' @rdname graph-edgeList
nonEdgeList.default <- function(object, matrix=FALSE){
    cls <- match.arg(class( object ),
                     c("graphNEL","matrix","dgCMatrix"))
    switch(cls,
           "graphNEL"={nonEdgeListMAT(graphNEL2M(object), matrix=matrix)},
           "dgCMatrix"=,
           "matrix"={nonEdgeListMAT( object, matrix=matrix )})

}

#' @rdname graph-edgeList
nonEdgeListMAT <- function(adjmat, matrix=FALSE){
    if (!issymMAT_( adjmat )) stop("'adjmat' must be symmetric")
    if (class(adjmat) == "dgCMatrix"){
        adjmat <- as( ((-1*adjmat) + 1), "dgCMatrix")
    } else {
        adjmat <- -1 * adjmat + 1
    }
    edgeListMAT( adjmat, matrix=matrix)
}

## ##########################################################
##
## vpar implemented for graphNEL, matrix and Matrix
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
#' dagNEL <- dag(~a:b:c + c:d:e, result="NEL")

#' vpar(dagMAT)
#' vpar(dagNEL)
#' vpar(dagMAT, getv=FALSE)
#' vpar(dagNEL, getv=FALSE)

#' ## Undirected graphs
#' ugMAT <- ug(~a:b:c + c:d:e, result="matrix")
#' ugNEL <- ug(~a:b:c + c:d:e, result="NEL")

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


vchi <- function(object, getv=TRUE, forceCheck=TRUE){
  UseMethod("vchi")
}

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


#' @rdname graph-vpar
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

#' @rdname graph-vpar
vchi.Matrix <- vchiMAT
#' @rdname graph-vpar
vchi.matrix <- vchiMAT

#' @rdname graph-vpar
vpar <- function(object, getv=TRUE, forceCheck=TRUE){
  UseMethod("vpar")
}

#' @rdname graph-vpar
vparMAT <- function(object, getv=TRUE, forceCheck=TRUE){
  if (forceCheck && !is.adjMAT(object))
    stop("Matrix is not adjacency matrix... \n")
  if (forceCheck && isSymmetric(object))
    stop("Graph is undirected; (v, pa(v)) does not exist...\n")

  vn <- rownames(object)
  out <- lapply(seq_along(vn),
                function(j) vn[c(j, which(object[, j]!=0))])
  names(out) <- vn

  if (!getv) # Don't want v, just pa(v)
      lapply(out, function(x)x[-1])
  else
      out
}


#' @rdname graph-vpar
vpar.graphNEL <- function(object, getv=TRUE, forceCheck=TRUE){
    if (forceCheck && graph::edgemode(object)=="undirected")
        stop("Graph is undirected; (v,pa(v)) does not exist...\n")

    ch <- graph::edges(object) ## Nodes and their children
    vn <- names(ch)
    tf <- lapply(seq_along(ch),
                 function(i)
                     names2pairsM( ch[[i]], vn[i],
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

#' @rdname graph-vpar
vpar.Matrix <- vparMAT
#' @rdname graph-vpar
vpar.matrix <- vparMAT


## #############################################################
##
## Get the cliques of an undirected graph
##
## #############################################################

#' @title Get cliques of an undirected graph
#' 
#' @description Return a list of (maximal) cliques of an undirected graph.
#'
#' @name graph-cliques
#' 
#' @details In graph theory, a clique is often a complete subset of a graph. A maximal
#' clique is a clique which can not be enlarged. In statistics (and that is the
#' convention we follow here) a clique is usually understood to be a maximal
#' clique.
#' 
#' Finding the cliques of a general graph is an NP complete problem. Finding
#' the cliques of triangualted graph is linear in the number of cliques.
#' 
#' The workhorse is the \code{maxCliqueMAT} function which calls the
#' \code{maxClique} function in the \code{RBGL} package.
#' 
#' @aliases getCliques getCliques.graphNEL getCliques.default maxCliqueMAT
#' @param object An undirected graph represented either as a \code{graphNEL}
#'     object, a (dense) \code{matrix}, a (sparse) \code{dgCMatrix}
#' @param amat An adjacency matrix.
#' @return A list.
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @seealso \code{\link{ug}}, \code{\link{dag}}, \code{\link{mcs}},
#'     \code{\link{mcsMAT}}, \code{\link{rip}}, \code{\link{ripMAT}},
#'     \code{\link{moralize}}, \code{\link{moralizeMAT}}
#' @keywords utilities
#' @examples
#' 
#' ## graphNEL
#' uG1 <- ug(~a:b + b:c + c:d + d:e + e:f + f:a)
#' getCliques(uG1)
#' 
#' ## adjacency matrix
#' uG2 <- ug(~a:b + b:c + c:d + d:e + e:f + f:a, result="matrix")
#' getCliques(uG2)
#' 
#' ## adjacency matrix (sparse)
#' uG3 <- ug(~a:b + b:c + c:d + d:e + e:f + f:a, result="Matrix")
#' getCliques(uG3)
#' 
#' 
#' @export getCliques
getCliques <- function(object){
    UseMethod("getCliques")
}

#' @rdname graph-cliques
getCliques.graphNEL <- function(object){
    maxCliqueMAT( graphNEL2dgCMatrix(object) )[[1]]
}

#' @rdname graph-cliques
getCliques.default <- function(object){
    maxCliqueMAT(object)[[1]]
}


## FIXME: Should check that it is undirected.

#' @rdname graph-cliques
maxCliqueMAT <- function(amat){
  vn <- dimnames(amat)[[2L]]
  em <- t.default( MAT2ftM_( amat ) )
  RBGL::maxClique(nodes=vn, edgeMat=em)
}

## FIXME: getCliques.graphNEL; graphNEL2dgCMatrix
## FIXME: -> should be graphNEL2adjMAT combined with an
## FIXME: -> intelligent choice of representation


## ############################################
##
## Generate a random dag
##
## ############################################

#' @title Random directed acyclic graph
#' 
#' @description Generate a random directed acyclic graph (DAG)
#'
#' @name graph-randomdag
#' 
#' @details If the maximum number of parents for a node is, say 3 and wgt=0.1, then the
#' probability of the node ending up with 0,1,2,3 parents is proportional to
#' 0.1^0, 0.1^1, 0.1^2, 0.1^3.
#' 
#' @param V The set of vertices.
#' @param maxpar The maximum number of parents each node can have
#' @param wgt A parameter controlling how likely it is for a node to have a
#' certain number of parents; see 'Details'
#' @return A graphNEL object.
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @keywords utilities
#' @examples
#' 
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
        zz <- 0:(min(maxpar, length(rest))-1)
        if (min(zz)<0)
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
dual.rep <- function(glist, S, minimal=TRUE) {
    ## S is total varset - often but by no means always given by unique(unlist(g.list))
    list.save <- list()
    ##if (length(glist)==0) list.save <- list(S)
    if (length(glist)==1 & is.logical(glist[[1]]))
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
                list.save <- removeRedundant(aaa, FALSE)
            }
        }
        if (!minimal)
            list.save <- lapply(list.save, function(g) setdiffPrim(S, g))}
    list.save
}





