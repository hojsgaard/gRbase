## #######################################################################
##
#' @title Find edges in a graph and edges not in a graph.
#'
#' @description Returns the edges of a graph (or edges not in a graph)
#'     where the graph can be either an `igraph`
#'     object or an adjacency matrix.
#'
#' @name graph-edgeList
#'
## Issues: Check that it works on undirected and DAGs.
##
## #######################################################################

#' @param object An `igraph` object, a dense
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
edgeList <- function(object, matrix=FALSE) {
    UseMethod("edgeList")
}

#' @export
## #' @rdname graph-edgeList
edgeList.default <- function(object, matrix=FALSE) {
    if (inherits(object, c("igraph")))
        return(edgeListMAT(as(object, "matrix"), matrix=matrix))
    if (inherits(object, c("dgCMatrix", "matrix")))
        return(edgeListMAT(object, matrix=matrix))
    stop("Can not find edge list for this type of object\n")
}

#' @export
#' @rdname graph-edgeList
edgeListMAT <- function(adjmat, matrix=FALSE) {
    out <- if (issymMAT_(adjmat)) symMAT2ftM_(adjmat)
           else MAT2ftM_(adjmat)

    ## cat("edgeListMAT\n")
    ## print(adjmat)

    ## print(out)
    di  <- dim(out)
    ## print(di)
    out <- colnames(adjmat)[out]
    dim(out) <- di

    if (!matrix) rowmat2list__(out)
    else out
    
}

#' @export
#' @rdname graph-edgeList
nonEdgeList <- function(object, matrix=FALSE) {
    UseMethod("nonEdgeList")   
}

#' @export
## #' @rdname graph-edgeList
nonEdgeList.default <- function(object, matrix=FALSE) {
    if (inherits(object, c("igraph")))
        return(nonEdgeListMAT(as(object, "matrix"), matrix=matrix))
    if (inherits(object, c("dgCMatrix", "matrix")))
        return(nonEdgeListMAT(object, matrix=matrix))
    stop("Can not find non-edge list for this type of object\n")
}

#' @export
#' @rdname graph-edgeList
nonEdgeListMAT <- function(adjmat, matrix=FALSE){
    if (!issymMAT_(adjmat)) stop("'adjmat' must be symmetric")
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
#'     is chordal.
#' 
#' @title graph-ug2dag
#'
#' @param object An igraph object.

#' @export
#' @rdname graph-ug2dag
ug2dag <- function(object) { ## FIXME
    
    stopifnot_igraph(object)
    if (igraph::is_dag(object))
        return(object)
    m <- mcs(object)
    
    if (length(m) == 0) stop("Graph is not chordal")
    
    adj_lst <- lapply(m,
                      function(m_) {
                          gRbase::adj(object, m_)
                      })
    
    vpar_lst <- vector("list", length(m))
    names(vpar_lst) <- m
    
    vpar_lst[[1]] <- m[1]
    if (length(m) > 1) {
        for (i in 2:length(m)) {
            a <- intersect(adj_lst[[ i ]], m[ 1:i ])
            vpar_lst[[ i ]] <- c(m[ i ], a)
        }
    }
    dg <- dagList(vpar_lst)

    return(dg)
}







## ##########################################################
##
## vpar implemented for igraph, matrix and dgCMatrix
##
## ##########################################################

#' @title List of vertices and their parents for graph.
#'
#' @description Get list of vertices and their parents for graph.
#'
#' @name graph_vpar
#'
#' @param object An object representing a graph. Valid objects are an
#'     adjacency matrix or an igraph.
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
#' dag_mat <- dag(~a:b:c + c:d:e, result="matrix")
#' dag_ig <- dag(~a:b:c + c:d:e)

#' vpar(dag_mat)
#' vpar(dag_ig)
#' vpar(dag_mat, getv=FALSE)
#' vpar(dag_ig, getv=FALSE)

#' ## Undirected graphs
#' ug_mat <- ug(~a:b:c + c:d:e, result="matrix")
#' ug_ig <- ug(~a:b:c + c:d:e)

#' \dontrun{
#' ## This will fail because the adjacency matrix is symmetric and the
#' ## graph has undirected edges
#' vpar(ug_mat)
#' vpar(ug_ig)
#' }

#' ## When forceCheck is FALSE, it will not be detected that the
#' #g raphs are undirected.
#' vpar(ug_mat, forceCheck=FALSE)
#' vpar(ug_ig, forceCheck=FALSE)


#' @export
vchi <- function(object, getv=TRUE, forceCheck=TRUE) {
  UseMethod("vchi")
}

#' @export
#' @rdname graph_vpar
vchiMAT <- function(object, getv=TRUE, forceCheck=TRUE) {
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
## #' @rdname graph_vpar
vchi.igraph <- function(object, getv=TRUE, forceCheck=TRUE) {
    vchiMAT(as(object, "matrix"), getv=getv, forceCheck=forceCheck)
}

#' @export
## #' @rdname graph_vpar
vchi.Matrix <- vchiMAT

#' @export
## #' @rdname graph_vpar
vchi.matrix <- vchiMAT

#' @export
#' @rdname graph_vpar
vpar <- function(object, getv=TRUE, forceCheck=TRUE){
  UseMethod("vpar")
}

#' @export
#' @rdname graph_vpar
vparMAT <- function(object, getv=TRUE, forceCheck=TRUE) {
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
## #' @rdname graph_vpar
vpar.igraph <- function(object, getv=TRUE, forceCheck=TRUE){
    vpar(as_adjacency_matrix(object), getv=getv, forceCheck=forceCheck)
}

#' @export
## #' @rdname graph_vpar
vpar.Matrix <- vparMAT

#' @export
## #' @rdname graph_vpar
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
#' @param object An undirected graph represented either as an `igraph`
#'     object, a (dense) \code{matrix}, a (sparse) \code{dgCMatrix}
#' @param amat An adjacency matrix.
#' @return A list.
#' 
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @seealso \code{\link{ug}}, \code{\link{dag}}, \code{\link{mcs}},
#'     \code{\link{mcsMAT}}, \code{\link{rip}}, \code{\link{ripMAT}},
#'     \code{\link{moralize}}, \code{\link{moralizeMAT}}
#' @keywords utilities
#' @examples
#' uG0 <- ug(~a:b + b:c + c:d + d:e + e:f + f:a)
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
get_cliques.igraph <- function(object) {
    ## max_cliqueMAT(as(object, "matrix"))[[1]]
    out <- max_cliques(object)
    out <- lapply(out, attr, "names")    
    return(out)
}

#' @export 
get_cliques.default <- function(object) {
    if (inherits(object, c("matrix", "dgCMatrix"))){
        out <- get_cliques(as(object, "igraph"))
        return(out)
    } else {
        stop("'object' must be a matrix\n")
    }
}

## FIXME: Should check that it is undirected.
#' @export 
#' @rdname graph-clique
max_cliqueMAT <- function(amat) {
    get_cliques(as(amat, "igraph"))
}



#' @name graph-clique
#' @section Synonymous functions:
#'
#' For backward compatibility with downstream packages we have the
#' following synonymous functions:
#'
#' * getCliques = get_cliques
#'
#' * maxCliqueMAT = max_cliqueMAT 

#' @export
#' @rdname graph-clique
getCliques <- function(object) {
    out <- get_cliques(object)
    out
}   

#' @rdname graph-clique
#' @aliases maxCliqueMAT
#' @export
maxCliqueMAT  <- function(amat) {
    maxClique(as(amat, "igraph"))
}

#' @export
#' @rdname graph-clique
maxClique <- function(object) {
    out <- get_cliques(object)
    list(maxCliques=out)
}   




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
#' @return An igraph object.
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
random_dag <- function(V, maxpar=3, wgt=0.1) {
    V <- as.character(V)
    vpar_lst <- vector("list", length(V))
    names(vpar_lst) <- V
    for (ii in 1:length(V)){
        rest <- V[-(1:ii)]
        zz <- 0:(min(maxpar, length(rest)))
        ## zz <- 0:(min(maxpar, length(rest))-1)
        if (min(zz) < 0)
            zz <- 0
        pp <- wgt^zz
        npar <- sample(zz, 1, prob=pp)
        vpar_lst[[ii]] <- c(V[ii], sample(rest, npar, replace=FALSE))
    }
    dg <- dagList(vpar_lst)
    dg
}


#' @title Coerce dag to edge matrix
#
#' @description A DAG can be represented as a triangular matrix of regression coefficients. 
#' @name edge_matrix
#' 
#' @param object A graph, either an igraph object or an adjacency matrix.
#' @param edge_matrix Lower triangular matrix representing a dag
#' @param out Format of the output, can be 1, 2, 3 or 4.
#' @examples
#' g <- dag(~x2|x1 + x3|x1:x2 + x4|x3)
#' dag2edge_matrix(g, out=1)
#' dag2edge_matrix(g, out=2)
#' dag2edge_matrix(g, out=3)
#' dag2edge_matrix(g, out=4)
#' d2 <- dag(~c|a:b+d:c)
#' dag2edge_matrix(d2)
#'
#' @rdname edge_matrix
#' @export
dag2edge_matrix <- function(object, out=1) {
    stopifnot("Not igraph or adjacency matrix"=
                  inherits(object, c("igraph", "matrix", "Matrix")))
    if (inherits(object, c("matrix", "Matrix")))
        stopifnot("Not adjacency matrix"=is_adjMAT(object))
    
    to <- topoSort(object)
    stopifnot("Graph is not a DAG"= length(to) != 0)

    mm <- as(object, "matrix")
    nr <- nrow(mm)
    to <- topoSort(object)
    nms <- rownames(mm)
    idx <- match(to, nms)
    mm <- mm[idx,idx]
    nms <- nms[idx]
    
    nn1 <- outer(nms, nms, paste0)
    nn2 <- outer(1:nr, 1:nr, paste0)
    
    LL <- -t(mm)
    rownames(LL) <- colnames(LL) <- nms
    switch(out,
           "1"= {
               LL[LL != 0] <- nn1[LL != 0]
           },
           "2"={
               LL[LL != 0] <- paste0("-b_", nn1[LL != 0],"")
           },           
           "3"={
               LL[LL != 0] <- paste0("-b", nn2[LL != 0],"")               
           },
           "4"={
               LL[LL != 0] <- paste0("-b_", nn2[LL != 0],"")
           }           
           )
    
    diag(LL) <- 1
    LL
    ## list(L = LL, vn = nms)
   
}
#' @rdname edge_matrix
#' @export
edge_matrix2dag <- function(edge_matrix){
    d <- edge_matrix 
    d[d!="0"] <- 1
    diag(d) <- 0
    mode(d) <- "numeric"
    d <- t(d)
    igraph::graph_from_adjacency_matrix(d)
}










## #' @export
## dag2chol <- function(object) {
##     stopifnot("Not igraph or adjacency matrix"=
##                   inherits(object, c("igraph", "matrix", "Matrix")))
##     if (inherits(object, c("matrix", "Matrix")))
##         stopifnot("Not adjacency matrix"=is_adjMAT(object))
    
##     to <- topoSort(object)
##     ## print(object)
##     ## print(to)
##     stopifnot("Graph is not a DAG"= length(to) != 0)
    
##     vp <- vpar(object)[to]
  
##     vn <- names(vp)
##     Lo <- diag(1, length(vn))
##     rownames(Lo) <- colnames(Lo) <- vn
    
##     for (i in seq_along(vn)) {
##         pa <- vp[[i]][-1]
##         if (length(pa) > 0) {
##             vn[i]
##             idx <- match(pa, vn)
##             Lo[i, idx] <- paste0("-a", i, idx)
##         }
##     }
##     list(L=Lo, vn=vn)
## }








##
## SHD version of DED's dual rep; based on faster set operations
##

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
                m2 <- as.list( setdiff(S, glist[[v]]) )
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
            list.save <- lapply(list.save, function(g) setdiff(S, g))}
    list.save
}




