##################################################################
####
#### Find RIP-ordering of cliques of chordal (triangulated)
#### undirected graph
####
#### Based on Algorithm 4.11 in Steffen et all (the yellow book)
####
#### Known issues: Should check that amat specifies TUG;
#### possibly controlled by forceCheck argument
####
##################################################################

#' @title Create  RIP ordering of the cliques of  an undirected graph;
#'     create junction tree.
#' 
#' @description A RIP (running intersection property) ordering of the
#'     cliques is also called a perfect ordering. If the graph is not
#'     chordal, then no such ordering exists.
#' 
#' @details The RIP ordering of the cliques of a decomposable
#'     (i.e. chordal) graph is obtained by first ordering the
#'     variables linearly with maximum cardinality search (by
#'     \code{mcs}). The root argument is transfered to \code{mcs} as a
#'     way of controlling which clique will be the first in the RIP
#'     ordering.  The \code{jTree()} (and \code{jTree()}) (for
#'     "junction tree") is just a wrapper for a call of
#'     \code{triangulate()} followed by a call of \code{rip()}.
#'
#' @name graph-rip
#' 
#' @aliases rip rip.default ripMAT jTree jTree.default jTreeMAT
#'     junctionTree junctionTree.default junctionTreeMAT
#' @param object An undirected graph represented either as a
#'     \code{graphNEL} object, an \code{igraph}, a (dense)
#'     \code{matrix}, a (sparse) \code{dgCMatrix}.
#' @param root A vector of variables. The first variable in the
#'     perfect ordering will be the first variable on 'root'. The
#'     ordering of the variables given in 'root' will be followed as
#'     far as possible.
#' @param nLevels Typically, the number of levels of the variables
#'     (nodes) when these are discrete. Used in determining the
#'     triangulation using a "minimum clique weight heuristic". See
#'     section 'details'.
#' @param amat Adjacency matrix
#' @param ... Additional arguments; currently not used
#' @return \code{rip} returns a list (an object of class
#'     \code{ripOrder}. A print method exists for such objects.)
#' 
#' @note The workhorse is the \code{ripMAT()} function. The
#'     \code{nLevels} argument to the \code{rip} functions has no
#'     meaning.
#' 
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#'
#' @seealso \code{\link{mcs}} \code{\link{triangulate}}
#'     \code{\link{moralize}} \code{\link{ug}}, \code{\link{dag}}
#'
#' @keywords utilities
#' @examples
#' 
#' ## graphNEL
#' uG <- ug(~me:ve + me:al + ve:al + al:an + al:st + an:st)
#' mcs(uG)
#' rip(uG)
#' jTree(uG)
#' 
#' ## Adjacency matrix
#' uG <- ug(~me:ve:al + al:an:st, result="matrix")
#' mcs(uG)
#' rip(uG)
#' jTree(uG)
#' 
#' ## Sparse adjacency matrix
#' uG <- ug(c("me", "ve", "al"), c("al", "an", "st"), result="Matrix")
#' mcs(uG)
#' rip(uG)
#' jTree(uG)
#' 
#' ## Non--decomposable graph
#' uG <- ug(~1:2 + 2:3 + 3:4 + 4:5 + 5:1)
#' mcs(uG)
#' rip(uG)
#' jTree(uG)
#' 
#' 

#' @rdname graph-rip
rip <- function(object, ...){
  UseMethod("rip")
}

#' @rdname graph-rip
rip.default <- function(object, root=NULL, nLevels=NULL, ...){
    cls <- match.arg(class( object ),
                     c("graphNEL", "igraph", "matrix", "dgCMatrix"))
    switch(cls,
           "dgCMatrix"=,
           "matrix"   ={
               ripMAT(object, root=root, nLevels=nLevels)
           },
           "graphNEL" ={
               if (graph::edgemode(object)=="directed")
                   stop("Graph must be undirected")
               ripMAT(graphNEL2dgCMatrix(object), root=root, nLevels=nLevels)
           },
           "igraph" ={
               if (igraph::is.directed(object))
                   stop("Graph must be undirected")               
               ripMAT(igraph::get.adjacency(object), root=root, nLevels=nLevels)
           }
           )
}

#' @rdname graph-rip
ripMAT <- function(amat, root=NULL, nLevels=rep(2, ncol(amat))){

    #cat("ripMAT\n")
    
    ## mcs.idx: The enumeration of nodes in vn
    ## so 1,2,8 ... means that nodes 1,2,8... come first in the elimination
    ## mcs.vn: corresponding ordering of nodes

  mcs.vn <- mcsMAT(amat, root=root)
  if (length(mcs.vn)==0)
    return( NULL )

  cq.vn   <- maxCliqueMAT(amat)[[1]]
  ncq     <- length(cq.vn)
  vn      <- colnames(amat)

  if (ncq>1){
    ## cq.idx       <- lapplyV2I(cq.vn, vn)
    cq.mcs.idx   <- lapplyV2I(cq.vn, mcs.vn)
    max.no       <- sapply(cq.mcs.idx, function(cc) max(cc))
    ## max.no: The highest number assigned to a clique by MCS
    cq.mcs.idx   <- cq.mcs.idx[ order(max.no) ]
    ## cq.mcs.idx: Order the cliques by the largest number
    cq.mcs.vn    <- lapplyI2V( cq.mcs.idx, mcs.vn )

    sp.idx      <- vector("list", ncq)
    sp.idx[[1]] <- integer(0)
    pa.idx      <- rep.int(0L, ncq)

    ## Use: cq.mcs.idx
    for (ii in 2:ncq){
      paset  <- unlist(cq.mcs.idx[ 1:(ii-1L) ])
      isect  <- intersectPrim( cq.mcs.idx[[ii]], paset )
      sp.idx[[ii]] <- isect
      if (length(isect)){
        for (kk in (ii-1):1){
          if (subsetof( isect, cq.mcs.idx[[kk]]) ){
            pa.idx[ii] <- kk
            break()
          }
        }
      }
    }
    nodes <- mcs.vn
    sp.vn <- lapplyI2V(sp.idx, mcs.vn)
    child <- match(seq_along(cq.mcs.idx), pa.idx)
    host  <- rep.int(0L, length(nodes))
    ll    <- lapply(cq.mcs.vn, match, nodes)
    for (ii in seq_along(ll)){
      host[ll[[ii]]] <- ii
    }
  } else { ## The graph has only one clique!
    cq.mcs.vn <- list(vn)
    nodes  <- vn
    pa.idx <- 0L
    sp.vn  <- list(character(0))
    child  <- NA
    host  <- rep.int(1L, length(nodes))
  }

  .getChildList <- function(parents){
    vv  <- 1:length(parents)
    ft  <- unname(cbind(parents, vv))
    ft  <- ft[ft[, 1] != 0, , drop = FALSE]
    vch <- lapply(vv, function(ff) ft[ft[,1]==ff,2])
    names(vch) <- vv
    vch
  }
  ## FIXME: Create ftM2vch and ftM2vpar from the above

  rip3 <-
    structure(list(nodes       = nodes,
                   cliques     = cq.mcs.vn,
                   separators  = sp.vn,
                   parents     = pa.idx,
                   children    = child,
                   host        = host,
                   nLevels     = nLevels,
                   #createGraph = .createJTreeGraph,
                   childList   = .getChildList(pa.idx)
                   ),
              class="ripOrder")

  return(rip3)
}

## @rdname graph-rip
## @param x Object to be printet or plottet
print.ripOrder <- function(x, ...){
  idx <- 1:length(x$cliques)
  cat("cliques\n")
  mapply(function(xx,ii) cat(" ",ii,":",paste(xx, collapse=' '),"\n"), x$cliques, idx)

  cat("separators\n")
  mapply(function(xx,ii) cat(" ",ii,":",paste(xx, collapse=' '),"\n"), x$separators, idx)

  cat("parents\n")
  mapply(function(xx,ii) cat(" ",ii,":",paste(xx, collapse=' '),"\n"), x$pa, idx)

#  cat("Children\n")
#  mapply(function(xx,ii) cat(" ",ii,paste(xx, collapse=' '),"\n"), x$ch, idx)
}


## @rdname graph-rip
plot.ripOrder <- function(x, ...){
    .createJTreeGraph <- function(rip){
        if (length(rip$cliques)>1){
            ft <-cbind(rip$parents, 1:length(rip$parents))
            ft <- ft[ft[,1]!=0,, drop=FALSE]
            V <- seq_along(rip$parents)
            if (nrow(ft)==0){
                jt <- new("graphNEL", nodes = as.character(V), edgemode = "undirected")
            } else {
                jt <- graph::ftM2graphNEL(ft, V=as.character(V), edgemode="undirected")
            }
        } else {
            jt <- new("graphNEL", nodes = "1", edgemode = "undirected")
        }
        return(jt)
    }

    plot( .createJTreeGraph( x ) )
    ##plot(x$createGraph(x))
}

.rip2dag<-function (rip) {
  if (length(rip$cliques) > 1) {
    ft <- cbind(rip$parents, 1:length(rip$parents))
    ft <- ft[ft[, 1] != 0, , drop = FALSE]
    V  <- seq_along(rip$parents)
    if (nrow(ft) == 0) {
      jt <- new("graphNEL", nodes = as.character(V), edgemode = "directed")
    } else {
      jt <- graph::ftM2graphNEL(ft, V = as.character(V), edgemode = "directed")
    }
  } else {
    jt <- new("graphNEL", nodes = "1", edgemode = "directed")
  }
  return(jt)
}


#' @rdname graph-rip
jTree <- function(object, ...){
  UseMethod("jTree")
}

#' @rdname graph-rip
jTree.default <-  function(object, nLevels = NULL, ...){
    cls <- match.arg(class( object ),
                     c("graphNEL","igraph","matrix","dgCMatrix"))

    switch(cls,
           "graphNEL" ={jTreeMAT( graphNEL2dgCMatrix( object ), nLevels=nLevels, ... )},
           "igraph"   ={jTreeMAT( igraph::get.adjacency( object ), nLevels=nLevels, ... )},
           "dgCMatrix"=,
           "matrix"   ={jTreeMAT( object, nLevels=nLevels, ... )})
}

#' @rdname graph-rip
jTreeMAT <- function(amat, nLevels=rep(2,ncol(amat)), ...){
  tug  <- triangulateMAT( amat, nLevels=nLevels, result="Matrix", ... )
  ripMAT( tug, nLevels=nLevels )
}


## Copying
junctionTree          <- jTree
junctionTree.default  <- jTree.default
junctionTreeMAT       <- jTreeMAT


## jTree.graphNEL <- function(object, nLevels = NULL, ...){
##   jTreeMAT( graphNEL2dgCMatrix( object ), nLevels=nLevels, ... )
## }

## jTree.igraph <- function(object, nLevels = NULL, ...){
##   jTreeMAT( igraph::get.adjacency( object ), nLevels=nLevels, ... )
## }

## jTree.matrix <- function(object, nLevels = NULL, ...){
##   jTreeMAT( object, nLevels=nLevels, ... )
## }

## jTree.Matrix <- jTree.matrix

## junctionTree.graphNEL <- jTree.graphNEL
## junctionTree.igraph   <- jTree.igraph
## junctionTree.matrix   <- jTree.matrix
## junctionTree.Matrix   <- jTree.Matrix






























## jTree.graphNEL <- function(object, method  = "mcwh",
##                   nLevels = rep(2,length(nodes(object))), ...){

##   method <- match.arg(tolower(method),c("mcwh","r"))
##   tug        <- triangulate(object, method=method, nLevels=nLevels, result="Matrix")
##   val        <- ripMAT(tug,nLevels=nLevels)
##   return(val)
## }

## jTree.matrix <- function(object, method  = "mcwh",
##                          nLevels = rep(2,ncol(object)), ...){

##   method <- match.arg(tolower(method),c("mcwh","r"))
##   tug        <- triangulateMAT(object, method=method, nLevels=nLevels, result="Matrix")
##   val        <- ripMAT(tug,nLevels=nLevels)
##   return(val)
## }

## jTree.Matrix <- function(object, method  = "mcwh",
##                          nLevels = rep(2,ncol(object)), ...){

##   method <- match.arg(tolower(method),c("mcwh","r"))
##   tug        <- triangulateMAT(object, method=method, nLevels=nLevels, result="Matrix")
##   val        <- ripMAT(tug,nLevels=nLevels)
##   return(val)
## }

## jTree.igraph <- function(object,  method  = "mcwh",
##                   nLevels = rep(2,length(V(object))), ...){

##   method <- match.arg(tolower(method),c("mcwh","r"))
##   tug        <- triangulate(object, method=method, nLevels=nLevels, result="matrix")
##   val        <- ripMAT(tug,nLevels=nLevels)
##   return(val)
## }







