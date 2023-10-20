#######################################################################
####
#### querygraph provides unified interface to graph operations.
####
#### Works on graphNEL objects, igraph objects, and adjacency matrices
####
#### Notice: when a graph is returned it is always a graphNEL object
####
#######################################################################

#' @title Query a graph
#'
#' @description Unified approach to query a graph about its properties
#'     (based partly on functionality from gRbase and functionality
#'     imported from RBGL).
#'
#' @name graph_query
#'
#' @param object A graph.
#' @param op The operation or query.
#' @param set,set2,set3 Sets of nodes in graph.
#'
#' @examples
#'
#' ug0 <- ug(~a:b + b:c:d + e)
#'
#' separates("a", "d", c("b", "c"), ug0)
#' separates("a", "d", "c", ug0) 
#' is.simplicial("b", ug0)
#' simplicialNodes(ug0)

#' @export
querygraph <-function(object, op, set=NULL, set2=NULL, set3=NULL) {

  gRbase <-
    c("ancestors",
      "ancestralGraph",
      "ancestralSet",
      "children",
      "closure",
      "edgeList",
      "is.decomposition",
      "is.complete",
      "is.simplicial",
      "parents",
      "simplicialNodes",      
      "separates",
      "vpar")
    op <- match.arg(op, choices=gRbase)
    object <- as(object, "igraph") 
    
    ## FIXME REDO WHAT IS COMMENTED OUT
    switch(op,
           ## Functions from graph/RBGL package here.
           ## "maxClique"=        { RBGL::maxClique(object)$maxCliques               },
           ## "connectedComp"=    { RBGL::connectedComp(object)                      },
           ## "separates"=        { RBGL::separates(set, set2, set3, object)         },
           ## "is.triangulated"=  { RBGL::is.triangulated(object)                    },
           ## "separates"=        { RBGL::separates(set, set2, set3, object)         },                  
           ## "adj"=              { graph::adj(object, set)                          },
           ## "subgraph"=         { graph::subGraph(set, object)                     },
           ## "nodes"=            { gRbase::nodes(object)                             },
           ## "edges"=            { graph::edges(object)                             },
           ## gRbase functions
           "nodes"=            { gRbase::nodes(object)                             },
           "ancestors"=,"an"=  { gRbase::ancestors(set, object)		        },
           "ancestralGraph"=   { gRbase::ancestralGraph(set, object)	        },
           "ancestralSet"=     { gRbase::ancestralSet(set, object)                },
           "children"=         { gRbase::children(set, object)         	        },
           "closure"=          { gRbase::closure(set, object)          	        },
           "edgeList"=         { gRbase::edgeList(object)	       		        },
           "is.decomposition"= { gRbase::is.decomposition(set, set2, set3, object)},
           "is.complete"=      { gRbase::is.complete(object, set)         	},
           "is.simplicial"=    { gRbase::is.simplicial(set, object)         	},
           "parents"=          { gRbase::parents(set, object)         		},
           "simplicialNodes"=  { gRbase::simplicialNodes(object)         	        },
           "separates"=        { gRbase::separates(set, set2, set3, object)       },         
           "vpar"=             { gRbase::vpar(object)         			}
           )
}


    ## From RBGL / graph packages
    ## graph.RBGL <-
    ##   c("maxClique",
    ##     "connectedComp",
    ##     "separates",
    ##     "is.triangulated",
    ##     "adj", ## FIXME ? nei 
    ##     "subgraph",
    ##     "nodes",
    ##     "edges")
    ## From gRbase
    
    ## op <- match.arg(op, choices=c(graph.RBGL, gRbase))
    ## object <- coerceGraph(object, "graphNEL") ## FIXME
    ##object <- coerceGraph(object, "igraph")

#' @export
#' @rdname graph_query
qgraph <- querygraph


########################################################################
###
### Functions which return vectors
###
########################################################################

## FIXME quick-fixme

g_gn2dm_ <- function(x){
    as_adjacency_matrix(x)
}

## 
## adjmat based
#' @export
#' @rdname graph_query
ancestors <- function(set, object) {
    stopifnot_igraph(object)
    if (!is_dag(object))
        return(NULL) 

    if (missing(set))
        stop("'set' must be given..\n")

    amat <- as_adjacency_matrix(object)

    An <- setorig <- set
    amat  <- amat[-match(set, rownames(amat)),]
    
    repeat {
        set2 <- rowSums(amat[,set, drop=FALSE])
        set  <- names(which(set2>0))
        if (!length(set))
            break()
        An <- c(An, set)
        amat  <- amat[set2 == 0,,drop=FALSE]
    }
    setdiff(An, setorig)
}



## graph::subGraph
#' @export
#' @rdname graph_query
subGraph <- function(set, object) {
    stopifnot_igraph(object)
    igraph::subgraph(object, set)    
}


## RBGL::is.triangulated
#' @export
#' @rdname graph_query
is.triangulated <- function(object) {
    length(mcs(object)) > 0
}



## RBGL::connectedComp
#' @export
#' @rdname graph_query
connComp <- function(object) {
    stopifnot_igraph(object)
    comp <- components(object)    
    cc <- unique(comp$membership)
    lapply(cc,
           function(cci) {
               names(which(cci == comp$membership))           
           }
           )
}


## adjmat based -- Must be very slow !!!
#' @export
#' @rdname graph_query
ancestralSet <- function(set, object) {
    stopifnot_igraph(object)
    if (!is_dag(object))
        return(NULL)

        
    if (missing(set))
        stop("'set' must be given..\n")

    amat <- as_adjacency_matrix(object)
    ## print(amat)
    vn   <- colnames(amat)
    an   <- rep(0, length(vn))
    names(an) <- vn
    an[set]   <- 1
    
    A0 <- set
    repeat {
        x <- amat[,A0,drop=FALSE]
        B <- rownames(x)[apply(x, 1, sum) > 0]
        if (!length(B))
            break()
        an[B] <- 1
        idx   <- match(A0, colnames(amat))
        amat  <- amat[-idx, -idx, drop=FALSE]
        vn    <- colnames(amat)
        A0    <- intersect(B, vn)
        if (!length(A0))
            break()
    }
    names(an[an > 0])
}

#' @export
#' @rdname graph_query
ancestralGraph <- function(set, object) {
    stopifnot_igraph(object)    
    ## graph::subGraph(ancestralSet(set, object), object)

    igraph::subgraph(object, ancestralSet(set, object))
}



## igraph based
#' @export
#' @rdname graph_query
parents <- function(set, object) {
    stopifnot_igraph(object)
    if (!is_dag(object))
        return(NULL)
    a <- as_adjacency_matrix(object)
    ## Parents
    m <- as(a[, set, drop=FALSE], "matrix")
    s <- rowSums(m)
    out <- names(which(s>0))
    return(out)    
}

## igraph based
#' @export
#' @rdname graph_query
children <- function(set, object) {
    stopifnot_igraph(object)
    if (!is_dag(object))
        return(NULL)
    a <- igraph::as_adjacency_matrix(object)
    ## Children
    m <- as(a[set,, drop=FALSE ], "matrix")
    s <- colSums(m)
    out <- names(which(s>0))
    return(out)
}


stopifnot_igraph <- function(object) {
    if (!is_igraph(object)) {
        stop("Not a graph object")
    }    
}


## RBGL::separates 
#' @export
#' @rdname graph_query
separates <- function(set, set2, set3, object) {
    stopifnot_igraph(object)
    if (is_dag(object))
        return(NULL)

    asp <- all_simple_paths(object, set, set2) 
    asp <- lapply(asp, function(v) attr(v, "names"))
    ss <- c(set, set2)
    bb <- set3
    seps <- lapply(asp, setdiff, ss)
    all(sapply(lapply(seps, intersect, bb), length) > 0)    
}



## graphNEL based
#' @export
#' @rdname graph_query
closure <- function(set, object){ 
  unique.default(c(set, unlist(adj(object, set)))) ## FIXME 
}




## graph::adj
#' @export
#' @rdname graph_query
adj <- function(object, set) {
    stopifnot_igraph(object)
    if (is_dag(object))  return(NULL)    

    out <- lapply(set, function(s) {
        attr(igraph::neighbors(object, s), "names")    
    }
    )
    names(out) <- set
    out
}


## #' @export
## #' @rdname graph_query
## is.simplicical <- function(set, object) {
##     stopifnot_igraph(object)
##     if (is_dag(object))  return(NULL)
    
## }

#' @export
#' @rdname graph_query
is.simplicial <- function(set, object) {
    ## x <- unlist(graph::adj(object,set))        ## FIXME
    ## is.complete(graph::subGraph(x, object), x) ## FIXME
    
    x <- unlist(adj(object, set))       
    if (length(x) == 0)
        return(TRUE)
    is.complete(subGraph(x, object), x) 
}




#' @rdname graph_query
#' @examples
#'
#' simplicialNodes(ug0)
#' @export
simplicialNodes <- function(object) {
    stopifnot_igraph(object)
    vn <- nodes(object) ## FIXME
    ## b     <- unlistPrim(lapply(nodes, function(s) is.simplicial(s, object)))
    
    aa <- lapply(vn, function(s) {
        is.simplicial(s, object)
    })
    
    
    b     <- unlist(aa)
    sim   <- vn[b]
    sim
}






########################################################################
###
### Boolan graph funcions (is.something)
###
########################################################################

#' @export
#' @rdname graph_query
is.complete <- function(object, set=NULL) {
    stopifnot_igraph(object)
    if (is_dag(object))
        return(NULL)

    if (length(V(object)) == 1)
        return(TRUE)
    
    if (length(set) == 1)
        return(TRUE)
    
    submat <- as_adjacency_matrix(object)
    if (!is.null(set))
        submat <- submat[set, set]

    submat <- as(submat, "matrix")
    all(submat[upper.tri(submat)] > 0)    
}





#' @export
#' @rdname graph_query
is.decomposition <- function(set, set2, set3, object) {
    stopifnot_igraph(object)
    if (is_dag(object)) return(NULL)

    vn <- uniquePrim(c(set, set2, set3))
    if (setequal(vn, nodes(object))) { ## FIXME
        separates(set, set2, set3, object) & is.complete(object, set3) 
    } else {
        FALSE
    }
}


#' @export
#' @rdname graph_query
nodes_ <- function(object) {
    stopifnot_igraph(object)
    out <- V(object)
    vv <- attr(out, "names")
    if (is.null(vv)) {
        as.character(out)
    } else {
        vv
    }
}


setGeneric("nodes", function(object, ...) standardGeneric("nodes"))

#' @export
#' @rdname graph_query
setMethod("nodes",
          signature(object = "igraph"),
          function (object, ...) 
          {
              nodes_(object)
          }
          )


#' @export
#' @rdname graph_query
edges <- function(object) {
    stopifnot_igraph(object)
    if (igraph::is_dag(object))
        mode <- "out"
    else
        mode <- "all"
    adjl <- as_adj_list(object, mode=mode)
    
    out <- lapply(adjl, function(d) {        
        vv <- attr(d, "names")
        if (is.null(vv))
            vv <- as.character(d)
        vv
    })
    names(out) <- nodes(object)
    return(out)
}

#' @export
#' @rdname graph_query
edges_ <- edges

#' @export
#' @rdname graph_query
#' @param v1,v2 Vertex names
addEdge <- function(v1, v2, object) {
    object2 <- igraph::add_edges(object, c(v1, v2))
    object2
}

#' @export
#' @rdname graph_query
removeEdge <- function(v1, v2, object) {
    object2 <- igraph::delete_edges(object, paste0(v1, "|", v2))
    object2
}
