## ###################################################################
##
## Maximum Cardinality Search (mcs)
## Maximum Cardinality Search for marked graphs (mcsmarked)
##
## Returns perfect ordering if it exists and character(0) otherwise
##
## ###################################################################

#' @title Maximum cardinality search on undirected graph.
#' 
#' @description Returns (if it exists) a perfect ordering of the
#'     vertices in an undirected graph.
#'
#' @name graph-mcs
#' 
#' @details An undirected graph is decomposable iff there exists a
#'     perfect ordering of the vertices. The maximum cardinality
#'     search algorithm returns a perfect ordering of the vertices if
#'     it exists and hence this algorithm provides a check for
#'     decomposability. The \code{mcs()} functions finds such an
#'     ordering if it exists.
#' 
#'     The notion of strong decomposability is used in connection
#'     with e.g. mixed interaction models where some vertices
#'     represent discrete variables and some represent continuous
#'     variables. Such graphs are said to be marked. The
#'     \code{mcsmarked()} function will return a perfect ordering iff
#'     the graph is strongly decomposable. As graphs do not know about
#'     whether vertices represent discrete or continuous variables,
#'     this information is supplied in the \code{discrete} argument.
#' 
#' @aliases mcs mcs.default mcsMAT mcsmarked mcsmarked.default
#'     mcsmarkedMAT
#' @param object An undirected graph represented either as a
#'     \code{graphNEL} object, an \code{igraph}, a (dense)
#'     \code{matrix}, a (sparse) \code{dgCMatrix}.
#' @param root A vector of variables. The first variable in the
#'     perfect ordering will be the first variable on 'root'. The
#'     ordering of the variables given in 'root' will be followed as
#'     far as possible.
#' @param discrete A vector indicating which of the nodes are
#'     discrete. See 'details' for more information.
#' @param index If TRUE, then a permutation is returned
#' @param amat Adjacency matrix
#' @param vn Nodes in the graph given by adjacency matrix
#' @return A vector with a linear ordering (obtained by maximum
#'     cardinality search) of the variables or character(0) if such an
#'     ordering can not be created.
#' @note The workhorse is the \code{mcsMAT} function.
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' @seealso \code{\link{moralize}}, \code{\link{jTree}},
#'     \code{\link{rip}}, \code{\link{ug}}, \code{\link{dag}}
#' @keywords utilities
#' @examples
#' 
#' uG <- ug(~ me:ve + me:al + ve:al + al:an + al:st + an:st)
#' mcs(uG)
#' mcsMAT(as(uG, "matrix"))
#' ## Same as
#' uG <- ug(~ me:ve + me:al + ve:al + al:an + al:st + an:st, result="matrix")
#' mcsMAT(uG)
#' 
#' ## Marked graphs
#' uG1 <- ug(~ a:b + b:c + c:d)
#' uG2 <- ug(~ a:b + a:d + c:d)
#' ## Not strongly decomposable:
#' mcsmarked(uG1, discrete=c("a","d"))
#' ## Strongly decomposable:
#' mcsmarked(uG2, discrete=c("a","d"))
#' 
#' @export mcs
mcs <- function(object, root=NULL, index=FALSE){
  UseMethod("mcs")
}

## FIXME: mcs: returns character(0) if graph is not undirected. Should
## FIXME: mcs: instead signal an error??

#' @rdname graph-mcs
mcs.default <- function(object, root=NULL, index=FALSE){
    cls <- match.arg(class( object ),
                     c("graphNEL", "matrix", "dgCMatrix", "igraph"))
    mm <- coerceGraph(object, "matrix")
    if (!is.UGMAT(mm))
        character(0) ##FIXME: mcs.default: Should perhaps be error...
    else
        mcsMAT( mm, root=root, index=index )
}

#' @rdname graph-mcs
mcsMAT <- function (amat, vn = colnames(amat), root = NULL, index = FALSE)
{
    vn.old <- vn
    if (!is.null(root)){
        vn    <- c(root, setdiffPrim(vn, root))
        root2 <- match(vn, vn.old) - 1
    } else {
        root2 <- 0:(ncol(amat) - 1)
    }
    ##cat("mcsMAT:"); print(root2)
    a <- mcsMAT__( amat, root2 )

    if (index){
        if (a[1] < 0){
            NA
        } else {
            a + 1
        }
    } else {
        if (a[1] < 0){
            character(0)
        } else {
            vn.old[a + 1]
        }
    }
}


#' @rdname graph-mcs
mcsmarked <- function (object, discrete=NULL, index = FALSE){
  UseMethod("mcsmarked")
}

#' @rdname graph-mcs
mcsmarked.default <- function (object, discrete=NULL, index = FALSE){
    cls <- match.arg(class( object ),
                     c("graphNEL","igraph","matrix","dgCMatrix"))
    switch(cls,
           "graphNEL" ={
               if (is.null(discrete))
                   mcsMAT(gn2sm_(object), index=index)
               else
                   mcsmarkedMAT(gn2sm_(object), discrete=discrete, index = index)
           },
           "igraph"   ={
               if (is.null(discrete))
                   mcsMAT(ig2sm_(object), index=index)
               else
                   mcsmarkedMAT(ig2sm_(object), discrete=discrete, index = index)
           },
           "dgCMatrix"=,
           "matrix"   ={
               if (is.null(discrete))
                   mcsMAT(object, index=index)
               else
                   mcsmarkedMAT(object, discrete=discrete, index = index)
           })
}

## FIXME: mcsmarkedMAT: candidate for C++ implementation.

#' @rdname graph-mcs
mcsmarkedMAT <- function(amat, vn = colnames(amat), discrete = NULL, index = FALSE) {

    nv   <- length(vn)

    if (is.null(discrete)){
        return(mcsMAT(amat, vn=vn, index=index))
    } else {
        if (is.logical(discrete)){
            discrete <- as.numeric(discrete)
        }
        if (is.numeric(discrete)){
            if ( length(discrete) != nv ){
                stop("'discrete' is numeric or logical but does not have the correct length")
            } else {
                vn.ext <- c(".", vn)
                idx <- c(1, discrete)
            }
        } else {
            if (is.character(discrete)){
                vn.ext <- c(".", vn)
                idx <- match(discrete, vn.ext)
            } else {
                stop ("'discrete' is not a character")
            }
        }
    }

    amat.ext <- as(Matrix(0, nrow=nv+1L, ncol=nv+1L), "dgCMatrix")
    amat.ext[2:(nv+1),2:(nv+1)] <- amat
    amat.ext[idx, 1L] <- 1L
    amat.ext[1L, idx] <- 1L
    ans <- mcsMAT(amat.ext, vn=vn.ext, index=index)
    if (length(ans)>0)
        ans <- ans[-1L]
    ans
}


