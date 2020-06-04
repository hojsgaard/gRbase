## #############################################################
##
#' @title Suite of set operations
#' @description Set operations for gRbase and related packages.
#' @name set_operations
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
##
## #############################################################
#' 
#' @param x,set,set2 Vector representing a set.
#' @param setlist List of vectors (representing a set of subsets)
#' @param maximal Logical; see section 'Details' for a description.
#' @param all Logical; see section 'Details' for a description.
#' @param index Logical; should indices (in setlist) be returned or a
#'     set of subsets.
#'
#' @details
#'
#'  'setlist' is a list of vectors representing a set of subsets;
#'  i.e. V1,...VQ where Vk is a subset of some base set V.
#'
#'  'all' If true, \code{get_superset} will return index of all
#'  vectors containing the element; otherwise only the first index is
#'  returned.
#'
#'  \code{is_inset}: Checks if the set
#'  x is in one of the Vk's.
#'
#'  \code{remove_redundant}: Returns those Vk which are not contained
#'  in other subsets; i.e. gives the maximal sets. If maximal is FALSE
#'  then returns the minimal sets; i.e. Vk is returned if Vk is
#'  contained in one of the other sets Vl and there are no set Vn
#'  contained in Vk.
#'  
#'  Notice that the comparisons are made by turning the elements into
#'  characters and then comparing these. Hence 1 is identical to "1".
#'
#' 
#' @examples
#'
#' set <- list(c(1, 2), c(1, 2, 3), c(2, 3, 6), c(2, 4), c(5, 6), 5)            
#'                                                             
#' el1 <- c(2, 1)                                               
#' el2 <- c(2, 3)                                               
#' el3 <- c(4, 3)                                               
#' el4 <- c(2, 1, 3)                                             
#'                                                             
#' maximal_sets(set)                                           
#' minimal_sets(set)                                           
#'                                                             
#' remove_redundant(set)                                       
#' remove_redundant(set, maximal=FALSE)                        
#'                                                             
#' is_inset(el1, set)                                          
#' is_inset(el2, set)                                          
#' is_inset(el3, set)                                          
#'                                                             
#' get_subset(el1, set)
#' get_subset(el1, set)                                        
#' get_subset(el2, set)                                        
#' get_subset(el3, set)                                        
#'
#' get_superset(el1, set)                                      
#' get_superset(el1, set, all=TRUE)                                      
#' get_superset(el2, set)                                      
#' get_superset(el3, set)                                      
## #'                                                             
## #' is_subsetof(el1, el1)                                       
## #' is_subsetof(el1, el2)                                       
## #' is_subsetof(el1, el4)
#' 


#' @export
#' @rdname set_operations
maximal_sets <- function(setlist, index=FALSE){
    if (length(setlist)<=1){
        if (index) return(1)
        else return(setlist)
    }
    
    lenx     <- c(lapply(setlist,length), recursive=TRUE)
    ooo      <- order(lenx, decreasing=TRUE)
    setlist2 <- setlist[ooo]
    ends     <- cumsum(c( lapply(setlist2,length), recursive=TRUE ))
    iii<-.C("C_maxset",
            setlist=as.character(c(setlist2, recursive=TRUE)),
            ends=ends, nset=length(setlist2), ans=integer(length(setlist2))
          , PACKAGE="gRbase")$ans
    iii <- iii[order(ooo)]
    
    if (index){
        iii
    } else {
        setlist[iii==1]
    }
}

#' @export
#' @rdname set_operations
minimal_sets <- function(setlist, index=FALSE){
    if (length(setlist) <= 1){
        if (index) return(1)
        else return(setlist)
    }
    
    lenx     <- c(lapply(setlist,length), recursive=TRUE)
    ooo      <- order(lenx, decreasing=FALSE)
    setlist2 <- setlist[ooo]
    ends     <- cumsum(c( lapply(setlist2,length), recursive=TRUE ))
    iii<-.C("C_minset",
            setlist=as.character(c(setlist2, recursive=TRUE)),
            ends=ends, nset=length(setlist2), ans=integer(length(setlist2))
          , PACKAGE="gRbase")$ans
    iii <- iii[order(ooo)]
    
    if (index){
        iii
    } else {
        setlist[iii==1]
    }
}

## A function to remove redundant generators.  If maximal=T, returns
## the maximal generators, if =F, the minimal generators.
## Can be speeded up if the as.character part can be avoided...

#' @export
#' @rdname set_operations
remove_redundant <- function(setlist, maximal=TRUE, index=FALSE){
  if (maximal) maximal_sets(setlist, index)
  else minimal_sets(setlist, index)
}

## Is x contained in any vector in setlist;
#' @export
#' @rdname set_operations
is_inset <- function(x, setlist, index=FALSE){
  .isin(setlist, x, index)
}

.isin <- function(setlist, x, index=FALSE){
    len.setlist <- length(setlist)
    if (len.setlist == 0){
        if (index) return(0)
        else return(FALSE)
    }

    ## FIXME: Looks strange
    if (len.setlist < 1){
        if (index) return(rep(1, length(x)))
        else return(TRUE)
    }
    
    ll    <- cumsum(c(lapply(setlist, length), recursive=TRUE))
    iii<-.C("C_isin",
            as.character(x), length(x),
            as.character(c(setlist, recursive=TRUE)), ll, len.setlist,
            ans=integer(len.setlist)
          , PACKAGE="gRbase")$ans
    
    if (index) { return(iii) }
    else { return(any(iii)) }
}

#' @export
#' @rdname set_operations
get_subset <- get_subset_

#' @export
#' @rdname set_operations
get_superset <- get_superset_

#' @export
#' @rdname set_operations
is_subsetof <- is_subsetof_


## FIXME: is.subsetof : Use Rcpp implementation

#' @export
#' @rdname set_operations
is.subsetof <- function(x, set){
  all(match(x, set) > 0)
}

## ###################################################################
##
#' @title Create all subsets
#' @description Create all subsets of a vector
#' @name all_subsets
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
##
## Issues: deprecate allSubsets; use all_subsets instead
##
## ###################################################################
#' @param x Vector

#' @export
#' @rdname all-subsets
all_subsets <- allSubsets_

#' @export
#' @rdname all-subsets
all_subsets0 <- allSubsets0_


.all_subsets0 <- function(x) {
    y <- list(vector(mode(x), length = 0))
    for (i in seq_along(x)) {
        y <- c(y, lapply(y, "c", x[i]))
    }
    y[-1L]
}

.all_subsets <- function(x){
    out <- vector("list", length=2^length(x))
    ny = 1 # filled elements of out
    for (i in seq_along(x)){
        z = x[i]
        for (k in 1:ny){
            out[[ny + k]] = c(out[[k]],z)
        }
        ny = 2 * ny
    }
    out[-1]
}



## #' @rdname all-subsets
## #' @param g.sep Pick a value which is not in x
## allSubsets <- function(x, g.sep="+"){
##   if (length(x)==1)
##     return(x)
##   else {
##     val <- x[1]
##     for (i in 2:length(x)){
##       v <- paste(val,x[i],sep=g.sep)
##       val <- c(val,x[i],v)
##     }
##     val <- strsplit(val,paste("\\",g.sep,sep=""))
##     return(val)
##   }
## }





## ###################################################################
##
#' @title Create all possible pairs
#' @description Create all possible pairs of two character vectors.
#' @name all_pairs
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#'
## ###################################################################
#'
#' @param x,y Character vectors.
#' @param sort Logical.
#' @param result A list or a matrix.
#'
#' @details NOTICE: If y is not NULL then x and y must be disjoint (no
#'     checks are made); otherwise pairs of identical elements wil also be obtained. 
#'
#' @examples
#'
#' x <- letters[1:4]
#' y <- letters[5:7]
#'
#' all_pairs(x)
#' all_pairs(x, result="matrix")
#'
#' all_pairs(x, y)
#' all_pairs(x, y, result="matrix")

#' @export
#' @rdname all_pairs
all_pairs <- all_pairs__

## FIXME names2pairs should be deprecated and replaced by all_pairs
#' @export
#' @rdname all_pairs
names2pairs <- function(x, y=NULL, sort=TRUE, result="list"){
  result <- match.arg(result, c("list", "matrix"))
  lenx <- length(x)
  leny <- length(y)

  if (leny == 0){
    if (lenx == 1){
      if (result == "matrix")
        return(matrix(nrow=0, ncol=2))
      else
        return(list())
    } else {
      cc   <- combn_prim(1:length(x), 2)
      out  <- x[cc]
      dim(out) <- dim(cc)
      if (sort){
        idx <- out[1,] > out[2, ]
        out[1:2,idx] <- out[2:1, idx]
      }
      if (result == "matrix")
        return(t.default(out))
      else
        return(colmat2list(out))
    }
  } else {
    out <- cbind(rep(x, each=leny), rep(y, times=lenx))
    if (sort){
      idx <- out[,1] > out[,2]
      out[idx, 1:2] <- out[idx, 2:1]
    }

    if (identical(result, "matrix")) out 
    else rowmat2list__(out)
  }
}



