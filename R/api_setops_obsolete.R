## #############################################################
##
#' @title Suite of set operations
#' @description Set operations for gRbase and related packages.
#' @name set-operations-obsolete
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
##
## #############################################################
#' 
#' @param x Vector representing a set.
#' @param setlist List of vectors (representing a set of subsets)
#' @param maximal Logical; see section 'Details' for a description.
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
#' @examples
#'
#' set <- list(c(1, 2), c(1, 2, 3), c(2, 3, 6), c(2, 4), c(5, 6), 5)            
#'                                                             
#' el1 <- c(2, 1)                                               
#' el2 <- c(2, 3)                                               
#' el3 <- c(4, 3)                                               
#' el4 <- c(2, 1, 3)                                             
#'                                                             
#' maximal_sets_old(set)                                           
#' minimal_sets_old(set)                                           
#'                                                             
#' remove_redundant_old(set)                                       
#' remove_redundant_old(set, maximal=FALSE)                        
#'                                                             
#' is_inset_old(el1, set)                                          
#' is_inset_old(el2, set)                                          
#' is_inset_old(el3, set)                                          
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
#' 
#' is_subsetof(el1, el1)                                       
#' is_subsetof(el1, el2)                                       
#' is_subsetof(el1, el4)
#' 


#' @export
#' @rdname set-operations-obsolete
maximal_sets_old <- function(setlist, index=FALSE){
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
#' @rdname set-operations-obsolete
minimal_sets_old <- function(setlist, index=FALSE){
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
#' @rdname set-operations-obsolete
remove_redundant_old <- function(setlist, maximal=TRUE, index=FALSE){
  if (maximal) maximal_sets_old(setlist, index)
  else minimal_sets_old(setlist, index)
}

## Is x contained in any vector in setlist;
#' @export
#' @rdname set-operations-obsolete
is_inset_old <- function(x, setlist, index=FALSE){
  .isin_old(setlist, x, index)
}

.isin_old <- function(setlist, x, index=FALSE){
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
