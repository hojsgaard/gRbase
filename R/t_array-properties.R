#' @title Check if object is array
#'
#' @description Check if object is array (that it is a vector with a
#'     dim attribute) and that the object has dimnames and that
#'     dimnames are named.
#'
#' @name array-properties
#' 
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk} 
#' 
#' @examples
#'
#' is.named.array( HairEyeColor )
#' is.named.array( matrix(1:4, nrow=2) )
#' is_named_array_( HairEyeColor )
#' is_named_array_( matrix(1:4, nrow=2) )
#' is_number_vector_(1:4)
#' is_number_vector_(list(1:4))
#' 
#' @param obj Some R object.
#' 

#' @rdname array-properties
is.named.array <- is_named_array_

##is.named.array <- function(obj){
##    is.array(obj) && !is.null(dimnames(obj)) && !is.null(names(dimnames(obj)))
##}

#' @rdname array-properties
#' 
is_named_array_ <- is_named_array_

#' @rdname array-properties
#' 
is_number_vector_ <- is_number_vector_

#' @rdname array-properties
#' 
is_dimnames_ <- is_dimnames_


#' @title Check compatibility of dimnames
#'
#' @description Check that the intersection of dimnames of two arrays are
#'     identical.
#'
#' @name array-dimnames
#'
#' @aliases dimnames_match_
#' 
#' @param a1,a2,tab1,tab2 Arrays with named dimnames.
#' @param verbose Should errors be printed.
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk} 
#' @seealso \code{\link{is.named.array}}
#' @examples
#' 
#' ar1 = newar(c("a","b"), levels=c(2,3))
#' ar2 = newar(c("c","a"), levels=c(2,2))
#' ar1
#' ar2
#' ## dimension a has levels a1,a2 in both ar1 and ar2.
#' # Hence we have a match.
#' dimnames_match(ar1, ar2)
#' 
#' ar1 = newar(c("a","b"), levels=c(2,3))
#' ar2 = newar(c("c","a"), levels=c(2,3))
#' ar1
#' ar2
#' ## dimension a has levels a1,a2 in ar1 and levels a1,a2,a3 in ar2.
#' # Hence we do not have a match.
#' dimnames_match(ar1, ar2)
#' 
#' ar2 = newar(c("c","a"), levels=list(c=c("c1","c2"), a=c("a2","a1")))
#' ar2
#' ## dimension a has levels a1,a2 in ar1 and levels a2,a1 in ar2.
#' # Hence we do not have a match.
#' dimnames_match(ar1, ar2)
## 

dimnames_match <- function( a1, a2 ){
    if ( !is.named.array( a1 ) )
        stop("'a1' is not a named array \n")
    if ( !is.named.array( a2 ) )
        stop("'a2' is not a named array \n")

    dn1 <- dimnames( a1 )
    dn2 <- dimnames( a2 )
    vn1 <- names( dn1 )
    vn2 <- names( dn2 )
    isect <- intersect( vn1, vn2 )
    if (length( isect ) == 0) TRUE
    else {
        chk <- lapply(seq_along(isect),
                      function(i) {
                          v <- isect[ i ]
                          identical(dn1[[v]], dn2[[v]])
                      } )        
        chk <- unlist(chk)
        all( chk )
    }   
}

#' @rdname array-dimnames
#' 
dimnames_match_ <- dimnames_match_ 

