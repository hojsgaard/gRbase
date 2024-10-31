#####################################################################
#'
#' @title Check if object is array
#' @description Check if object is array (that it is a vector with a
#'     dim attribute) and that the object has dimnames and that
#'     dimnames are named.
#' @name api-array-properties
#' @author Søren Højsgaard, \email{sorenh@@math.aau.dk}
#' 
#####################################################################
#' 
#' @param obj Some R object.
#' @param a1,a2 Arrays with named dimnames.
#' 
## #' @seealso \code{\link{is_named_array}}
#' @examples
#' is_named_array( HairEyeColor )
#' is_named_array( matrix(1:4, nrow=2) )
#' is_named_array_( HairEyeColor )
#' is_named_array_( matrix(1:4, nrow=2) )
#' is_number_vector_(1:4)
#' is_number_vector_(list(1:4))
#' 
#' ar1 = tab_new(c("a", "b"), levels=c(2, 3))
#' ar2 = tab_new(c("c", "a"), levels=c(2, 2))
#' ar1
#' ar2
#' ## dimension a has levels a1,a2 in both ar1 and ar2.
#' # Hence we have a match.
#' dimnames_match(ar1, ar2)
#' 
#' ar1 = tab_new(c("a", "b"), levels=c(2, 3))
#' ar2 = tab_new(c("c", "a"), levels=c(2, 3))
#' ar1
#' ar2
#' ## dimension a has levels a1,a2 in ar1 and levels a1,a2,a3 in ar2.
#' # Hence we do not have a match.
#' dimnames_match(ar1, ar2)
#' 
#' ar2 = tab_new(c("c", "a"), levels=list(c=c("c1", "c2"), a=c("a2", "a1")))
#' ar2
#' ## dimension a has levels a1,a2 in ar1 and levels a2,a1 in ar2.
#' # Hence we do not have a match.
#' dimnames_match(ar1, ar2)

#' @export
#' @rdname api-array-properties
is_named_array <- is_named_array_

## FIXME: Needed for gRain
#' @export  
#' @rdname api-array-properties
is.named.array <- is_named_array_

#' @export
#' @rdname api-array-properties
is_named_array_ <- is_named_array_

#' @export
#' @rdname api-array-properties
is_number_vector_ <- is_number_vector_

#' @export
#' @rdname api-array-properties
is_dimnames_ <- is_dimnames_

#' @export
#' @rdname api-array-properties   
dimnames_match <- function( a1, a2 ){
    if ( !is_named_array( a1 ) )
        stop("'a1' is not a named array \n")
    if ( !is_named_array( a2 ) )
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

