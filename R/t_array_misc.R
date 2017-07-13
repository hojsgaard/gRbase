## ###################################################################
##
## Miscellaneous array functions
##
## ###################################################################

## ###################################################################
#' @title Convert dataframe to contingency table
#'
#' @description: Much like xtabs but with more flexibility
#'
#' @name df2xtabs
## ###################################################################
#' 
#' @param indata A dataframe.
#' @param names Names of variables defining table; a character vector or a right
#'     hand sided formula.
#' @param normalize Either "none", "first" or "all". Should result be
#'     normalized, see 'Details' below.
#' @param smooth Should values be smoothed, see 'Details' below.
#' 
#' @examples
#' ## Extract arrays from dataframe (much like xtabs() but with more flexibility)
#' data(cad1) 
#' df2xtabs(cad1, ~Sex:AngPec:AMI)
#' df2xtabs(cad1, c("Sex", "AngPec", "AMI"))
#' df2xtabs(cad1, c(1, 2, 3))

df2xtabs <- function(indata, names=NULL, normalize="none", smooth=0){

    if ( !( is.data.frame(indata) ) )
        stop("'indata' must a dataframe\n")
        
    if (!is.null( names )) {
        if (is.numeric( names )){
            if (min(names) < 1 || max(names) > ncol(indata)){
                stop("columns out of range \n")
            }
        } else {
            if (class(names) %in% c("formula", "character")){
                names <- rhsf2list(names)[[1]]
            } else {
                stop("don't know what to do\n")
            }
        }
    } 
    
    if (is.null(names))
        out <- xtabs(~., data=indata)
    else
        out <- xtabs(~., data=indata[, names, drop=FALSE])

    ## FIXME : There is no check on what smooth is
    if (smooth > 0)
        out <- out + smooth

    if (normalize != "none")
        tabNormalize( out, normalize )
    else
        out
}

#' @title Normalize an array
#' 
#' @description Normalize an array in various ways.
#'
#' @name array-normalize
#' 
#' @param tab A multidimensional array
#' @param type Either "none", "first", or "all"
#' @return An array
#' @examples
#' ar_normalize( HairEyeColor, type="first")
#' ar_normalize( HairEyeColor, type="all")
#' 
tabNormalize <- function(tab, type="none"){
    switch(type,
           "first"={
               if (length(dim(tab))>1){
                   tab <- tabPerm(tabDiv(tab, tabMarg(tab, 2:length(dim(tab)))),
                                  names(dimnames(tab)))
               } else {
                   tab <- tab / sum(tab)
               }
           },
           "all"  = { tab <- tab / sum(tab) },
           "none" = {}
           )
    #attr(tab, "call") <- NULL
    tab
}

#' @rdname array-normalize
ar_normalize <- tabNormalize

#' @rdname array-normalize
ar_norm <- tabNormalize



