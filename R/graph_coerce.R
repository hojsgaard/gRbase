## #####################################################################
##
#' @title Graph coercion
#' @description Methods for changing graph representations
#' @name graph-coerce
##
## #####################################################################
#' 
#' @param object A graph object
#' @param class The desired output class
#'
#' @details coerceGraph is used in the book "Graphical models with R".
#' A more generic approach is as().
#'
#' @examples
#' 
#' g1 <- ug(~a:b+b:c)
#' as(g1, "igraph")
#' as(g1, "matrix")
#' as(g1, "Matrix")
#' as(g1, "dgCMatrix")
#'
#' ## graph_as(g1, "ugList") ## Fails
#' ## getCliques(g1)         ## Works
#' 
#' l1 <- list(c("a" ,"b"), c("b", "c"))
#' graph_as(l1, "graphNEL", "ugList")
#' 
#' @export
#' @rdname graph-coerce
coerceGraph <- function(object, class){
    ##.as_fun(object, class)
    as(object, class)
}



## ####################################################################
##
## as( ): Coercion between graphNEL, igraph, matrix, dgCMatrix.
##
## ####################################################################

setOldClass("igraph")
setAs("igraph",   "matrix",    function(from) g_ig2dm_(from))
setAs("igraph",   "dgCMatrix", function(from) g_ig2sm_(from))

setAs("matrix",    "igraph",   function(from) g_dm2ig_(from))
setAs("dgCMatrix", "igraph",   function(from) g_sm2ig_(from))



#' @export
#' @rdname graph-coerce
#'
#' @param outtype The desired output outtype
#' @param intype The desired output outtype (only relevant if object is a list)
#' 
graph_as  <- function(object, outtype, intype=NULL){
    
    if (!inherits(object, c(.graph_type(), "list", "formula")))
        stop("Wrong input format\n")
       
    if (inherits(object, c("list", "formula")))
        return(.handle_list_case(object, outtype, intype))

    if (inherits(object, c("matrix", "Matrix")))
        return(.handle_matrix_case(object, outtype))

    if (inherits(object, c("igraph", "graphNEL")))
        return(.handle_graph_case(object, outtype))
}


.handle_matrix_case <- function(object, outtype){
    ## 'object' is matrix or Matrix  
    if (outtype %in% .graph_type())
        return(as(object, outtype))
        ##return(.as_fun(object, outtype))

    id <- match(outtype, .list_format())
    idt <- .get_idtype(id)
    
    switch(id,
           "ugl" ={g_M2ugl_(object)},
           "dagl"={g_M2dagl_(object)},
           "adl" ={g_M2adl_(object)})                            
}

.handle_graph_case <- function(object, outtype){
    if (outtype %in% .graph_type())
        return(as(object, outtype))
        ##return(.as_fun(object, outtype))
    stop(".handle_graph_case not implemented for this outtype\n")
}

.handle_list_case <- function(object, outtype, intype){
    if (inherits(object, "formula"))
        object <- rhsf2list(object)
    
    id <- match(intype, .list_format())
    idt <- .get_idtype(id)
    
    switch(id,
           "ugl"  ={g_ugl2XX_(object, outtype)},
           "dagl" ={g_dagl2XX_(object, outtype)},
           "adl"  ={g_adl2XX_(object, outtype)})                            
}

.get_idtype  <- function(id){
    if (id %in% 1:4) idt <- "ugl"
    else if (id %in% 5:8) idt <- "dagl"
    else if (id %in% 9:12) idt <- "adl"
    else stop("invalid 'type'\n")
    idt
}

.graph_type <- function(){
    c("matrix", "dgCMatrix", "graphNEL", "igraph")
}

.list_format <- function(){
    c("ugList",   "uglist",   "ug_list",  "ugl",
      "dagList",  "daglist",  "dag_list", "dagl",
      "adjList",  "adjlist",  "adj_list", "adl")
}












