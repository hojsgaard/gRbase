.check.is.graphNEL.or.igraph <- function(x){
    if (!inherits(x, c("graphNEL", "igraph")))
        stop("Input must be a graphNEL or a igraph\n")
}

.check.is.matrix <- function(x){
    if (!inherits(x, c("matrix", "dgCMatrix")))
        stop("Input must be a matrix or a dgCMatrix\n")
}

.check.is.graphNEL <- function(x){
    if (!inherits(x, "graphNEL"))
        stop("'x' not a graphNEL object...")    
}

.check.is.igraph <- function(x){
    if (!inherits(x, "graphNEL"))
        stop("'x' not an igraph object...")    
}


.is_list_of_atomic <- function(z){
    is.list(z) && all(sapply(z, is.atomic))            
}
