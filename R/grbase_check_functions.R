
.check.is.matrix <- function(x){
    if (!inherits(x, c("matrix", "dgCMatrix")))
        stop("Input must be a matrix or a dgCMatrix\n")
}


.check.is.igraph <- function(x){
    if (!inherits(x, "igraph"))
        stop("'x' not an igraph object...")    
}

.is_list_of_atomic <- function(z){
    is.list(z) && all(sapply(z, is.atomic))            
}
