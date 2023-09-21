# in a package namespace:
.onLoad <- function(libname=NULL, pkgname) {

    ## if(("graph" %in% (.packages())) || ("RBGL" %in% (.packages()))) {
    ##     cat(sprintf("NOTICE: The graph and/or RBGL package is loaded. This may cause confusion because graph/RBGL and gRbase packages\n"))
    ##     cat(sprintf("contains (a few) functions with the same name, but the graph function applies to graphNEL objects\n"))
    ##     cat(sprintf("while the gRbase function applies to igraph objects or adjacency matrices\n"))
    ##     cat(sprintf("The functions in question include: addEdge, adj, connComp, edges, nodes, removeEdge, subGraph\n"))
    ##     cat(sprintf("Please ensure to call the right function by graph::edges and gRbase::edges etc\n"))
    ## }

}


## .onAttach<-function(libname, pkgname) {

##     ## ## package startup check
##     ## toinstall=c(
##     ##     "graph",
##     ##     "Rgraphviz",
##     ##     "RBGL"
##     ## )
    
##     ## already_installed <- sapply(toinstall, function(pkg)
##     ##     requireNamespace(pkg, quietly=TRUE))

##     ## if (any(!already_installed)){
##     ##     packageStartupMessage("Need to install the following package(s): ",
##     ##                           toString(toinstall[!already_installed]), "\n")
##     ## }
    
##     ## ## install if needed
##     ## if(!base::all(already_installed)){
##     ##     if (!requireNamespace("BiocManager", quietly=TRUE))
##     ##         install.packages("BiocManager")
##     ##     BiocManager::install(toinstall[!already_installed], dependencies=TRUE)
##     ## }

## }



