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
