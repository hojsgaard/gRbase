
.onAttach<-function(libname, pkgname) {

    ## ## package startup check
    ## toinstall=c(
    ##     "graph",
    ##     "Rgraphviz",
    ##     "RBGL"
    ## )
    
    ## already_installed <- sapply(toinstall, function(pkg)
    ##     requireNamespace(pkg, quietly=TRUE))

    ## if (any(!already_installed)){
    ##     packageStartupMessage("Need to install the following package(s): ",
    ##                           toString(toinstall[!already_installed]), "\n")
    ## }
    
    ## ## install if needed
    ## if(!base::all(already_installed)){
    ##     if (!requireNamespace("BiocManager", quietly=TRUE))
    ##         install.packages("BiocManager")
    ##     BiocManager::install(toinstall[!already_installed], dependencies=TRUE)
    ## }

}



