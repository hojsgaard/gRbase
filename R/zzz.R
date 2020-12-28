
.onAttach<-function(libname, pkgname) {

    ## package startup check
    toinstall=c(
        "graph",
        "Rgraphviz",
        "RBGL"
    )
    
    already_installed <- sapply(toinstall, function(pkg)
        requireNamespace(pkg, quietly=TRUE))

    if (any(!already_installed)){
        packageStartupMessage("Need to install the following package(s): ",
                              toString(toinstall[!already_installed]), "\n")
    }
    
    ## install if needed
    if(!base::all(already_installed)){
        if (!requireNamespace("BiocManager", quietly=TRUE))
            install.packages("BiocManager")
        BiocManager::install(toinstall[!already_installed], dependencies=TRUE)
    }
}



    ## previously installed
    ## already_installed=toinstall %in% utils::installed.packages()[,1]
    ## cat(toString(already_installed), "\n")
    ## cat(toString(toinstall[!already_installed]), "\n")


## .onLoad <- function(libname, pkgname){

   ##  ow <- options(warn=-1)
   ##  pkg <- c("Rgraphviz", "graph", "RBGL")
   ##  ##out <- sapply(pkg, requireNamespace, quietly=TRUE)
   ##  out <- sapply(pkg, function(p) requireNamespace(p, quietly=TRUE))
   ##  if (any(!out)){
   ##      packageStartupMessage(paste0("Required packages from Bioconductor are not installed: ",
   ##                 toString(pkg[!out])), "\n")
   ##      packageStartupMessage("Please execute these lines and re-install gRbase again:\n")
   ##      packageStartupMessage('
   ##      source("https://bioconductor.org/biocLite.R");
   ##      biocLite(c("graph", "RBGL", "Rgraphviz"))
   ##      \n')        
   ##  } else {
   ##      packageStartupMessage("You are good to go\n")
   ##  }
   ## options(ow)
## }



## .onAttach <- function(libname, pkgname){
##     ow <- options(warn=-1)
##     pkg <- c("Rgraphviz", "graph", "RBGL")
##     ##out <- sapply(pkg, requireNamespace, quietly=TRUE)
##     out <- sapply(pkg, function(p) requireNamespace(p, quietly=TRUE))
##     if (any(!out)){
##         msg <- paste0("Required packages from Bioconductor are not installed: ",
##                       toString(pkg[!out]), "\n",
##                       "Please execute these lines and re-install gRbase again:\n",
##                       'source("https://bioconductor.org/biocLite.R");',
##                       'biocLite(c("graph", "RBGL", "Rgraphviz")) \n')
##         packageStartupMessage(msg)        
##     }
##     ##else {
##     ##    packageStartupMessage("You are good to go\n")
##     ##}
##    options(ow)

## }
