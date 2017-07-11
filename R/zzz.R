.onLoad <- function(libname, pkgname){

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
}

.onAttach <- function(libname, pkgname){
    ow <- options(warn=-1)
    pkg <- c("Rgraphviz", "graph", "RBGL")
    ##out <- sapply(pkg, requireNamespace, quietly=TRUE)
    out <- sapply(pkg, function(p) requireNamespace(p, quietly=TRUE))
    if (any(!out)){
        msg <- paste0("Required packages from Bioconductor are not installed: ",
                      toString(pkg[!out]), "\n",
                      "Please execute these lines and re-install gRbase again:\n",
                      'source("https://bioconductor.org/biocLite.R");',
                      'biocLite(c("graph", "RBGL", "Rgraphviz")) \n')
        packageStartupMessage(msg)        
    } else {
        packageStartupMessage("You are good to go\n")
    }
   options(ow)

}
