
## set up hooks, S4 classes and initialize global variables.
.onLoad = function(lib, pkg) {

    cat("onLoad\n")

    setGeneric("nodes", function(object, ...) standardGeneric("nodes"))

    setMethod("nodes",
              signature(object = "igraph"),
              function (object, ...) 
              {
                  nodes_(object)
              }
              )
    
}





## safe version of setMethod().
## tryMethod = function(f, signature, definition, generic) {
    
##     ## try a first time.
##     catch = try(setMethod(f, signature, definition), silent = TRUE)
    
##     if (is(catch, "try-error")) {        
##         ## if it failed, create a generic function ...
##         setGeneric(f, generic)
##         ## ... and then try again.
##         setMethod(f, signature, definition)
        
##     }#THEN
    
## }#TRYMETHOD




    
  ##   cl <- "igraph"

  ## setHook(packageEvent("gRbase", "attach"), action = "append",
  ##   function(...) {

  ##     for (cl in "igraph") {

  ##       setMethod("nodes", cl, where = .GlobalEnv,
  ##         function(object) nodes_(object))

  ##     }#FOR

  ## })
    
  ##   setMethod("nodes", cl, where = .GlobalEnv,
  ##             function(object) nodes_(object))

  ##   tryMethod("nodes", cl,
  ##             definition = function(object) nodes_(object),
  ##             generic = function(object, ...) standardGeneric("nodes"))
    
    ## tryMethod("nodes<-", cl,
    ##           definition = function(object, value) .relabel(object, value),
    ##           generic = function(object, value) standardGeneric("nodes<-"))
