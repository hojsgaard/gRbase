## #########################################################################
##
## Interface to cpp functions for indexing multidimensional arrays
##
## Autor: Søren Højsgaard
##
## Known issues:
##
## FIXME: Naming of arguments; e.g. that sliceset should
## perhaps be marginal etc.
##
## FIXME: Should be documented with roxygen
##
## #########################################################################


## --------------------------
## Aliases for cpp functions
## --------------------------
cell2entry <- cell2entry_
nextCell <- next_cell_
nextCellSlice <- next_cell_slice_
slice2entry <- slice2entry_
## --- END ---

## -------------------------
## Additional functionality
## -------------------------

entry2cell <- function(entry, dim, plev=cumprod(dim)/dim){
  cell <- rep(NA, length(dim))
  rrr <- entry-1
  for (ii in length(dim):1){
    cell[ii] <- rrr %/% plev[ii]
    rrr <- rrr %% plev[ii]
  }
  cell + 1
}


## -----------------------------------------------------------
## factgrid
## -----------------------------------------------------------

factGrid <- function(dim, slicecell=NULL, sliceset=NULL){
  if (is.null(slicecell)){
    .factgrid1Prim(dim)
  } else {
    .factgrid2Prim(dim, slicecell, sliceset)
  }
}

.factgrid1Prim <- function( dim ){

  nr <- prod(dim)
  nc <- length(dim)
  mm <- matrix(NA, nrow=nr, ncol=nc)

  cell    <- rep(1, nc)
                                        #print(cell)
  mm[1, ]  <- cell
  if (nr > 1)
    for (ii in 2:nr){
      cell <- nextCell(cell, dim)
                                        #print(cell)
      mm[ii,] <- cell
    }
  mm
}

.factgrid2Prim <- function(dim , slicecell, sliceset){

  nr <- prod(dim[-sliceset])
  nc <- length(dim)
  mm <- matrix(NA, nrow=nr, ncol=nc)

  cell    <- rep(1, nc)
  cell[sliceset] <- slicecell
                                        #print(cell)
  mm[1, ]  <- cell
  if (nr > 1)
    for (ii in 2:nr){
      cell <- nextCellSlice(cell, sliceset, dim)
                                        #print(cell)
      mm[ii,] <- cell
    }
  mm
}
