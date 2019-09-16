## #########################################################################
#' @title Table cell operations.
#'
#' @description Low level table cell operations.
#'
#' @name array-cell
#'
#' @param cell Vector giving the cell, e.g. c(1, 1, 2) in 3-way table.
#' @param dim Vector giving array dimension, eg c(2, 2, 2).
#' @param perm Vector giving permutaion of array, eg. c(1, 3, 2).
#' @param slice_marg Vector giving the margin of a table, eg. c(2, 3)
#' @param slice_cell Vector giving the corresponding cell of marginal table, e.g. c(1, 2)
#' @param entry An entry in an array (a number indexing a vector).
#' 
#' @examples
#'
#' di <- c(2, 2, 3)
#'
#' cell2entry(c(1, 1, 1), dim=di)
#' cell2entry(c(2, 2, 3), dim=di)
#'
#' entry2cell(1, dim=di)
#' entry2cell(12, dim=di)
#'
#' next_cell(c(1, 1, 1), dim=di)
#' next_cell(c(2, 1, 1), dim=di)
#'
#' ## The first two entries are kept fixed
#' next_cell_slice(c(2, 1, 1), dim=di, slice_marg=c(1, 2))
#' next_cell_slice(c(2, 1, 2), dim=di, slice_marg=c(1, 2))
#'
#' ## Cell (2, 2, 1) corresponds to entry 4
#' cell2entry(c(2, 2, 1), dim=di)
#' ## Same as
#' cell2entry_perm(c(2, 2, 1), dim=di, perm=c(1, 2, 3))
#' ## If the table dimensions are permuted as (3, 1, 2)
#' ## the entry becomes
#' cell2entry_perm(c(2, 2, 1), dim=di, perm=c(3, 1, 2))

## --------------------------
## Aliases for cpp functions
## --------------------------
#' @rdname array-cell
cell2entry    <- cell2entry_
#' @rdname array-cell
entry2cell    <- entry2cell_
#' @rdname array-cell
next_cell      <- next_cell_
#' @rdname array-cell
next_cell_slice <- next_cell_slice_
#' @rdname array-cell
slice2entry   <- slice2entry_
##' @rdname array-cell
cell2entry_perm <- cell2entry_perm_
#' @rdname array-cell
perm_cell_entries <- perm_cell_entries_
## --- END ---

## -------------------------
## Additional functionality
## -------------------------

.entry2cell <- function(entry, dim, plev=cumprod(dim) / dim){
  cell <- rep(NA, length(dim))
  rrr <- entry - 1
  for (ii in length(dim):1){
    cell[ii] <- rrr %/% plev[ii]
    rrr <- rrr %% plev[ii]
  }
  cell + 1
}

## -----------------------------------------------------------
## factgrid
## -----------------------------------------------------------

#' @rdname array-cell
fact_grid <- function(dim, slice_cell=NULL, slice_marg=NULL){
  if (is.null(slice_cell)){
    .factgrid1Prim(dim)
  } else {
    .factgrid2Prim(dim, slice_cell, slice_marg)
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
      cell <- next_cell_(cell, dim)
                                        #print(cell)
      mm[ii,] <- cell
    }
  mm
}

.factgrid2Prim <- function(dim , slice_cell, slice_marg){

  nr <- prod(dim[-slice_marg])
  nc <- length(dim)
  mm <- matrix(NA, nrow=nr, ncol=nc)

  cell    <- rep(1, nc)
  cell[slice_marg] <- slice_cell
                                        #print(cell)
  mm[1, ]  <- cell
  if (nr > 1)
    for (ii in 2:nr){
      cell <- next_cell_slice_(cell, slice_marg, dim)
                                        #print(cell)
      mm[ii,] <- cell
    }
  mm
}
