#include <Rcpp.h>
using namespace Rcpp;
//[[Rcpp::interfaces(r,cpp)]]

typedef Rcpp::NumericVector   numVec;
typedef Rcpp::IntegerVector   intVec;
typedef Rcpp::CharacterVector chrVec;

//' @title table cell operations.
//' @description low level table cell operations
//' @name array-cell
//' @param cell Vector giving the cell, eg c(1,1,2) in 3-way table.
//' @param dim  Vector giving array dimension, eg c(2,2,2).
//' @param perm Vector giving permutaion of array, eg. c(1,3,2).
//' @param slice_set Vector giving the margin of a table, eg. c(2,3)
//' @param slice_cell Vector giving the corresponding cell of marginal table, eg. c(1,2)

IntegerVector make_indic(int ndim, const IntegerVector& slice){
  IntegerVector indic( ndim );
  for (int i=0; i < slice.length(); i++){
    indic[slice[i] - 1] = 1;
  }
  return indic;
}

IntegerVector make_prod( int ndim, const IntegerVector& dim ){
  IntegerVector plevels( ndim );
  plevels[0] = 1;
  for (int i=1; i < ndim; i++){
    plevels[i] =  dim[i - 1] * plevels[i - 1];
  }
  return plevels;
}

//' @rdname array-cell
//[[Rcpp::export]]
int cell2entry_(const NumericVector& cell, const IntegerVector& dim){
  int ndim=dim.length();
  int i, ss=1, res=cell[0] - 1;

  for (i=1; i < ndim; i++){
    ss  *=  dim[i - 1];
    res += (cell[i] - 1) * ss;
  }
  return res + 1;
}


int cell2entry_prim_(const NumericVector& cell, const IntegerVector& plevels){
  int ndim=cell.length(), out=0;
  for (int i=0; i < ndim; i++){
    out += (cell[i] - 1) * plevels[i];
  }
  return out + 1;
}

// ------------------------------------------------

//' @rdname array-cell
//[[Rcpp::export]]
NumericVector next_cell_(const NumericVector& cell, const IntegerVector& dim){
  numVec out_cell = clone(cell);
  int ndim=dim.length(), j, n_init=0;
  for (j=0; j < ndim; j++){
    if (out_cell[j]  <  dim[j]){
      out_cell[j] = out_cell[j] + 1;
      break;
    } else {
      out_cell[j] = 1;
      n_init++;
    }
  }
  return out_cell;
}

// ------------------------------------------------

NumericVector next_cell_slice_prim_(const NumericVector& cell, const IntegerVector& dim, const IntegerVector& sliceIndic){
  numVec out_cell = clone(cell);
  int ndim = cell.length();
  int sum=0, n_init=0;
  for (int j=0; j < ndim; j++){
    sum += sliceIndic[j];
    if (sliceIndic[j] == 0){
      if (out_cell[j]  <  dim[j]){
	out_cell[j] = out_cell[j] + 1;
	break;
      } else {
	out_cell[j] = 1;
	n_init++;
      }
    }
  }
  if (n_init == (ndim - sum)){
    out_cell[0] = -1;
  }
  return out_cell;
}

//' @rdname array-cell
//[[Rcpp::export]]
NumericVector next_cell_slice_(const NumericVector& cell, const IntegerVector& dim, const IntegerVector& slice_set){
  IntegerVector sliceIndic = make_indic( dim.length(), slice_set);
  return next_cell_slice_prim_(cell, dim, sliceIndic);
}

// ---------------------------------

IntegerVector slice2entry_prim_(const IntegerVector& slice_cell, const IntegerVector& slice_set,
				const IntegerVector& dim,
				const IntegerVector& sliceIndic, const IntegerVector& plevels
				){

  //Rcout << "dimXXX: " << dim << std::endl;
  int i, ndim=dim.length(), entry, out_len=1;
  NumericVector cell( ndim );

  // Create initial cell
  for (i=0; i < ndim; i++)
    cell[i] = 1;
  for (i=0; i < slice_cell.length(); i++)
    cell[slice_set[i] - 1] = slice_cell[i];


  IntegerVector tmp = clone(dim);
  for (i=0; i < slice_set.length(); i++)
    tmp[slice_set[i] - 1] = 1;

  for (i=0; i < ndim; i++){
    out_len *= tmp[ i ];
  }
  //Rcout << "dimXXX: " << dim << std::endl;
  
  IntegerVector out( out_len );

  for (i=0; i < out_len; i++){
    entry = cell2entry_prim_( cell, plevels );
    out[ i ] = entry;
    cell  = next_cell_slice_prim_(cell, dim, sliceIndic);
  }
  return out;
}

//' @rdname array-cell
//[[Rcpp::export]]
IntegerVector slice2entry_(const IntegerVector& slice_cell, const IntegerVector& slice_set, const IntegerVector& dim){
  IntegerVector sliceIndic = make_indic(dim.length(), slice_set);
  IntegerVector plevels    = make_prod(dim.length(), dim);

  //Rcout << "dim: " << dim << std::endl;
  IntegerVector out = slice2entry_prim_(slice_cell, slice_set, dim, sliceIndic, plevels);
  //Rcout << "dim: " << dim << std::endl;
  return out;
}

// ---------------------------------

int get_cell_number_prim_(const NumericVector& cell, const IntegerVector& perm, const IntegerVector& pvec){
  int cell_number = 0, ndim=cell.length();
  for (int i=0; i < ndim; i++){
    cell_number +=  (pvec[perm[i] - 1] * (cell[i] - 1));
  }
  return cell_number + 1;
}


//' @rdname array-cell
//[[Rcpp::export]]
int get_cell_number_(const NumericVector& cell, const IntegerVector& dim, const IntegerVector& perm){
  IntegerVector pvec=make_prod( dim.length(), dim );
  return get_cell_number_prim_( cell, perm, pvec );
}

// ---------------------------------


//' @rdname array-cell
//[[Rcpp::export]]
IntegerVector perm_cell_entries_(const IntegerVector& perm, const IntegerVector& dim){

  int ndim=dim.length(), i;
  NumericVector cell( ndim );
  for (i=0; i < ndim; i++) cell[i] = 1;
  int out=0, len_entry=1;

  IntegerVector dim_new( ndim );
  for (i=0; i < ndim;i++) {
    len_entry *= dim[ i ];
    dim_new[i] = dim[perm[i] - 1];
  }

  IntegerVector pvec = make_prod( ndim, dim);
  IntegerVector entry_new( len_entry );
  for (i=0; i < len_entry; i++){
    //Rf_PrintValue( cell );
    out = get_cell_number_prim_(cell, perm, pvec);
    entry_new[i] = out;
    next_cell_(cell, dim_new);
  }
  return entry_new;
}







/*** R

library(gRbase)
## 1-dimensional array
x1 <- 1:8; dim(x1) <- 8; x1
c(is.array(x1), is.matrix(x1))

## 2-dimensional array (matrix)
x2 <- 1:8; dim(x2) <- c(2,4); x2
c(is.array(x2), is.matrix(x2))

## 3-dimensional array
x3 <- array(1:8, dim=c(2,2,2)); x3
c(is.array(x3), is.matrix(x3))

dim2222 <- c(2,2,2,2)
dim2323 <- c(2,3,2,3)

entry2cell(1, dim2222)
entry2cell(6, dim2222)

cell2entry(c(1,1,1,1), dim2222)
cell2entry_(c(1,1,1,1), dim2222)
cell2entry(c(2,1,2,1), dim2222)
cell2entry_(c(2,1,2,1), dim2222)

#library(microbenchmark)
#microbenchmark(
#cell2entry(c(1,1,1,1), dim2222),
#cell2entry_(c(1,1,1,1), dim2222),
#cell2entry(c(2,1,2,1), dim2222),
#cell2entry_(c(2,1,2,1), dim2222)
#)

nextCell(c(1,1,2,1), dim2222)
nextCell(c(2,2,2,1), dim2222)
next_cell_(c(1,1,2,1), dim2222)
next_cell_(c(2,2,2,1), dim2222)

x<-c(1,1,2,1)
next_cell_(x, dim2222)
x ## notice: x has changed!!!


next_cell_slice(c(2,1,1,2),  sliceset=c(2), dim2323)
next_cell_slice_(c(2,1,1,2),  slice_set=c(2), dim2323)
next_cell_slice(c(1,3,2,1),  sliceset=c(2,3), dim2323)
next_cell_slice_(c(1,3,2,1),  slice_set=c(2,3), dim2323)

x<-c(1,3,2,1)
next_cell_slice_(x,  slice_set=c(2,3), dim2323)
x ## notice: x has changed

#library(microbenchmark)
#microbenchmark(
#next_cell_slice(c(2,1,1,2),  sliceset=c(2), dim2323),
#next_cell_slice_(c(2,1,1,2),  slice_set=c(2), dim2323),
#next_cell_slice(c(1,3,2,1),  sliceset=c(2,3), dim2323),
#next_cell_slice_(c(1,3,2,1),  slice_set=c(2,3), dim2323)
#)


(r1<-slice2entry(slicecell=c(1,2), sliceset=c(2,3), dim2222))
(r2<-slice2entry_(slice_cell=c(1,2), slice_set=c(2,3), dim2222))


x  <- HairEyeColor
ii <- seq_along(x)
dim(ii) <- dim(x)
pp <- c(2,3,1)
as.integer(aperm(ii, pp))
permuteCellEntries_(pp, dim(x))

permuteCellEntries_(c(2,1), c(2,3))

as.integer(aperm(ii, pp))
permuteCellEntries_(pp, c(4,4,2))

library(microbenchmark)
microbenchmark(as.integer(aperm(ii, pp)),permuteCellEntries_(pp, c(4,4,2)))

*/

