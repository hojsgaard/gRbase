
#include <Rcpp.h>
using namespace Rcpp;
//[[Rcpp::interfaces(r,cpp)]]

typedef Rcpp::NumericVector   numVec;
typedef Rcpp::IntegerVector   intVec;
typedef Rcpp::CharacterVector chrVec;

IntegerVector make_indic(int ndim, const IntegerVector& slice){
  IntegerVector indic(ndim);
  for (int i=0; i < slice.length(); i++){
    indic[slice[i] - 1] = 1;
  }
  return indic;
}

//' @rdname array-cell
//[[Rcpp::export]]
IntegerVector make_plevels_(const IntegerVector& dim){
  IntegerVector plevels(dim.length());
  plevels[0] = 1;
  for (int i=1; i < dim.length(); i++){
    plevels[i] =  dim[i - 1] * plevels[i - 1];
  }
  return plevels;
}


// //' @rdname array-cell
// //[[Rcpp::export]]
int cell2entry_prim_(const NumericVector& cell, const IntegerVector& plevels){
  double out=0;
  for (int i=0; i < cell.length(); ++i){
    out += (cell[i] - 1) * plevels[i];
  }
  return ((int) out) + 1;
}

//' @rdname array-cell
//[[Rcpp::export]]
int cell2entry_(const NumericVector& cell, const IntegerVector& dim){
  int i, ss=1;
  double out=cell[0] - 1;
  for (i=1; i < dim.length(); i++){
    ss  *=  dim[i - 1];
    out += (cell[i] - 1) * ss;
  }
  return ((int) out) + 1;
}

// ------------------------------------------------

// //' @rdname array-cell
// //[[Rcpp::export]]
IntegerVector entry2cell_prim_(const int& entry, const IntegerVector& plevels){
  IntegerVector cell(plevels.length());
  int rrr = entry - 1;
  for (int i=plevels.length() - 1; i>=0; i--){
    cell[i] = rrr / plevels[i];
    rrr = rrr % plevels[i];
    //Rcout << i << std::endl;
  }
  return cell + 1;
}

//' @rdname array-cell
//[[Rcpp::export]]
IntegerVector entry2cell_(const int& entry, const IntegerVector& dim){
  IntegerVector plevels = make_plevels_(dim);
  return entry2cell_prim_(entry, plevels);
}

// ----------------------------------------------------


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

NumericVector next_cell_slice_prim_(const NumericVector& cell, const IntegerVector& dim, const IntegerVector& slice_idx){
  numVec out_cell = clone(cell);
  int ndim = cell.length();
  int sum=0, n_init=0;
  for (int j=0; j < ndim; j++){
    sum += slice_idx[j];
    if (slice_idx[j] == 0){
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
NumericVector next_cell_slice_(const NumericVector& cell, const IntegerVector& dim, const IntegerVector& slice_marg){
  IntegerVector slice_idx = make_indic(dim.length(), slice_marg);
  return next_cell_slice_prim_(cell, dim, slice_idx);
}

// ---------------------------------

IntegerVector slice2entry_prim_(const IntegerVector& slice_cell, const IntegerVector& slice_marg,
				const IntegerVector& dim,
				const IntegerVector& slice_idx, const IntegerVector& plevels
				){

  //Rcout << "dimXXX: " << dim << std::endl;
  int i, ndim=dim.length(), entry, out_len=1;
  NumericVector cell(ndim);

  // Create initial cell
  for (i=0; i < ndim; i++)
    cell[i] = 1;
  for (i=0; i < slice_cell.length(); i++)
    cell[slice_marg[i] - 1] = slice_cell[i];


  IntegerVector tmp = clone(dim);
  for (i=0; i < slice_marg.length(); i++)
    tmp[slice_marg[i] - 1] = 1;

  for (i=0; i < ndim; i++){
    out_len *= tmp[ i ];
  }
  //Rcout << "dimXXX: " << dim << std::endl;
  
  IntegerVector out(out_len);

  for (i=0; i < out_len; i++){
    entry = cell2entry_prim_(cell, plevels);
    out[ i ] = entry;
    cell  = next_cell_slice_prim_(cell, dim, slice_idx);
  }
  return out;
}

//' @rdname array-cell
//[[Rcpp::export]]
IntegerVector slice2entry_(const IntegerVector& slice_cell, const IntegerVector& slice_marg, const IntegerVector& dim){
  IntegerVector slice_idx = make_indic(dim.length(), slice_marg);
  IntegerVector plevels    = make_plevels_(dim);

  //Rcout << "dim: " << dim << std::endl;
  IntegerVector out = slice2entry_prim_(slice_cell, slice_marg, dim, slice_idx, plevels);
  //Rcout << "dim: " << dim << std::endl;
  return out;
}


// ---------------------------------


//' @rdname array-cell
//[[Rcpp::export]]
int cell2entry_perm_(const NumericVector& cell, const IntegerVector& dim, const IntegerVector& perm){

  IntegerVector new_dim(dim.length());
  NumericVector new_cell(cell.length());

  new_dim = dim[perm - 1];
  new_cell = cell[perm - 1];
  return cell2entry_(new_cell, new_dim); 
}

// ---------------------------------

int cell2entry_perm_prim_(const NumericVector& cell, const IntegerVector& perm, const IntegerVector& plevels){
  int cell_number = 0, ndim=cell.length();
  for (int i=0; i < ndim; i++){
    cell_number +=  (plevels[perm[i] - 1] * (cell[i] - 1));
  }
  return cell_number + 1;
}

//' @rdname array-cell
//[[Rcpp::export]]
IntegerVector perm_cell_entries_(const IntegerVector& perm, const IntegerVector& dim){

  int ndim=dim.length(), i, out=0, len_entry=1;
  NumericVector cell(ndim);
  IntegerVector dim_new(ndim);
  
  for (i=0; i < ndim; i++) {
    cell[i] = 1;
    len_entry *= dim[ i ];
    dim_new[i] = dim[perm[i] - 1];
  }

  IntegerVector plevels = make_plevels_(dim);
  IntegerVector entry_new( len_entry );
  for (i=0; i < len_entry; i++){
    //Rf_PrintValue( cell );
    out = cell2entry_perm_prim_(cell, perm, plevels);
    entry_new[i] = out;
    cell = next_cell_(cell, dim_new);
  }
  return entry_new;
}




// //' @rdname array-cell
// //[[Rcpp::export]]
// int get_cell_number_(const NumericVector& cell, const IntegerVector& dim, const IntegerVector& perm){
//   IntegerVector plevels=make_plevels_(dim);

//   NumericVector new_cell(cell.length());
//   for (int i=0; i < cell.length(); ++i)
//     new_cell[i] = cell[perm[i] - 1];

//   IntegerVector dim_new(dim.length());
//   dim_new = dim[perm - 1];
  
//   Rcout << "cell : " << cell;
//   Rcout << " new_cell : " << new_cell << std::endl;
  
//   return get_cell_number_prim_(new_cell, perm, plevels);
// }






// //' @rdname array-cell
// //[[Rcpp::export]]
// int gcn_(const NumericVector& cell, const IntegerVector& dim, const IntegerVector& perm){
//   IntegerVector plevels=make_plevels_(dim);
//   //Rf_PrintValue( plevels );
//   return get_cell_number_prim_(cell, perm, plevels);
// }




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


#next_cell_slice(c(2,1,1,2),  sliceset=c(2), dim2323)
#next_cell_slice_(c(2,1,1,2),  slice_marg=c(2), dim2323)
#next_cell_slice(c(1,3,2,1),  sliceset=c(2,3), dim2323)
#next_cell_slice_(c(1,3,2,1),  slice_marg=c(2,3), dim2323)

#x<-c(1,3,2,1)
#next_cell_slice_(x,  slice_marg=c(2,3), dim2323)
#x ## notice: x has changed

#library(microbenchmark)
#microbenchmark(
#next_cell_slice(c(2,1,1,2),  sliceset=c(2), dim2323),
#next_cell_slice_(c(2,1,1,2),  slice_marg=c(2), dim2323),
#next_cell_slice(c(1,3,2,1),  sliceset=c(2,3), dim2323),
#next_cell_slice_(c(1,3,2,1),  slice_marg=c(2,3), dim2323)
#)


## (r1<-slice2entry(slicecell=c(1,2), sliceset=c(2,3), dim2222))
## (r2<-slice2entry_(slice_cell=c(1,2), slice_marg=c(2,3), dim2222))


x  <- HairEyeColor
ii <- seq_along(x)
dim(ii) <- dim(x)
pp <- c(2,3,1)
as.integer(aperm(ii, pp))
#permuteCellEntries_(pp, dim(x))

#permuteCellEntries_(c(2,1), c(2,3))

as.integer(aperm(ii, pp))
#permuteCellEntries_(pp, c(4,4,2))

#library(microbenchmark)
#microbenchmark(as.integer(aperm(ii, pp)),permuteCellEntries_(pp, c(4,4,2)))

*/




// //' @rdname array-cell
// //[[Rcpp::export]]
// int cell2entry_(const NumericVector& cell, const IntegerVector& dim){
//   int i, ss=1, res=cell[0] - 1;

//   for (i=1; i < dim.length(); i++){
//     ss  *=  dim[i - 1];
//     res += (cell[i] - 1) * ss;
//   }
//   return res + 1;
// }
