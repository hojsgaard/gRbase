#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
SEXP which_matrix_index( SEXP XX_ ){
  NumericMatrix X(XX_);
  double sum=0;
  for(int ii=0; ii<X.nrow(); ii++){
    for (int jj=0; jj<X.ncol(); jj++){
      sum += (X(ii,jj)!=0);
    }
  }
  
  int kk=0;
  NumericMatrix out(sum, 2);
  for(int ii=0; ii<X.nrow(); ii++){
    for (int jj=0; jj<X.ncol(); jj++){
      if (X(ii,jj)!=0){
		out(kk,0)=ii+1; 
		out(kk++,1)=jj+1;
      }
    }
  }

  return out;
}
