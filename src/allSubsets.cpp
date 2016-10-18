/* **************************************************************
  Implements:
  allSubsets_cpp(x)  - 'any' vector x
  allSubsets0_cpp(x) - integer vector x

  *************************************************************** */

#include <Rcpp.h>
using namespace Rcpp;
//[[Rcpp::interfaces(r,cpp)]]

// Works for integer input vector
//[[Rcpp::export]]
List allSubsets0__(const IntegerVector& x ){
  int nx = x.length(), nout=pow(2., nx), i, k, ny=1;
  double z;
  List out( nout );
  out[0]=-1;

  for (i=0; i<nx; ++i){
    z = x[i];
    for (k=0; k<ny; ++k){
      IntegerVector tmp = out[k];
      tmp.push_back( z );
      out[ny+k] = tmp;
    }
    ny = 2*ny;
  }

  for (i=1; i<nout; ++i){
    IntegerVector aa=out[i];
    int M = aa.length();
    IntegerVector vv = no_init( M-1 );
    for (k=1; k<M; ++k){
      vv[k-1]=aa[k];
    }
    out[i-1] = vv;
  }
  out.erase(out.end()-1, out.end());
  return out;
}

template <int RTYPE>
List do_allSubsets (Vector<RTYPE> vn){
  IntegerVector sq = seq_len( vn.size() );
  List lst = allSubsets0__( sq );
  int N=lst.size(), i;
  for (i=0; i<N; ++i){
    lst[i] = vn[ IntegerVector( lst[i] )-1 ];
  }
  return lst;
}

// Works for any type of input vector
// [[Rcpp::export]]
SEXP allSubsets__( SEXP& x){
  int type = TYPEOF(x) ; //Rprintf("type=%i\n", type);
  switch( type ){
  case INTSXP  : return allSubsets0__( x ) ;
  case REALSXP : return do_allSubsets<REALSXP>( x ) ;
  case STRSXP  : return do_allSubsets<STRSXP> ( x ) ;
  }
  return R_NilValue ;
}





/*** R

allSubsets0_R <- function(x) {
        y <- list(vector(mode(x), length = 0))
        for (i in seq_along(x)) {
            y <- c(y, lapply(y, "c", x[i]))
        }
        y[-1L]
    }

allSubsets1_R <- compiler::cmpfun(
function(x){
    out <- vector("list", length=2^length(x))
    ny = 1 # filled elements of out
    for (i in seq_along(x)){
        z=x[i]
        for (k in 1:ny){
            out[[ny + k]] = c(out[[k]],z)
        }
        ny = 2 * ny
    }
    out[-1]
})


allSubsets(1:5)
allSubsets(letters[1:5])


library(microbenchmark)
x <- 1:4
microbenchmark(allSubsets0_R( x ), allSubsets1_R( x ), allSubsets(x))

x <- 1:10
microbenchmark(allSubsets0_R( x ), allSubsets1_R( x ), allSubsets(x))

x <- 1:15
microbenchmark(allSubsets0_R( x ), allSubsets(x),times=10)



 */
