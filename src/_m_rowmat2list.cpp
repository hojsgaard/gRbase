#include <Rcpp.h>

using namespace Rcpp;

template <typename TT>
SEXP do_rowmat2list( SEXP XX_ ){
  TT X(XX_);
  int nr=X.nrow();
  List out(nr);
  for (int ii=0; ii<nr; ii++){
    out[ii] = X(ii,_);
  }
  return(out);
}

template <typename TT>
SEXP do_colmat2list( SEXP XX_ ){
  TT X(XX_);
  int nc=X.ncol();
  List out(nc);
  for (int ii=0; ii<nc; ii++){
    out[ii] =  X( _, ii);
  }
  return(out);
}

List do_colmat2list_str(SEXP XX_){
  CharacterMatrix X(XX_);
  int nc=X.ncol(), nr=X.nrow(), k;
  List out(nc);
  for (int j=0; j<nc; j++){
	CharacterVector v(nr);
    for (k=0; k<nr; ++k){
      v[ k ] = X( k, j);
    }
    out[j] =  v ; 
  }
  return out;
}

// [[Rcpp::export]]
SEXP rowmat2list ( SEXP XX_ ){
  int type = TYPEOF(XX_) ;  //Rf_PrintValue(wrap(type));
  switch( type ){
  case INTSXP  : return do_rowmat2list<IntegerMatrix>(XX_); 
  case REALSXP : return do_rowmat2list<NumericMatrix>(XX_); 
  case STRSXP  : return do_rowmat2list<CharacterMatrix>(XX_); 
  }                                    
  return List(0);
}

// [[Rcpp::export]]
SEXP colmat2list ( SEXP XX_ ){
  int type = TYPEOF(XX_) ;  //Rf_PrintValue(wrap(type));
  switch( type ){
  case INTSXP  : return do_colmat2list<IntegerMatrix>(XX_); 
  case REALSXP : return do_colmat2list<NumericMatrix>(XX_); 
  case STRSXP  : return do_colmat2list_str(XX_); // FIXME bug in Rcpp???  
  }                                    
  return List(0) ;
}

	


/*** R

m1 <- matrix(1:16,4)
m2 <- matrix(letters[1:16], 4)

m2 <- matrix(as.character(1:10000), 100)

imat <- matrix(1:90, nrow=10)
dmat <- 1.0*imat
cmat <- matrix(as.character(1:90), nrow=10)

ilist <- rowmat2list(imat); #sapply(ilist, class)
dlist <- rowmat2list(dmat); #sapply(dlist, class)
clist <- rowmat2list(cmat); #sapply(clist, class)

ilist <- colmat2list(imat); #sapply(ilist, class)
dlist <- colmat2list(dmat); #sapply(dlist, class)
clist <- colmat2list(cmat); #sapply(clist, class)

#library(microbenchmark)
#microbenchmark(gRbase::rowmat2list(imat), rowmat2list(imat))
#microbenchmark(gRbase::colmat2list(imat), colmat2list(imat))
#microbenchmark(gRbase::rowmat2list(cmat), rowmat2list(cmat))
#microbenchmark(gRbase::colmat2list(cmat), colmat2list(cmat))
 */

