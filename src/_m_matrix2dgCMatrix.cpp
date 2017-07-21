/*
  Fast coercion of dense matrix to sparse matrix and vice versa
*/

#include <RcppEigen.h>
//[[Rcpp::depends(RcppEigen)]]

using namespace Rcpp;

typedef Eigen::Map<Eigen::MatrixXd> MapMatd;
typedef Eigen::Map<Eigen::MatrixXi> MapMati;
typedef Eigen::SparseMatrix<double> SpMatd;
typedef Eigen::SparseMatrix<double> SpMat;
typedef Eigen::SparseMatrix<int> SpMati;
typedef Eigen::MappedSparseMatrix<double> MSpMat;

#ifndef BEGIN_RCPP
#define BEGIN_RCPP
#endif

#ifndef END_RCPP
#define END_RCPP
#endif

using namespace Rcpp;

template <typename TT>
SEXP do_matrix2dgCMatrix( SEXP XX_ ){
  NumericMatrix Xin(XX_);
  const TT X(as<TT>(Xin));
  SpMatd Xsparse = X.sparseView();
  S4 Xout(wrap(Xsparse));
  List dn = clone(List(Xin.attr("dimnames")));
  if (dn.length()>0){
    Xout.slot("Dimnames") = dn;
  }
  return(Xout);
};


SEXP do_dgCMatrix2matrix ( SEXP XX_ ){
  S4 DD(wrap(XX_));
  List dn = clone(List(DD.slot("Dimnames")));
  SpMatd X(as<SpMatd>(XX_));
  Eigen::MatrixXd dMat;
  dMat = Eigen::MatrixXd( X );
  NumericMatrix Xout(wrap(dMat));
  Xout.attr("dimnames") = dn;
  return Xout;
}



// [[Rcpp::export]]
SEXP matrix2dgCMatrix_ ( SEXP XX_ ){
  int type = TYPEOF(XX_) ;
  switch( type ){
  case INTSXP  : return do_matrix2dgCMatrix<MapMatd>(XX_); // matrix - integer 
  case REALSXP : return do_matrix2dgCMatrix<MapMatd>(XX_); // matrix - double
  }
  return R_NilValue ;
}

// [[Rcpp::export]]
SEXP dgCMatrix2matrix_ ( SEXP XX_ ){
  int type = TYPEOF(XX_) ;
  switch( type ){
  case S4SXP   : return do_dgCMatrix2matrix(XX_); 
  }
  return R_NilValue ;
}

// [[Rcpp::export]]
SEXP M2dgCMatrix_ ( SEXP XX_ ){
  int type = TYPEOF(XX_) ;
  switch( type ){
  case INTSXP  : return do_matrix2dgCMatrix<MapMatd>(XX_); // matrix - integer 
  case REALSXP : return do_matrix2dgCMatrix<MapMatd>(XX_); // matrix - double
  case S4SXP   : return XX_; // matrix - double
  }
  return R_NilValue ;
}

// [[Rcpp::export]]
SEXP M2matrix_ ( SEXP XX_ ){
  int type = TYPEOF(XX_) ;
  //Rf_PrintValue(wrap(type));
  switch( type ){
  case INTSXP  : return XX_;   
  case REALSXP : return XX_;
  case S4SXP   : return do_dgCMatrix2matrix(XX_); 
  }
  return R_NilValue ;
}












/*** R

library(Matrix)
n <- 1000
mi <- matrix(1:n^2, nrow=n)
mi <- diag(1:n)
dimnames(mi) <- list(as.character(1:n), as.character(1:n))
md <- mi; storage.mode(md)<-"double"
MM <- as(mi, "dgCMatrix")


MAT2matrix <- function(amat){
  if( class(amat)=="dgCMatrix"){
    dgCMatrix2matrix(amat)
  } else if( class(amat)=="matrix"){
    amat
  } else { stop("Can not convert 'amat'")}
}

MAT2dgCMatrix <- function(amat){
  if( class(amat)=="matrix"){
    matrix2dgCMatrix(amat)
  } else if( class(amat)=="dgCMatrix"){
    amat
  } else { stop("Can not convert 'amat'")}
}

require(microbenchmark)
microbenchmark(
as(MM, "dgCMatrix"),
MAT2dgCMatrix( MM )
)

microbenchmark(
as(mi, "dgCMatrix"),
MAT2dgCMatrix( mi )
)

microbenchmark(
as(md, "dgCMatrix"),
MAT2dgCMatrix( md )
)


require(microbenchmark)
microbenchmark(
as.matrix(MM),
MAT2matrix( MM )
)

microbenchmark(
as.matrix( mi ),
MAT2matrix( mi )
)

microbenchmark(
as.matrix( md ),
MAT2matrix( md )
)




*/


