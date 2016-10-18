/* *******************************************************
   
  MAT2ftM and symMAT2ftM2ftM : 

  Coerces dense and sparse matrices to from-to-matrix 

  Author: Soren Hojsgaard, November 2014

********************************************************* */
  

#include <RcppEigen.h>
#include <math.h>       /* fabs */

//[[Rcpp::depends(RcppEigen)]]

#ifndef BEGIN_RCPP
#define BEGIN_RCPP
#endif

#ifndef END_RCPP
#define END_RCPP
#endif

using namespace Rcpp;

typedef Eigen::SparseMatrix<double> SpMat;
typedef Eigen::Map<Eigen::MatrixXd> MapMatd;
typedef Eigen::Map<Eigen::MatrixXi> MapMati;
typedef Eigen::MappedSparseMatrix<double> MSpMat;

template <typename TT>
SEXP do_MAT2ftM_ ( SEXP XX_ ){
  using Eigen::MatrixXd;
  const TT X(as<TT>(XX_));

  int i, j, kk=0, sum=0;
  int nrX(X.rows()), ncX(X.cols());
  for (i=0; i<nrX; i++){
    for (j=00; j<ncX; j++){
      if (X.coeff(i,j))
	sum++;
    }
  }

  NumericMatrix out(sum,2);
  for (i=0; i<nrX; i++){
    for (j=0; j<ncX; j++){
      if(X.coeff(i,j)){
	out(kk,0) = i+1;
	out(kk,1) = j+1;
	kk++;
      }
    }
  }
  return out;  
}



//[[Rcpp::export]]
SEXP MAT2ftM_ ( SEXP XX_ ){
  int type = TYPEOF(XX_) ;
  //Rf_PrintValue(wrap(type));
  switch( type ){
  case INTSXP  : return do_MAT2ftM_<MapMati>(XX_); // matrix - integer 
  case REALSXP : return do_MAT2ftM_<MapMatd>(XX_); // matrix - double
  case S4SXP   : return do_MAT2ftM_<MSpMat>(XX_);  // dgCMatrix
  }
  return R_NilValue ;
}


template <typename TT>
SEXP do_symMAT2ftM_ ( SEXP XX_ ){
  using Eigen::MatrixXd;
  const TT X(as<TT>(XX_));

  int i, j, kk=0, sum=0;
  int nrX(X.rows()), ncX(X.cols());
  for (i=0; i<nrX-1; i++){
    for (j=i+1; j<ncX; j++){
      if (X.coeff(i,j))
	sum++;
    }
  }

  NumericMatrix out(sum,2);
  for (i=0;i<nrX-1;i++){
    for (j=i+1;j<ncX;j++){
      if(X.coeff(i,j)){
	out(kk,0) = i+1;
	out(kk,1) = j+1;
	kk++;
      }
    }
  }
  return out;  
}



//[[Rcpp::export]]
SEXP symMAT2ftM_ ( SEXP XX_ ){
  int type = TYPEOF(XX_) ;
  //Rf_PrintValue(wrap(type));
  switch( type ){
  case INTSXP  : return do_symMAT2ftM_<MapMati>(XX_); // matrix - integer 
  case REALSXP : return do_symMAT2ftM_<MapMatd>(XX_); // matrix - double
  case S4SXP   : return do_symMAT2ftM_<MSpMat>(XX_);  // dgCMatrix
  }
  return R_NilValue ;
}




/*** R


amat<-structure(c(0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 1, 0, 
1, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 
0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0), .Dim = c(7L, 7L), .Dimnames = list(
    c("a", "b", "c", "d", "e", "f", "g"), c("a", "b", "c", "d", 
    "e", "f", "g")))

#matrix2ftM(amat)

amat2 <- structure(c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 1, 0, 
0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0), .Dim = c(7L, 7L), .Dimnames = list(
    c("a", "b", "c", "d", "e", "g", "f"), c("a", "b", "c", "d", 
    "e", "g", "f")))

amat3<-structure(c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 1, 0, 
0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0), .Dim = c(7L, 7L), .Dimnames = list(
    c("a", "b", "c", "d", "e", "g", "f"), c("a", "b", "c", "d", 
    "e", "g", "f")))

#MAT2ftM(as(amat,"dgCMatrix"))
#MAT2ftM(amat)
#storage.mode(amat)<-"integer"
#MAT2ftM(amat)

amat <- structure(c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 1, 0,
0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0), .Dim = c(7L, 7L), .Dimnames = list(
    c("a", "b", "c", "d", "e", "g", "f"), c("a", "b", "c", "d",
    "e", "g", "f")))

mm<-MAT2ftM(amat)

vn<-c("a", "b", "c", "d", "e", "g", "f")
mm2<-vn[mm]
dim(mm2) <- dim(mm)
mm2

em<-structure(c("a", "a", "b", "c", "e", "a", "f", "b", "c", "c",
"d", "d", "e", "g"), .Dim = c(7L, 2L))
em

*/

