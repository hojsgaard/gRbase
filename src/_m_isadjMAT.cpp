/* **************************************************
  
   Check matrix properties: 
   adjmat_ : a sparse or dense matrix
   
   issymMAT_ (is symmetric)
   isadjMAT_ (is square and 0's on diagonal)
   isugMAT_  (is symmetric and 0's on diagonal)

   Author: Soren Hojsgaard, November 2014

**************************************************** */

#include <RcppEigen.h>
#include "_g_topoSortMAT.h"
//[[Rcpp::depends(RcppEigen)]]

using namespace Rcpp;

#ifndef BEGIN_RCPP
#define BEGIN_RCPP
#endif

#ifndef END_RCPP
#define END_RCPP
#endif

typedef Eigen::MappedSparseMatrix<double> MSpMat;
typedef Eigen::Map<Eigen::MatrixXi> MapMati;
typedef Eigen::Map<Eigen::MatrixXd> MapMatd;

// ------------------------------------------------
//
// --- is symmetric matrix ---
//
// ------------------------------------------------

template <typename TT>
bool do_issymMAT_ ( SEXP X_ ){
  const TT X(as<TT>(X_));
  int i, j, nrX(X.rows()), ncX(X.cols());
  bool out=true;
  if (nrX!=ncX) return false;
	
  for( i=0; i<nrX; ++i){
    for( j=i; j<ncX; ++j ){
      if ( fabs( (double) (X.coeff(i,j)-X.coeff(j,i)) ) > 1e-6 ){
				out=false;
				break;
      }
    }
  }
  return out;
}

// [[Rcpp::export]]
bool issymMAT_ ( SEXP A_ ){
  int type = TYPEOF(A_) ;  //Rf_PrintValue(wrap(type));
  switch( type ){
  case INTSXP  : return do_issymMAT_<MapMati>( A_ ); 
  case REALSXP : return do_issymMAT_<MapMatd>( A_ ); 
  case S4SXP   : return do_issymMAT_<MSpMat>( A_ ); 
  }
  return R_NilValue ;
}


// -----------------------------------------------------
//
// --- is undirected graph ---
// 
// 1) is square 2) has 0 on diagonal 3) is symmetric
//
// -----------------------------------------------------

template <typename TT>
bool do_isugMAT_ ( SEXP X_ ){
  const TT X(as<TT>(X_));
  int i, j, nrX(X.rows()), ncX(X.cols());
  bool out=true;
  if (nrX!=ncX) return false;
	
  for( i=0; i<nrX; ++i){
    if (X.coeff(i,i) != 0){
      out=false;
      break;
    } 
    for( j=i; j<ncX; ++j ){
      if ( fabs( (double) (X.coeff(i,j)-X.coeff(j,i)) ) > 1e-6 ){
				out=false;
				break;
      }
    }
  }
  return out;
}


// [[Rcpp::export]]
bool isugMAT_ ( SEXP A_ ){
  int type = TYPEOF(A_) ;  //Rf_PrintValue(wrap(type));
  switch( type ){
  case INTSXP  : return do_isugMAT_<MapMati>( A_ ); 
  case REALSXP : return do_isugMAT_<MapMatd>( A_ ); 
  case S4SXP   : return do_isugMAT_<MSpMat>( A_ ); 
  }
  return R_NilValue ;
}


// ------------------------------------------------
//
// --- is adjacency matrix ---
//
// 1) is symmetric, 2) has 0 on diagonal
//
// ------------------------------------------------

template <typename TT>
bool do_isadjMAT_ ( SEXP X_ ){
  const TT X(as<TT>(X_));
  int i, nrX(X.rows()), ncX(X.cols());
  bool out=true;
  if (nrX!=ncX) return false;
  for (i=0; i<nrX; ++i){
    if( X.coeff(i,i) != 0 ){
      out=false;
      break;
    }
  }
  return out;
}

// [[Rcpp::export]]
bool isadjMAT_ ( SEXP A_ ){
  int type = TYPEOF(A_) ;  //Rf_PrintValue(wrap(type));
  switch( type ){
  case INTSXP  : return do_isadjMAT_<MapMati>( A_ ); 
  case REALSXP : return do_isadjMAT_<MapMatd>( A_ ); 
  case S4SXP   : return do_isadjMAT_<MSpMat>( A_ ); 
  }
  return R_NilValue ;
}


// ---------------------------------------------------------------
//
// --- is dag ---
// 
// 1) is adjacency (square and 0 on diagonal) and 2) can be given
// topological ordering
//
// ---------------------------------------------------------------

template <typename TT>
bool do_isdagMAT_ ( SEXP X_ ){
  const TT X(as<TT>(X_));
  bool out=false;

  if( do_isadjMAT_<TT>(X_) ){
    IntegerVector outvec = do_topoSortMAT_<TT>(X_);
    if (outvec(0) != -1)
      out = true;
  }
  return out;
}

// [[Rcpp::export]]
bool isdagMAT_ ( SEXP A_ ){
  int type = TYPEOF(A_);  //Rf_PrintValue(wrap(type));
  switch( type ){
  case INTSXP  : return do_isdagMAT_<MapMati>( A_ );
  case REALSXP : return do_isdagMAT_<MapMatd>( A_ );
  case S4SXP   : return do_isdagMAT_<MSpMat>( A_ );
  }
  return R_NilValue ;
}


























/*** R
library(Matrix)
library(gRbase)

M <- ug(~a:b+b:c+c:d+d:a, result="Matrix")
m <- as(M, "matrix")

isadjMAT_(M)
isadjMAT_(m)

m2 <- cbind(m, rep(1,nrow(m)))
M2 <- as(m2, "dgCMatrix")


isadjMAT_(M2)
isadjMAT_(m2)


microbenchmark::microbenchmark(
is.adjMAT(M),
isadjMAT_(M),
is.adjMAT(m),
isadjMAT_(m)
)

microbenchmark::microbenchmark(
isSymmetric(M),
issymMAT_(M),
isSymmetric(m),
issymMAT_(m)
)

microbenchmark::microbenchmark(
is.UG(M),
isugMAT_(M),
is.UG(m),
isugMAT_(m)
)


 */




// template <typename TT>
// bool do_isadjMAT_ ( SEXP X_ ){
//   const TT X(as<TT>(X_));
//   int i, nrX(X.rows()), ncX(X.cols());
//   bool out=true;
//   if (nrX!=ncX) return false;
//   for (i=0; i<nrX; ++i){
//     if( X.coeff(i,i) != 0 ){
//       out=false;
//       break;
//     }
//   }
//   return out;
// }



// typedef Eigen::SparseMatrix<double> SpMat;
// typedef Eigen::SparseVector<double> SpVec;
// typedef SpVec::InnerIterator InIter;


// bool isadjMAT_sp ( SEXP X_ ){
//   SpMat   X(as<SpMat>(X_));
//   int i, nrX(X.rows()), ncX(X.cols());
//   bool out=true;

//   if (nrX!=ncX) return false;

//   for (i=0; i<nrX; ++i){
//     if( X.coeff(i,i) != 0 ){
//       out=false;
//       break;
//     }
//   }
//   return out;
// }


// bool isadjMAT_de ( NumericMatrix X ){
//   int i, nrX(X.rows()), ncX(X.cols());
//   bool out=true;

//   if (nrX!=ncX)
//     return false;

//   for (i=0; i<nrX; ++i){
//     if( X(i,i) != 0 ){
//       out=false;
//       break;
//     }
//   }
//   return out;
// }

// // [[Rcpp::export]]
// bool isadjMAT_ ( SEXP adjmat_ ){
//   int type = TYPEOF(adjmat_) ;
//   //Rf_PrintValue(wrap(type));
//   switch( type ){
//   case INTSXP  : return isadjMAT_de( adjmat_ ); 
//   case REALSXP : return isadjMAT_de( adjmat_ ); 
//   case S4SXP   : return isadjMAT_sp( adjmat_ ); 
//   }
//   return R_NilValue ;
// }




// // [[Rcpp::export]]
// bool isugMAT_sp ( SEXP X_ ){
//   SpMat   X(as<SpMat>(X_));
//   int i, j, nrX(X.rows()), ncX(X.cols());
//   bool out=true;

//   if (nrX!=ncX) return false;
  
//   for( i=0; i<nrX; ++i){
//     if (X.coeff(i,i) != 0){
//       out=false;
//       break;
//     } 
//     for( j=i; j<ncX; ++j ){
//       if ( fabs( X.coeff(i,j)-X.coeff(j,i) ) > 1e-6 ){
// 	out=false;
// 	break;
//       }
//     }
//   }
//   return out;
// }


// // [[Rcpp::export]]
// bool isugMAT_de ( NumericMatrix X ){
//   int i, j, nrX(X.rows()), ncX(X.cols());
//   bool out=true;

//   if (nrX!=ncX) return false;
  
//   for( i=0; i<nrX; ++i){
//     if (X(i,i) != 0){
//       out=false;
//       break;
//     } 
//     for( j=i; j<ncX; ++j ){
//       if ( fabs( X(i,j)-X(j,i) ) > 1e-6 ){
// 	out=false;
// 	break;
//       }
//     }
//   }
//   return out;
// }


// // [[Rcpp::export]]
// bool isugMAT_ ( SEXP adjmat_ ){
//   int type = TYPEOF(adjmat_) ;
//   //Rf_PrintValue(wrap(type));
//   switch( type ){
//   case INTSXP  : 
//   case REALSXP : return isugMAT_de( adjmat_ ); // matrix - double
//   case S4SXP   : return isugMAT_sp( adjmat_ ); // dgCMatrix
//   }
//   return R_NilValue ;
// }




// // [[Rcpp::export]]
// bool issymMAT_sp ( SEXP X_ ){
//   SpMat   X(as<SpMat>(X_));
//   int i, j, nrX(X.rows()), ncX(X.cols());
//   bool out=true;

//   if (nrX!=ncX) return false;
  
//   for( i=0; i<nrX; ++i){
//     for( j=i; j<ncX; ++j ){
//       if ( fabs( X.coeff(i,j)-X.coeff(j,i) ) > 1e-6 ){
// 	out=false;
// 	break;
//       }
//     }
//   }
//   return out;
// }


// // [[Rcpp::export]]
// bool issymMAT_de ( NumericMatrix X ){
//   int i, j, nrX(X.rows()), ncX(X.cols());
//   bool out=true;

//   if (nrX!=ncX) return false;
  
//   for( i=0; i<nrX; ++i){
//     for( j=i; j<ncX; ++j ){
//       if ( fabs( X(i,j)-X(j,i) ) > 1e-6 ){
// 	out=false;
// 	break;
//       }
//     }
//   }
//   return out;
// }

// // [[Rcpp::export]]
// bool issymMAT_ ( SEXP adjmat_ ){
//   int type = TYPEOF(adjmat_) ;
//   //Rf_PrintValue(wrap(type));
//   switch( type ){
//   case INTSXP  : 
//   case REALSXP : return issymMAT_de( adjmat_ ); // matrix - double
//   case S4SXP   : return issymMAT_sp( adjmat_ ); // dgCMatrix
//   }
//   return R_NilValue ;
// }
