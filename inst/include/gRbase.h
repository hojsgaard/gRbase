
#ifndef __gRbase_h__
#define __gRbase_h__

#include <RcppArmadillo.h>
#define NDEBUG 1
#include <RcppEigen.h>

typedef Rcpp::IntegerVector intVec;
typedef Rcpp::NumericVector numVec;
typedef Eigen::SparseMatrix<double> SpMat;
typedef Eigen::Map<Eigen::MatrixXd> MapMatd;
typedef Eigen::Triplet<double> T;
typedef Eigen::MappedSparseMatrix<double> MSpMat;

#include "gRbase_RcppExports.h"

#endif // __gRbase_h__
