/* File: spdinv-arma.cpp */
#include <RcppArmadillo.h>

// //[[Rcpp::export]]
// SEXP solveSPD ( SEXP X_ ){
//   arma::mat X    = Rcpp::as<arma::mat>(X_);
//   arma::mat Xinv = arma::inv( arma::sympd(X) );
//   return(Rcpp::wrap(Xinv));
// }

//[[Rcpp::export]]
SEXP solveSPD (arma::mat X ){
  //arma::mat X    = Rcpp::as<arma::mat>(X_);
  arma::mat Xinv = arma::inv_sympd(X);
  // arma::mat Xinv = arma::inv( arma::sympd(X) );
  return(Rcpp::wrap(Xinv));
}

