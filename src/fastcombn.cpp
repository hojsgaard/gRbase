#include <RcppArmadillo.h>
#include "R_like.h"
//[[Rcpp::interfaces(r,cpp)]]
//[[Rcpp::depends(RcppEigen,RcppArmadillo)]]

using namespace Rcpp;
// using namespace arma;

typedef Rcpp::NumericVector   numVec;
typedef Rcpp::IntegerVector   intVec;
typedef Rcpp::CharacterVector chrVec;
typedef Rcpp::LogicalVector   logVec;

typedef Rcpp::IntegerMatrix   intMat;

#include <algorithm>
#include <iostream>
#include <string>


//[[Rcpp::export]]
int choose_(int N, int K){
  if (N < K) return 0;
  int num = 1, den=1;
  for (int i=N; i>N-K; i--)
    num *= i;
  for (int i=K; i>1; i--)
    den *= i;
  return num / den;
}

// Source: https://stackoverflow.com/questions/12991758/creating-all-possible-k-combinations-of-n-items-in-c
//[[Rcpp::export]]
IntegerMatrix do_combn(int N, int K){
  int ncol = choose_(N, K);
  int cc = 0, rr = 0;
  IntegerMatrix out(K, ncol);

  std::string bitmask(K, 1); // K leading 1's
  bitmask.resize(N, 0); // N-K trailing 0's
  
  do {
    rr = 0;
    for (int i = 0; i < N; ++i) // [0..N-1] integers
      {
	if (bitmask[i]) {
	  out(rr++, cc) = i + 1; // or i + 1
	}
      }
    cc++;
  } while (std::prev_permutation(bitmask.begin(), bitmask.end()));
  return out;
}


//[[Rcpp::export]]
void next_perm_(IntegerVector& vv){
  int n=vv.length(), ii, jj, ss;

  for (ii=n-1; ii>1; ii--){
    if (vv[ii]==0 && vv[ii-1]==1) break;
  }

  if (ii <= n){
    vv[ii-1] = 0;
    vv[ii]   = 1;
    if (ii < n){
      ss = 0;
      for (jj = ii; jj < n; jj++) ss += vv[jj];
      if (ss > 0){
	for (jj = ii;    jj < ii+ss; jj++) vv[jj] = 1;	
	for (jj = ii+ss; jj < n;     jj++) vv[jj] = 0;
      }
    }
  }
}



//[[Rcpp::export]]
NumericVector oho (NumericVector x, int begin, int end){
  //return x[Range(begin, end)];
  //x[Range(begin, end)] = -1.2;
  IntegerVector ss; 
  x[(ss=seq(begin, end))] = -1.2;
  double s=sum(x);
  Rcout << s << std::endl;
  return x;
}








// template <int RTYPE>
// Matrix<RTYPE> doit(const Vector<RTYPE>& x, const int m){

//   intMat comb = do_combn(x.length(), m);
//   intVec dim = comb.attr("dim");
//   Vector<RTYPE> tmp = x[comb]; // or comb - 1
//   tmp.attr("dim") = dim; 
//   Matrix<RTYPE> out = as<Matrix<RTYPE>>(tmp);
//   return(out);
// }
  
// // [[Rcpp::export]]
// SEXP foo_(const SEXP& x, const int& m){
//   switch( TYPEOF(x) ){
//   case REALSXP: return doit<REALSXP>(x, m);
//   case INTSXP:  return doit<INTSXP>(x, m);
//   case STRSXP:  return doit<STRSXP>(x, m);
//   default: Rf_error("Unsupported type");
//   }
// }































// // Source: https://stackoverflow.com/questions/12991758/creating-all-possible-k-combinations-of-n-items-in-c
// //[[Rcpp::export]]
// void do_comb(int N, int K)
// {
//   std::string bitmask(K, 1); // K leading 1's
//   bitmask.resize(N, 0); // N-K trailing 0's
  
//   // print integers and permute bitmask
//   do {
//     for (int i = 0; i < N; ++i) // [0..N-1] integers
//       {
// 	if (bitmask[i]) std::cout << " " << i;
//       }
//     std::cout << std::endl;
//   } while (std::prev_permutation(bitmask.begin(), bitmask.end()));
// }

// // Source: https://stackoverflow.com/questions/12991758/creating-all-possible-k-combinations-of-n-items-in-c
// //[[Rcpp::export]]
// IntegerMatrix do_combn_slower(int N, int K){
//   int ncol = choose_(N, K), cc = 0;
//   IntegerMatrix out(K, ncol);
//   IntegerVector tmp(K);

//   std::string bitmask(K, 1); // K leading 1's
//   bitmask.resize(N, 0); // N-K trailing 0's
  
//   // print integers and permute bitmask
//   do {
//     for (int i = 0, idx = 0; i < N; ++i) // [0..N-1] integers
//       {
// 	if (bitmask[i]) {
// 	  tmp[idx++] = i;
// 	  // std::cout << " " << i;
// 	}
//       }
//     // std::cout << std::endl;
//     // print(tmp);
//     out(_, cc++) = tmp; // tmp+1
//   } while (std::prev_permutation(bitmask.begin(), bitmask.end()));
//   // print(out);
//   return out;
// }


// //[[Rcpp::export]]
// unsigned choose2_( unsigned n, unsigned k )
// {
//     if (k > n) return 0;
//     if (k * 2 > n) k = n-k;
//     if (k == 0) return 1;

//     int result = n;
//     for( int i = 2; i <= k; ++i ) {
//         result *= (n-i+1);
//         result /= i;
//     }
//     return result;
// }



// //[[Rcpp::export]]
// IntegerMatrix do_combn2(int N, int K, int ncol){
//   // int ncol = choose_(N, K),
//   int cc = 0, rr = 0;
//   IntegerMatrix out(K, ncol);

//   std::string bitmask(K, 1); // K leading 1's
//   bitmask.resize(N, 0); // N-K trailing 0's
  
//   do {
//     rr = 0;
//     for (int i = 0, idx = 0; i < N; ++i) // [0..N-1] integers
//       {
// 	if (bitmask[i]) {
// 	  out(rr++, cc) = i + 1; // or i + 1
// 	}
//       }
//     cc++;
//   } while (std::prev_permutation(bitmask.begin(), bitmask.end()));
//   return out;
// }

