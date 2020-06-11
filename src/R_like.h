// ------------------------------------------------------------
//
// Mimicing some R functions
//
// Author: Søren Højsgaard
//
//------------------------------------------------------------ 

#ifndef RLIKE_H
#define RLIKE_H

#include <Rcpp.h>
//[[Rcpp::plugins(cpp11)]]

using namespace Rcpp;
using namespace std;
// using namespace arma;

typedef Rcpp::NumericVector   numVec;
typedef Rcpp::IntegerVector   intVec;
typedef Rcpp::CharacterVector chrVec;
typedef Rcpp::LogicalVector   logVec;


// ### Mimic c() function ###

#define DO_CONCAT							\
  for (int i=0; i<nx; ++i)   out[i] = x[i];				\
  for (int i=0; i<ny; ++i)   out[i+nx] = y[i];				\
  bool has_xnames=!Rf_isNull(x.names());				\
  bool has_ynames = !Rf_isNull(y.names());				\
  if( has_xnames || has_ynames ){					\
    Rcpp::CharacterVector nam( nx + ny );				\
    if ( has_xnames ){							\
      Rcpp::CharacterVector xnam = x.names();				\
      for (int i=0; i<nx; ++i)	nam[i] = xnam[i];			\
    }									\
    if ( has_ynames ){							\
      Rcpp::CharacterVector ynam = y.names();				\
      for (int i=0; i<ny; ++i)	nam[i+nx] = ynam[i];			\
    }									\
    out.names() = nam;							\
  }									\
  
template <typename T>
T do_concat_(const T& x, const T& y){
  int nx=x.size(), ny=y.size();
  T   out=Rcpp::no_init( nx + ny );
  DO_CONCAT;
  return out;
}

// ### Mimic order()
Rcpp::IntegerVector order_(Rcpp::IntegerVector x);

// ### Mimic order()

IntegerVector order2_(SEXP x, bool desc = false);

// ### Mimic which()

IntegerVector which2_ (const SEXP& x);
  
#endif



// NOTE TO SELF: Template can be in .cpp or .h
// template <int RTYPE>
// IntegerVector order_impl(const Vector<RTYPE>& x, bool desc);






































// template <int RTYPE>
// Rcpp::Vector<RTYPE> do_concat2_(const Rcpp::Vector<RTYPE>& x, const Rcpp::Vector<RTYPE>& y){
//   int nx=x.size(), ny=y.size();
//   Rcpp::Vector<RTYPE>   out( nx + ny );
//   DO_CONCAT;
//   return out;
// }



/*** R
     
cppFunction(
"NumericVector f1 (NumericVector a, NumericVector b){
  return do_concat_<NumericVector>(a, b);
}"

)

 ***/


/* // [[Rcpp::export]] */
/* SEXP concat_(const SEXP& x, const SEXP& y){ */
/*   int type = TYPEOF(x) ; // Rprintf("type=%d\n", type); */
/*   switch( type ){ */
/*   case INTSXP   : return do_concat_<IntegerVector> ( x, y ); */
/*   case REALSXP  : return do_concat_<NumericVector>( x, y ); */
/*   case STRSXP   : return do_concat_<CharacterVector> ( x, y ); */
/*   case VECSXP   : return do_concat_<List>( x, y ); */
/*   }   */
/*   return R_NilValue ;  */
/* }  */




/* /\* using namespace Rcpp; *\/ */

// // [[Rcpp::export]]
// SEXP concat2_(const SEXP& x, const SEXP& y){
//   int type = TYPEOF(x) ; // Rprintf("type=%d\n", type);
//   switch( type ){
//   case INTSXP   : return do_concat2_<INTSXP> ( x, y );
//   case REALSXP  : return do_concat2_<REALSXP>( x, y );
//   case STRSXP   : return do_concat2_<STRSXP> ( x, y );
//   case VECSXP   : return do_concat2_<VECSXP> ( x, y );
//   }  
//   return R_NilValue ; 
// } 

