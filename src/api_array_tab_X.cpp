// ------------------------------------------------------------
//
// Rcpp functions for fast table operations.
//
// Author: Søren Højsgaard
//
// ------------------------------------------------------------


//' @title Table operations implemented in c++
//' @description Table operations implemented in c++. Corresponding R
//'    functions without the trailing underscore exist.
//' @name api-tabX_
//' @inheritParams api-tabX
//' @param op The operation to be carried out; "+", "-", "*", "/".

#include <Rcpp.h>
#include "concatenate.h"         // my version of c(x,y)
#include "t_array_properties.h"  

using namespace Rcpp;
using namespace std;
//[[Rcpp::interfaces(r,cpp)]]

typedef Rcpp::NumericVector   numVec;
typedef Rcpp::IntegerVector   intVec;
typedef Rcpp::CharacterVector chrVec;

// Dispatch on first argument type
#define DISPATCH1_METHOD(method, x1, x2)	\
  switch( TYPEOF(x1) ){				\
  case REALSXP: return method<REALSXP>(x1, x2);	\
  case INTSXP:  return method<INTSXP>(x1, x2);	\
  case STRSXP:  return method<STRSXP>(x1, x2);	\
  default: Rf_error("Unsupported type");	\
  }						\

// FIXME: 3/1/2015: A hack because setequal is broken
bool seteq_(CharacterVector x, CharacterVector y){
  return
	(((CharacterVector) setdiff(x,y)).length()==0) &
	(((CharacterVector) setdiff(y,x)).length()==0) ;
}

#define namesDimnames(tab) (((List) tab.attr("dimnames")).names())
#define dimnames(tab) ((List) tab.attr("dimnames"))
#define dim(tab) tab.attr("dim")

// ------------------------------------------------------------
// tab_perm__
//
// Implementation of array permutation; very similar
// to R's implementation of aperm()
//
// Author: Søren Højsgaard
// ------------------------------------------------------------

bool is_valid_perm_(const IntegerVector& dim, const IntegerVector& permi){
  bool out = false;
  if (dim.length() != permi.length()){
    ::Rf_error("'perm' is of wrong length");		
  } else {
    IntegerVector perm2 = unique(permi);
    if (any(is_na(perm2))){
      ::Rf_error("value out of range in 'perm'");		
    } else {
      if (min(perm2) == 1 && max(perm2) == dim.length()){
	out = true;
      } else ::Rf_error("invalid permutation");		
    }
  }
  return out;
}

// This function also exists elsewhere
inline IntegerVector make_prod__(const IntegerVector& adim ){
  IntegerVector plevels = no_init( adim.length() );
  plevels[0] = 1;
  for (int i = 1; i < adim.length(); ++i){
    plevels[i] =  adim[i - 1] * plevels[i - 1];
  }
  return plevels;
}

#define DO_CELL								\
  cell_number = - offset;						\
  for (k=0; k < ndim; ++k){						\
    cell_number += pvec_perm[k] * cell[k] ;				\
  }									\
  for (k=0; k < ndim; ++k){						\
    if (cell[k] == adim_new[k])						\
      cell[k] = 1;							\
    else{								\
      ++cell[k];							\
      break;								\
    }									\
  };

template <int RTYPE>
Vector<RTYPE> do_aperm_vec(const Vector<RTYPE>& tab,
			   const IntegerVector& adim,
			   const IntegerVector& permi){

  bool is_ok_perm = is_valid_perm_(adim, permi);
  if (!is_ok_perm) stop("invalid permutation; can not proceed");

  int ncells = tab.length(), ndim = adim.length(), i, k, cell_number, n, offset=0;
  Vector<RTYPE> out  = no_init( ncells );
  IntegerVector cell = no_init( ndim );

  IntegerVector pvec  = make_prod__(adim);
  IntegerVector pvec_perm =no_init( ndim ), perm0 = no_init( ndim ),
    adim_new=no_init( ndim );
  
  for (i=0; i < ndim; i++){
    cell[i]      = 1;
    n            = permi[i] - 1;
    perm0[i]     = n;
    pvec_perm[i] = pvec[n];
    adim_new[i]  = adim[n];
    offset       += pvec_perm[i];
  }
  
  for (i=0; i < ncells; ++i){
    DO_CELL;
    out[i] = tab[cell_number];
  }
  return out ;
}

template <int RTYPE>
Vector<RTYPE> do_aperm_gen(const Vector<RTYPE>& tab, const SEXP& perm){

  List            dn1 = dimnames(tab);
  IntegerVector   di1 = dim(tab);

  int type = TYPEOF(perm) ;
  switch( type ){
  case INTSXP  : 
  case REALSXP : {
    Vector<RTYPE>   out = do_aperm_vec<RTYPE>(tab, di1, perm);
    out.attr("dim")     = di1[ ((IntegerVector) perm) - 1 ];
    out.attr("dimnames")= dn1[ ((IntegerVector) perm) - 1 ];
    return out;
  } ;
  case STRSXP  : {
    CharacterVector vn1 = dn1.names();
    IntegerVector permi = match(((CharacterVector) perm), vn1);

    Vector<RTYPE>   out = do_aperm_vec<RTYPE>(tab, di1, permi);
    out.attr("dim")     = di1[ ((IntegerVector) permi) - 1 ];
    out.attr("dimnames")= dn1[ ((IntegerVector) permi) - 1 ];
    return out;
  } ;
  }
  return R_NilValue ;
}





//' @rdname api-tabX_
// [[Rcpp::export]]
SEXP tab_perm_(const SEXP& tab, const SEXP& perm){
  DISPATCH1_METHOD(do_aperm_gen, tab, perm);
  return R_NilValue ;
}



// -------------------------------------------------------------------
//
// tab_expand_, tab_align_
//
// Expand and align tables
//
// Author: Søren Højsgaard
//
// Details:
//
// tab_expand_(tab1, dn2):
// -----------------------
// tab1 is a array; dn2 is specification of extra dimnames.
//
// To be specific, let tab1 have variables v1, and dn2 have names v2.
// tab_expand_ expands tab1 to a table, say 'out' with variables (v2,
// v1 \ v2) such that the variables v2 vary fastest.
//
// The code goes as follows:
//
// (a)  if v2 is subset of v1 (ie if v2 \ v1 = emptyset):
// (a1) if v2-variables are first in v1, then return a copy of tab1
// (a2) else permute tab1 so that v2-variables come first, and return this
// permuted table
//
// (b) else (v2 is NOT subset of v1) then form table with variables
// (v1, v2\v1). Then permute this table so that the v2-variable go
// first.
// 
// tab_align_(tab1, tab2):
// -----------------------
// Align tab1 with tab2 if possible: 
// If tab1 and tab2 have the same variable names then tab1 is permuted so
// that the variables in tab1 is in the same order as the variables in
// tab2.
//
// -------------------------------------------------------------------


template <int RTYPE>
Vector<RTYPE> do_tab_expand_gen(const Vector<RTYPE>& tab1, const List& dn2, const int& exptype = 0){
  List   dn1=tab1.attr("dimnames");
  intVec di1=sapply(dn1, Rf_length), di2=sapply(dn2, Rf_length);
  chrVec vn1=dn1.names(),            vn2=dn2.names();

  //  d21 = v2 \ v1
  chrVec d21  = setdiff( vn2, vn1 );
  //Rcpp::Rcout << "d21 : " << d21 << std::endl;
  
  if ( d21.size() == 0 ){  // (a) v2 is subset of v1
    // Rprintf("++ no augmentation needed\n");
    // create variable vector vn = { vn2 , vn1\vn2 }
    chrVec d12 = setdiff( vn1, vn2 );
    chrVec vn  = do_concat_<chrVec>( vn2, d12 );

    // check if condition (a1) or (a2)
    intVec  perm = match(vn, vn1);

    // Check if permutation is needed
    int chk = sum( abs( perm - seq(1, vn1.size()) ) );
    if (chk == 0){ // condition (a1)
      //Rprintf("++ ++ no permutation needed; we are done\n");
      return clone(tab1);
    } else { // condition (a2)
      //Rprintf("++ ++ permutation needed\n");
      Vector<RTYPE> out   = do_aperm_vec<RTYPE>(tab1, di1, perm);
      out.attr("dim")     = di1[ perm - 1 ];
      out.attr("dimnames")= dn1[ perm - 1 ];
      return out;
    }
  } else { // condition (b): v2 is NOT subset of v1
    // Rprintf("++ augmentation of table needed\n");
    
    // Create table with variables {v1, v2\v1}:

    // 1: need to know where the d21's are to pick out dims and dimensions
    intVec d21_idx  = match( d21, vn2 );
    List   d21_dn   = dn2[ d21_idx - 1 ];
    intVec d21_di   = di2[ d21_idx - 1 ];

    // List lst1 = List::create(Named("d21")=d21, Named("vn2")=vn2,
    // 			     Named("d21_idx")=d21_idx, Named("d21_dn")=d21_dn,
    // 			     Named("d21_di")=d21_di);
    // Rf_PrintValue(lst1);
    
    // 2: find product of added dimensions
    double extra=1;
    for (int i=0; i < d21_di.length(); ++i){ extra *= d21_di[i];}

    // 3: find: vars, dimnames etc in {v1, v2\v1}-table
    chrVec vn_aug  = do_concat_<chrVec>(vn1, d21);
    List   dn_aug  = do_concat_<List>(dn1, d21_dn);
    intVec dim_aug = do_concat_<intVec>(di1, d21_di);
    //numVec aug     = rep(tab1, e);

    // List lst2 = List::create(Named("vn_aug")=vn_aug, Named("dn_aug")=dn_aug, Named("dim_aug")=dim_aug);
    // Rf_PrintValue(lst2);
    
    // FIXME: This is where we shall make sure there are zeros in the right places.
    //Vector<RTYPE> aug     = rep(tab1, e);
    int tab1_len = tab1.length();
    int new_len  = tab1_len * extra;

    //Rcpp::Rcout << "e : " << e << "tab1_len : " << tab1_len << std::endl;
    Vector<RTYPE> aug (new_len);
    //Rf_PrintValue(wrap(aug));


	  
        
    if (exptype == 0){
      for (int k=0; k<extra; k++){
	for (int i=0; i<tab1_len; i++){ 
	  aug[i + k * tab1_len] = tab1[i] ;
	}
      }      
    } else if (exptype == 1){
      for (int k=0; k<extra; k++){
    	for (int i=0; i<tab1_len; i++){ 
    	  aug[i + k * tab1_len] = tab1[i] ;
    	}
      }
      aug = aug / extra;    
    } else if (exptype == 2){
      for (int i=0; i<tab1_len; i++){ 
    	aug[i] = tab1[i];
      }      
    } else Rf_error("invalid 'exptype'");
    
    

    
    // 4: need to reorder aug so that vn2-vars go first
    chrVec d12  = setdiff(vn1, vn2);
    chrVec vn   = do_concat_<chrVec>(vn2, d12);
    intVec perm = match(vn, vn_aug);

    int chk = sum(abs(perm - seq(1, vn_aug.size())));		
    if (chk == 0){ // don't think this can happen!
      //Rprintf("++ ++ no permutation needed; we are done\n");
      aug.attr("dim")      = dim_aug;
      aug.attr("dimnames") = dn_aug;
      return aug;
    } else {
      //Rprintf("++ ++ permutation needed\n");
      //numVec out = do_aperm_vec<REALSXP>(aug, dim_aug, perm);
      // Vector<RTYPE> out = do_aperm_vec<RTYPE>(aug, dim_aug, perm);
      // out.attr("dim")     = dim_aug[ perm - 1 ];
      // out.attr("dimnames")= dn_aug [ perm - 1 ];      
      // return out;
      aug.attr("dim")      = dim_aug;
      aug.attr("dimnames") = dn_aug;
      return aug;

    }
  }

}


// We need a list with extra dimnames (tab2) to expand an array, but if we are
// instead given a named array then we can extract info from there.

//' @rdname api-tabX_
// [[Rcpp::export]]
SEXP tab_expand_(const SEXP& tab, const SEXP& aux, const int& type=0){
  List dn;
  if (is_dimnames_(aux)){
    dn = (List) aux;
  } else if (is_named_array_(aux)){
    dn = ((numVec) aux).attr("dimnames"); // FIXME: What is this??
  } else ::Rf_error("dont know what to do");		

  switch(TYPEOF(tab)){
  case REALSXP: return do_tab_expand_gen<REALSXP>(tab, aux, type);
  case INTSXP: return do_tab_expand_gen<INTSXP>(tab, aux, type);
  default: Rf_error("Unsupported type");
  }

  //DISPATCH1_METHOD(do_tab_expand_gen, tab, dn);
  return R_NilValue ;  
}



template <int RTYPE>
Vector<RTYPE> do_foo(const Vector<RTYPE>& tab, NumericVector aux){
  Vector<RTYPE> aug (tab.length() * 2);
  IntegerVector ii = seq(0,6);

  double ee = tab.length();
  aug[ii] = tab[ii];

  aug = aug / ee;
  
  return aug;
}
  
//[[Rcpp::export]]
SEXP foo (const SEXP& tab, SEXP& aux){
  switch(TYPEOF(tab)){
  case REALSXP: return do_foo<REALSXP>(tab, aux);
  case INTSXP: return do_foo<INTSXP>(tab, aux);
  default: Rf_error("Unsupported type");
  }
  return R_NilValue;
}





// Note: is_dimnames_(x) checks if x is a list an nothing model

//' @rdname api-tabX_
//[[Rcpp::export]]
SEXP tab_align_(const SEXP& tab1, const SEXP& tab2){

  chrVec vn1 = namesDimnames( as<RObject>(tab1) );
  chrVec vn2;
  
  if (is_dimnames_(tab2)){
    vn2 = ((List) tab2).names();
  }
  else if (is_named_array_(tab2))
    vn2 = namesDimnames( as<RObject>(tab2) );
  else ::Rf_error("dont know what to do");		
  
  if (seteq_(vn1, vn2)){
    return tab_expand_(tab1, tab2);
  } else return R_NilValue ;
}
  


// //[[Rcpp::export]]
// SEXP tab_align_(const SEXP& tab1, const SEXP& tab2){

//   chrVec vn1 = namesDimnames( as<RObject>(tab1) );
//   chrVec vn2 = namesDimnames( as<RObject>(tab2) );
    
//   if (seteq_(vn1, vn2)){
//     return tab_expand_(tab1, tab2);
//   } else return R_NilValue ;
// }



// -------------------------------------------------------------------
// tab_marg__
//
// Find marginal tables
//
// Author: Søren Højsgaard
//
// -------------------------------------------------------------------

template <int RTYPE>
Vector<RTYPE> do_margc_tab(const Vector<RTYPE>& tab1, const CharacterVector& margc){

  if (margc.length() == 0){
    double s = sum(tab1);
    Vector<RTYPE> out(1);
    out(0)= s;
    return out;    
  }
  
  List   dn1 = tab1.attr("dimnames");
  chrVec vn1 = dn1.names();
  intVec di1 = tab1.attr("dim");

  intVec marg_idx = match(margc, vn1);
  if (any (is_na( marg_idx ) ))
    stop("Invalid margc specification\n");

  chrVec rest   = setdiff(vn1, margc);
  //CharacterVector vn_new = do_concat_<Vector<STRSXP> >(rest, margc);
  chrVec vn_new = do_concat_<chrVec>(rest, margc);
  intVec perm     = match( vn_new, vn1 );
  Vector<RTYPE> out      = do_aperm_vec<RTYPE>(tab1, di1, perm);

  if (rest.length() == 0){    //Rprintf("no marginalization needed\n");
    out.attr("dim")      = di1[ perm - 1 ];
    out.attr("dimnames") = dn1[ perm - 1];
    return out;
  } else {                  //Rprintf("marginalization needed\n");
    int ncells=1, margcells=1;
    for (int i=0; i < di1.length(); ++i)
      ncells *= di1[i];
    for (int i=0; i < marg_idx.length(); ++i)
      margcells *= di1[marg_idx[i] - 1];
    int restcells = ncells / margcells;
    
    // Create output
    Vector<RTYPE> ret = no_init( margcells );
    typedef typename traits::storage_type<RTYPE>::type storage_t;
    storage_t sum;
    int offset;
    for (int j=0; j < margcells; ++j){
      sum    = 0;
      offset = restcells * j;
      for (int i=0; i < restcells; ++i)
	sum += out[i + offset];
      ret[j] = sum;
    }
    ret.attr("dim")      = di1[ marg_idx - 1 ];
    ret.attr("dimnames") = dn1[ marg_idx - 1 ];
    return ret;
  }
}

template <int RTYPE>
Vector<RTYPE> do_tabmarg_gen(const Vector<RTYPE>& tab, const SEXP& marg){
  int type = TYPEOF(marg) ;
  switch( type ){
  case NILSXP  : {
    double s = sum(tab);
    Vector<RTYPE> out(1);
    out(0)= s;
    return out;
  }    
  case INTSXP  : 
  case REALSXP : {
    chrVec vn1 = namesDimnames( tab );
    chrVec margc=vn1[((IntegerVector) marg) - 1];
    Vector<RTYPE>   out = do_margc_tab<RTYPE>(tab, margc);
    return out;
  } ;
  case STRSXP : {
    Vector<RTYPE>   out = do_margc_tab<RTYPE>(tab, (chrVec) marg);
    return out;
  }
  }
  return R_NilValue ;
}

//' @rdname api-tabX_
// [[Rcpp::export]]
SEXP tab_marg_(const SEXP& tab, const SEXP& marg){
  switch( TYPEOF(tab) ){
  case REALSXP: return do_tabmarg_gen<REALSXP>(tab, marg);
  case INTSXP:  return do_tabmarg_gen<INTSXP>(tab, marg);
  default: Rf_error("Unsupported type");
  }
  return R_NilValue ;
}



// -------------------------------------------------------------
//
// Algebraic operations (+, -, *, /, ==) on arrays
// 
// Ft. Lauderdale spring 2017,
//
// Author: Søren Højsgaard
//
// ------------------------------------------------------------- 

// //' @title Internal Rcpp functions
// //' @description Automatically generated documentation of Rcpp functions.
// //' @name internal_grbase_cpp
// //' @aliases tab_list_mult_ tab_list_add_ tab_op_ tab_add_ tab_subt_
// //' tab_mult_ tab_div_ tab_div0_ tab_equal_


#define _tab_op_loopit(_op_)						\
  int n1 = out.size(), n2=tab2.size(), n3=n1/n2, zz;			\
  for (int i=0; i < n2; ++i){						\
    for (int k=0; k < n3; ++k){						\
      zz = i + k * n2;							\
      out[zz] = out[zz] _op_ tab2[i];					\
    }									\
  }									\

//' @rdname api-tabX_
//[[Rcpp::export]]
NumericVector tab_op_(const NumericVector& tab1, const NumericVector& tab2, const char op='*'){

  if (!dimnames_match_( tab1, tab2)) ::Rf_error("dimnames do not match");
  NumericVector out = tab_expand_(tab1, tab2);
  
  switch(op){
  case '+' : {  _tab_op_loopit(+); break;}
  case '-' : {  _tab_op_loopit(-); break;}
  case '*' : {  _tab_op_loopit(*); break;}
  case '/' : {  _tab_op_loopit(/); break;}
  default : stop("'op' is an undefined operation");
  }
  // Rcout << "out (+) : " << out << std::endl;
  return out;
}

//' @rdname api-tabX_
//[[Rcpp::export]]
NumericVector tab_add_(const NumericVector& tab1, const NumericVector& tab2){
  return tab_op_(tab1, tab2, '+');
}

//' @rdname api-tabX_
//[[Rcpp::export]]
NumericVector tab_subt_(const NumericVector& tab1, const NumericVector& tab2){
  return tab_op_(tab1, tab2, '-');
}

//' @rdname api-tabX_
//[[Rcpp::export]]
NumericVector tab_mult_(const NumericVector& tab1, const NumericVector& tab2){
  return tab_op_(tab1, tab2, '*');
}

//' @rdname api-tabX_
//[[Rcpp::export]]
NumericVector tab_div_(const NumericVector& tab1, const NumericVector& tab2){
  return tab_op_(tab1, tab2, '/');
}

//' @rdname api-tabX_
//[[Rcpp::export]]
NumericVector tab_div0_(const NumericVector& tab1, const NumericVector& tab2){
  NumericVector out = tab_op_(tab1, tab2, '/');
  for (int i=0; i < out.length(); ++i){
    if (std::isinf(out[i]) || NumericVector::is_na(out[i]))
      out[i] = 0;
  }
  return out;
}

//' @rdname api-tabX_
//[[Rcpp::export]]
bool tab_equal_(const NumericVector& tab1, const NumericVector& tab2, double eps=1e-12){

  CharacterVector vn1 = namesDimnames(tab1), vn2 = namesDimnames(tab2);

  if (seteq_( vn1, vn2 )){
    NumericVector dif = tab_subt_(tab_expand_(tab1, tab2), tab2);
    return (sum( abs( dif ) )  <  eps);
  } else {
    return false;
  }
}





// -----------------------------------------------------------
//
// Multiply and add lists of tables
//
// Ft. Lauderdale spring 2017,
//
// Soren Hojsgaard
//
// -----------------------------------------------------------

#define looplist(_fun_)				\
  int len_lst = lst.length();			\
  if (len_lst == 0) return NumericVector(0);	\
  else {					\
    NumericVector out = lst[0];			\
    for (int i=1; i < len_lst; ++i){		\
      out = _fun_(out, lst[i]);			\
    }						\
    return out;					\
  }						\

//' @rdname api-tabX_
//[[Rcpp::export]]
NumericVector tab_list_mult_(const List& lst){
  looplist(tab_mult_);
}

//' @rdname api-tabX_
//[[Rcpp::export]]
NumericVector tab_list_add_(const List& lst){
  looplist(tab_add_);
}


// -------------------------------------------------------
//
// FIXME: ALIASES for gRain compatibility; July 2017
//
// -------------------------------------------------------


// [[Rcpp::export]]
SEXP tabMarg__(const SEXP& tab, const SEXP& marg){
  return tab_marg_(tab, marg);
}

//[[Rcpp::export]]
NumericVector tabDiv0__(const NumericVector& tab1, const NumericVector& tab2){
  return tab_div0_(tab1, tab2);
}

//[[Rcpp::export]]
NumericVector tabMult__(const NumericVector& tab1, const NumericVector& tab2){
  return tab_mult_(tab1, tab2);
}


























// template <int RTYPE>
// Vector<RTYPE> do_tab_expand_gen(const Vector<RTYPE>& tab1, const List& dn2){
//   List   dn1=tab1.attr("dimnames");
//   intVec di1=sapply(dn1, Rf_length), di2=sapply(dn2, Rf_length);
//   chrVec vn1=dn1.names(),            vn2=dn2.names();
	
//   //  d21 = v2 \ v1
//   chrVec d21  = setdiff( vn2, vn1 );
  
//   if ( d21.size() == 0 ){  // (a) v2 is subset of v1
//     // Rprintf("++ no augmentation needed\n");

//     // create variable vector vn = { vn2 , vn1\vn2 }
//     chrVec d12 = setdiff( vn1, vn2 );
//     chrVec vn  = do_concat_<chrVec>( vn2, d12 );

//     // check if condition (a1) or (a2)
//     intVec  perm = match(vn, vn1);

//     // Check if permutation is needed
//     int chk = sum( abs( perm - seq(1, vn1.size()) ) );
//     if (chk == 0){ // condition (a1)
//       //Rprintf("++ ++ no permutation needed; we are done\n");
//       return clone(tab1);
//     } else { // condition (a2)
//       //Rprintf("++ ++ permutation needed\n");
//       Vector<RTYPE> out   = do_aperm_vec<RTYPE>(tab1, di1, perm);
//       out.attr("dim")     = di1[ perm - 1 ];
//       out.attr("dimnames")= dn1[ perm - 1 ];
//       return out;
//     }
//   } else { // condition (b): v2 is NOT subset of v1
//     // Rprintf("++ augmentation of table needed\n");
    
//     // Create table with variables {v1, v2\v1}:

//     // 1: need to know where the d21's are to pick out dims and dimensions
//     intVec d21_idx  = match( d21, vn2 );
//     List   d21_dn   = dn2[ d21_idx - 1 ];
//     intVec d21_di   = di2[ d21_idx - 1 ];

//     // 2: find product of added dimensions
//     int e=1;
//     for (int i=0; i < d21_di.length(); ++i){ e *= d21_di[i];}

//     // 3: find: vars, dimnames etc in {v1, v2\v1}-table
//     chrVec vn_aug  = do_concat_<chrVec>( vn1, d21 );
//     List   dn_aug  = do_concat_<List>( dn1, d21_dn );
//     intVec dim_aug = do_concat_<intVec>(di1, d21_di);
//     //numVec aug     = rep(tab1, e);

//     // FIXME: This is where we shall make sure there are zeros in the right places.
//     Vector<RTYPE> aug     = rep(tab1, e);

//     // 4: need to reorder aug so that vn2-vars go first
//     chrVec d12  = setdiff(vn1, vn2);
//     chrVec vn   = do_concat_<chrVec>(vn2, d12);
//     intVec perm = match(vn, vn_aug);

//     int chk = sum( abs( perm - seq(1, vn_aug.size()) ) );		
//     if (chk == 0){ // don't think this can happen!
//       //Rprintf("++ ++ no permutation needed; we are done\n");
//       aug.attr("dim")      = dim_aug;
//       aug.attr("dimnames") = dn_aug;
//       return aug;
//     } else {
//       //Rprintf("++ ++ permutation needed\n");
//       //numVec out = do_aperm_vec<REALSXP>(aug, dim_aug, perm);
//       Vector<RTYPE> out = do_aperm_vec<RTYPE>(aug, dim_aug, perm);
//       out.attr("dim")     = dim_aug[ perm - 1 ];
//       out.attr("dimnames")= dn_aug [ perm - 1 ];
//       return out;
//     }
//   }

// }




// template <int RTYPE>
// Vector<RTYPE> do_tab_expand_gen(const Vector<RTYPE>& tab1, const List& dn2){
//   List   dn1=tab1.attr("dimnames");
//   intVec di1=sapply(dn1, Rf_length), di2=sapply(dn2, Rf_length);
//   chrVec vn1=dn1.names(),            vn2=dn2.names();

//   //  d21 = v2 \ v1
//   chrVec d21  = setdiff( vn2, vn1 );
//   //Rcpp::Rcout << "d21 : " << d21 << std::endl;
  
//   if ( d21.size() == 0 ){  // (a) v2 is subset of v1
//     // Rprintf("++ no augmentation needed\n");
//     // create variable vector vn = { vn2 , vn1\vn2 }
//     chrVec d12 = setdiff( vn1, vn2 );
//     chrVec vn  = do_concat_<chrVec>( vn2, d12 );

//     // check if condition (a1) or (a2)
//     intVec  perm = match(vn, vn1);

//     // Check if permutation is needed
//     int chk = sum( abs( perm - seq(1, vn1.size()) ) );
//     if (chk == 0){ // condition (a1)
//       //Rprintf("++ ++ no permutation needed; we are done\n");
//       return clone(tab1);
//     } else { // condition (a2)
//       //Rprintf("++ ++ permutation needed\n");
//       Vector<RTYPE> out   = do_aperm_vec<RTYPE>(tab1, di1, perm);
//       out.attr("dim")     = di1[ perm - 1 ];
//       out.attr("dimnames")= dn1[ perm - 1 ];
//       return out;
//     }
//   } else { // condition (b): v2 is NOT subset of v1
//     // Rprintf("++ augmentation of table needed\n");
    
//     // Create table with variables {v1, v2\v1}:

//     // 1: need to know where the d21's are to pick out dims and dimensions
//     intVec d21_idx  = match( d21, vn2 );
//     List   d21_dn   = dn2[ d21_idx - 1 ];
//     intVec d21_di   = di2[ d21_idx - 1 ];

//     // List lst1 = List::create(Named("d21")=d21, Named("vn2")=vn2,
//     // 			     Named("d21_idx")=d21_idx, Named("d21_dn")=d21_dn,
//     // 			     Named("d21_di")=d21_di);
//     // Rf_PrintValue(lst1);
    
//     // 2: find product of added dimensions
//     int e=1;
//     for (int i=0; i < d21_di.length(); ++i){ e *= d21_di[i];}

//     // 3: find: vars, dimnames etc in {v1, v2\v1}-table
//     chrVec vn_aug  = do_concat_<chrVec>(vn1, d21);
//     List   dn_aug  = do_concat_<List>(dn1, d21_dn);
//     intVec dim_aug = do_concat_<intVec>(di1, d21_di);
//     //numVec aug     = rep(tab1, e);

//     // List lst2 = List::create(Named("vn_aug")=vn_aug, Named("dn_aug")=dn_aug, Named("dim_aug")=dim_aug);
//     // Rf_PrintValue(lst2);
    
//     // FIXME: This is where we shall make sure there are zeros in the right places.
//     //Vector<RTYPE> aug     = rep(tab1, e);
//     int len=tab1.length();
//     int new_len = len * e;

//     //Rcpp::Rcout << "e : " << e << "len : " << len << std::endl;
//     Vector<RTYPE> aug (new_len);
//     //Rf_PrintValue(wrap(aug));
//     for (int i=0; i<len; i++){
//       aug[i] = tab1[i];
//     }
    
//     // 4: need to reorder aug so that vn2-vars go first
//     chrVec d12  = setdiff(vn1, vn2);
//     chrVec vn   = do_concat_<chrVec>(vn2, d12);
//     intVec perm = match(vn, vn_aug);

//     int chk = sum(abs(perm - seq(1, vn_aug.size())));		
//     if (chk == 0){ // don't think this can happen!
//       //Rprintf("++ ++ no permutation needed; we are done\n");
//       aug.attr("dim")      = dim_aug;
//       aug.attr("dimnames") = dn_aug;
//       return aug;
//     } else {
//       //Rprintf("++ ++ permutation needed\n");
//       //numVec out = do_aperm_vec<REALSXP>(aug, dim_aug, perm);
//       // Vector<RTYPE> out = do_aperm_vec<RTYPE>(aug, dim_aug, perm);
//       // out.attr("dim")     = dim_aug[ perm - 1 ];
//       // out.attr("dimnames")= dn_aug [ perm - 1 ];      
//       // return out;
//       aug.attr("dim")      = dim_aug;
//       aug.attr("dimnames") = dn_aug;
//       return aug;

//     }
//   }

// }
