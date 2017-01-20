// ------------------------------------------------------------
// Rcpp functions for fast table operations.
// Author: Søren Højsgaard
// ------------------------------------------------------------

#include <Rcpp.h>
#include "concatenate.h"  // my version of c(x,y)
#include "_t_array_properties.h"  // my version of c(x,y)

using namespace Rcpp;
using namespace std;

//[[Rcpp::interfaces(r,cpp)]]

// 3/1/2015: A hack because setequal is broken
bool seteq_(CharacterVector x, CharacterVector y){
  return
	(((CharacterVector) setdiff(x,y)).length()==0) &
	(((CharacterVector) setdiff(y,x)).length()==0) ;
}

#define namesDimnames(tab) (((List) tab.attr("dimnames")).names())

// ------------------------------------------------------------
// Implementation of array permutation; very similar
// to R's implementation of aperm()
// Author: Søren Højsgaard
// ------------------------------------------------------------

inline IntegerVector make_prod(const int ndim, const IntegerVector& adim ){
  IntegerVector plevels = no_init( ndim );
  plevels[0] = 1;
  for (int i=1; i<ndim; ++i){
    plevels[i] =  adim[i-1] * plevels[i-1];
  }
  return plevels;
}

#define DO_CELL																	\
  cell_number= -offset;													\
  for (k=0; k<ndim; ++k){												\
    cell_number += pvec_perm[k] * cell[k] ;			\
  }																							\
  for (k=0; k<ndim; ++k){												\
    if (cell[k] == adim_new[k])									\
      cell[k] = 1;															\
    else{																				\
      ++cell[k];																\
      break;																		\
    }																						\
  };

// //[[Rcpp::export]]
bool is_valid_perm(const IntegerVector& adim, const IntegerVector& permi){
  bool out = false;
  if (adim.length() != permi.length()){
		Rcout << "'perm' is of wrong length" << std::endl;
  } else {
		IntegerVector perm2 = unique(permi);
		//Rf_PrintValue(perm2);
		if (any(is_na(perm2))){
			Rcout << "value out of range in 'perm'" << std::endl;
		} else {
			if (min(perm2)==1 && max(perm2)==adim.length()){
				out = true;
			} else {
				Rcout << "invalid permutation" << std::endl;
			}
		}
  }
  return out;
}

template <int RTYPE>
Vector<RTYPE> do_aperm_vec(const Vector<RTYPE>& tab,
				 const IntegerVector& adim,
				 const IntegerVector& permi){

  bool is_ok_perm = is_valid_perm(adim, permi);
  if (!is_ok_perm) 	stop("invalid permutation; can not proceed");

  int ncells = tab.length(), ndim = adim.length(), i, k, cell_number, n, offset=0;
  Vector<RTYPE> out  = no_init( ncells );
  IntegerVector cell = no_init( ndim );

  IntegerVector pvec  = make_prod( ndim, adim );
  IntegerVector pvec_perm =no_init( ndim ), perm0 = no_init( ndim ),
    adim_new=no_init( ndim );

  for (i=0; i<ndim; i++){
    cell[i]      = 1;
    n            = permi[i]-1;
    perm0[i]     = n;
    pvec_perm[i] = pvec[n];
    adim_new[i]  = adim[n];
    offset += pvec_perm[i];
  }

  for (i=0; i<ncells; ++i){
    DO_CELL;
    out[i] = tab[cell_number];
  }
  return out ;
}


template <int RTYPE>
Vector<RTYPE> do_apermi_tab(const Vector<RTYPE>& tab, const IntegerVector& permi){
  List            dn1 = tab.attr("dimnames");
  IntegerVector   di1 = tab.attr("dim");
  Vector<RTYPE>   out = do_aperm_vec<RTYPE>(tab, di1, permi);
  out.attr("dim")     = di1[ permi - 1 ];
  out.attr("dimnames")= dn1[ permi - 1 ];
  return out;
}

template <int RTYPE>
Vector<RTYPE> do_apermc_tab(const Vector<RTYPE>& tab, const CharacterVector& permc){
  CharacterVector vn1 = namesDimnames( tab );
  IntegerVector permi = match(permc, vn1);
  return do_apermi_tab<RTYPE>(tab, permi);
}


// THE R INTERFACE:

// Here 'permi' is an integer vector
// // [[Rcpp::export]]
SEXP apermi__(const SEXP& tab, const SEXP& permi){
  int type = TYPEOF(tab) ; // Rprintf("type=%d\n", type);
  switch( type ){
  case INTSXP  : return do_apermi_tab<INTSXP> ( tab, permi ) ;
  case REALSXP : return do_apermi_tab<REALSXP>( tab, permi ) ;
  case STRSXP  : return do_apermi_tab<STRSXP> ( tab, permi ) ;
  }
  return R_NilValue ;
}

// Here 'permc' is a character vector
// // [[Rcpp::export]]
SEXP apermc__(const SEXP& tab, const SEXP& permc){
  int type = TYPEOF(tab) ; // Rprintf("type=%d\n"2, type);
  switch( type ){
  case INTSXP  : return do_apermc_tab<INTSXP> ( tab, permc ) ;
  case REALSXP : return do_apermc_tab<REALSXP>( tab, permc ) ;
  case STRSXP  : return do_apermc_tab<STRSXP> ( tab, permc ) ;
  }
  return R_NilValue ;
}

// [[Rcpp::export]]
SEXP aperm__(const SEXP& tab, const SEXP& perm){
  int permtype = TYPEOF(perm) ; // Rprintf("type=%d\n"2, type);
  switch( permtype ){
  case INTSXP  : return apermi__( tab, perm ) ;
  case REALSXP : return apermi__( tab, perm ) ;
  case STRSXP  : return apermc__( tab, perm ) ;
  }
  return R_NilValue ;
}


// PERHAPS SIMPLER USING A MACRO

#define DISPATCH2(type, fun, arg1, arg2)							\
	switch( type ){																			\
	case INTSXP  : return fun<INTSXP>(arg1, arg2);			\
	case REALSXP : return fun<REALSXP>(arg1, arg2);			\
	case STRSXP :  return fun<STRSXP>(arg1, arg2);			\
	}																										\

// 'permi' is an integer vector
// // [[Rcpp::export]]
SEXP tabPermi__(const SEXP& tab, const SEXP& permi){
  int type = TYPEOF(tab) ; // Rprintf("type=%d\n", type);
	DISPATCH2(type, do_apermi_tab, tab, permi);	
  return R_NilValue ;
}

// 'permc' is a character vector
// // [[Rcpp::export]]
SEXP tabPermc__(const SEXP& tab, const SEXP& permc){
  int type = TYPEOF(tab) ; // Rprintf("type=%d\n"2, type);
	DISPATCH2(type, do_apermc_tab, tab, permc);	
  return R_NilValue ;
}

// [[Rcpp::export]]
SEXP tabPerm__(const SEXP& tab, const SEXP& perm){
  //int permtype = TYPEOF(perm) ; // Rprintf("type=%d\n"2, type);
  switch( TYPEOF(perm) ){
  case INTSXP  : 
  case REALSXP : return tabPermi__( tab, perm ) ;
  case STRSXP  : return tabPermc__( tab, perm ) ;
  }
  return R_NilValue ;
}






// -------------------------------------------------------------------
// tabExpand__
//
// tabExpand__(tab1, tab2) expands tab1 to also have the variables in tab2 in
// its list of variables.
//
// To be specific, let tables tab1, tab2 have variables v1, v2.
//
// tabExpand__ expands tab1 to a table, say tab1new, with variables {v2,
// v1\v2} (such that the variables v2 vary fastests). The elementwise
// sum, product etc of tab1new and tab2 then has the expected meaning.
//
// The code goes as follows:
//
// (a) If v2 is contained in v1 then check if v2-variables are first
// in v1. If so, just return a copy of tab1; otherwise permute tab1 so
// that v2-variables come first, and return this permuted table
//
// (b) If v2 is NOT contained in v1 then form table with variables
// {v1, v2\v1}. Then permute this table so that the v2-variable go
// first.
// -------------------------------------------------------------------


NumericVector tabExpandOLD__(const NumericVector& tab1, const NumericVector& tab2){
  IntegerVector   di1=tab1.attr("dim"),      di2=tab2.attr("dim");
  List            dn1=tab1.attr("dimnames"), dn2=tab2.attr("dimnames");
  CharacterVector vn1=dn1.names(),           vn2=dn2.names();

  //  d21 = v2 \ v1
  CharacterVector d21  = setdiff( vn2, vn1 );

  if ( d21.size()==0 ){ 	// v2 is subset of v1
    // Rprintf("++ no augmentation needed\n");
		// create variable vector vn = { vn2 , vn1\vn2 }
    CharacterVector d12 = setdiff( vn1, vn2 );
    CharacterVector vn  = do_concat_<CharacterVector>( vn2, d12 );
    IntegerVector  perm = match(vn, vn1);
    // Rprintf("vn="); Rf_PrintValue(vn); Rprintf("perm: "); Rf_PrintValue(perm);
		// Check if permutation is needed
    int chk = sum( abs( perm - seq(1, vn1.size()) ) );
    if (chk==0){
      //Rprintf("++ ++ no permutation needed; we are done\n");
      return clone(tab1);
    } else {
      //Rprintf("++ ++ permutation needed\n");
      //Rf_PrintValue(di1); Rf_PrintValue(perm);
      NumericVector out   = do_aperm_vec<REALSXP>(tab1, di1, perm);
      out.attr("dim")     = di1[ perm - 1 ];
      out.attr("dimnames")= dn1[ perm - 1 ];
      return out;
    }
  } else {		// v2 is NOT subset of v1
    // Rprintf("++ augmentation of table needed\n");
		// Create table with variables {v1, v2\v1}
    // need to know where the d21's are to pick out dims and dimensions
    IntegerVector d21_idx  = match( d21, vn2 );
    List          d21_dn   = dn2[ d21_idx-1 ];
    IntegerVector d21_di   = di2[ d21_idx-1 ];
    // find product of added dimensions
    int e=1;
    for (int i=0; i<d21_di.length(); ++i){ e *= d21_di[i];}
		
    // vars, dimnames etc in {v1, v2\v1}-table
    CharacterVector vn_aug  = do_concat_<CharacterVector>( vn1, d21 );
    List            dn_aug  = do_concat_<List>( dn1, d21_dn );
    IntegerVector   dim_aug = do_concat_<IntegerVector>(di1, d21_di);
    NumericVector   aug     = rep(tab1, e);

    // need to reorder aug so that vn2-vars go first
    CharacterVector d12  = setdiff(vn1, vn2);
    CharacterVector vn   = do_concat_<CharacterVector>(vn2, d12);
    IntegerVector   perm = match(vn, vn_aug);
    int chk = sum( abs( perm - seq(1, vn_aug.size()) ) );
		
    if (chk==0){ // don't think this can happen!
      //Rprintf("++ ++ no permutation needed; we are done\n");
      aug.attr("dim")      = dim_aug;
      aug.attr("dimnames") = dn_aug;
      return aug;
    } else {
      //Rprintf("++ ++ permutation needed\n");
      NumericVector out = do_aperm_vec<REALSXP>(aug, dim_aug, perm);
      out.attr("dim")     = dim_aug[ perm - 1 ];
      out.attr("dimnames")= dn_aug [ perm - 1 ];
      return out;
    }
  }
}






NumericVector tabExpand_internal__(const NumericVector& tab1, const List& dn2){
  List            dn1=tab1.attr("dimnames");
	IntegerVector   di1=sapply(dn1, Rf_length), di2=sapply(dn2, Rf_length);
  CharacterVector vn1=dn1.names(),            vn2=dn2.names();
	//print(di1); print(di2); print(vn1); print(vn2);
	
  //  d21 = v2 \ v1
  CharacterVector d21  = setdiff( vn2, vn1 );

  if ( d21.size()==0 ){ 	// v2 is subset of v1
    // Rprintf("++ no augmentation needed\n");
		// create variable vector vn = { vn2 , vn1\vn2 }
    CharacterVector d12 = setdiff( vn1, vn2 );
    CharacterVector vn  = do_concat_<CharacterVector>( vn2, d12 );
    IntegerVector  perm = match(vn, vn1);
    // Rprintf("vn="); Rf_PrintValue(vn); Rprintf("perm: "); Rf_PrintValue(perm);
		// Check if permutation is needed
    int chk = sum( abs( perm - seq(1, vn1.size()) ) );
    if (chk==0){
      //Rprintf("++ ++ no permutation needed; we are done\n");
      return clone(tab1);
    } else {
      //Rprintf("++ ++ permutation needed\n");
      //Rf_PrintValue(di1); Rf_PrintValue(perm);
      NumericVector out   = do_aperm_vec<REALSXP>(tab1, di1, perm);
      out.attr("dim")     = di1[ perm - 1 ];
      out.attr("dimnames")= dn1[ perm - 1 ];
      return out;
    }
  } else {		// v2 is NOT subset of v1
    // Rprintf("++ augmentation of table needed\n");
		// Create table with variables {v1, v2\v1}
    // need to know where the d21's are to pick out dims and dimensions
    IntegerVector d21_idx  = match( d21, vn2 );
    List          d21_dn   = dn2[ d21_idx-1 ];
    IntegerVector d21_di   = di2[ d21_idx-1 ];
    // find product of added dimensions
    int e=1;
    for (int i=0; i<d21_di.length(); ++i){ e *= d21_di[i];}
		
    // vars, dimnames etc in {v1, v2\v1}-table
    CharacterVector vn_aug  = do_concat_<CharacterVector>( vn1, d21 );
    List            dn_aug  = do_concat_<List>( dn1, d21_dn );
    IntegerVector   dim_aug = do_concat_<IntegerVector>(di1, d21_di);
    NumericVector   aug     = rep(tab1, e);

    // need to reorder aug so that vn2-vars go first
    CharacterVector d12  = setdiff(vn1, vn2);
    CharacterVector vn   = do_concat_<CharacterVector>(vn2, d12);
    IntegerVector   perm = match(vn, vn_aug);
    int chk = sum( abs( perm - seq(1, vn_aug.size()) ) );
		
    if (chk==0){ // don't think this can happen!
      //Rprintf("++ ++ no permutation needed; we are done\n");
      aug.attr("dim")      = dim_aug;
      aug.attr("dimnames") = dn_aug;
      return aug;
    } else {
      //Rprintf("++ ++ permutation needed\n");
      NumericVector out = do_aperm_vec<REALSXP>(aug, dim_aug, perm);
      out.attr("dim")     = dim_aug[ perm - 1 ];
      out.attr("dimnames")= dn_aug [ perm - 1 ];
      return out;
    }
  }
}



// [[Rcpp::export]]
NumericVector tabExpand__(const NumericVector& tab1, const SEXP& tab2){
	// bool ina=is_named_array_(tab2);
	// Rcout << ina << std::endl;

	if (is_dimnames_(tab2)){
		List dn = (List) tab2;
		NumericVector out = tabExpand_internal__(tab1, dn);
		return out;
	} else if (is_named_array_(tab2)){
			List dn = ((NumericVector)tab2).attr("dimnames");
			NumericVector out = tabExpand_internal__(tab1, dn);				
			return out;
	} else {
		::Rf_error("dont know what to do");		
	}
}




// -----------------------------------------------------------------
// tabAlign:
//
// Align tab1 with tab2 if possible
//
// If tab1 and tab2 have the same variable names then tab1 is permuted so
// that the variables in tab1 is in the same order as the variables in
// tab2.
// -----------------------------------------------------------------

//[[Rcpp::export]]
NumericVector tabAlign__(const NumericVector& tab1, const NumericVector& tab2){

  // List            dn1=tab1.attr("dimnames"), dn2=tab2.attr("dimnames");
  // CharacterVector vn1=dn1.names(),         vn2=dn2.names();
  CharacterVector vn1 = namesDimnames(tab1), vn2 = namesDimnames(tab2);

  if (seteq_( vn1, vn2 )){
    NumericVector out = tabExpand__( tab1, tab2 );
    return out;
  } else {
    return NumericVector(0) ;
  }
}


// -------------------------------------------------------------------
// tabMarg__
//
// Find marginal tables
// -------------------------------------------------------------------

template <int RTYPE>
Vector<RTYPE> do_margc_tab(const Vector<RTYPE>& tab1, const CharacterVector& margc){
  List            dn1 = tab1.attr("dimnames");
  CharacterVector vn1 = dn1.names();
  IntegerVector   di1 = tab1.attr("dim");

  IntegerVector marg_idx = match(margc, vn1);
  if (any (is_na( marg_idx ) ))
    stop("Invalid margc specification\n");

  CharacterVector rest   = setdiff(vn1, margc);
  CharacterVector vn_new = do_concat_<Vector<STRSXP> >(rest, margc);
  IntegerVector perm     = match( vn_new, vn1 );
  Vector<RTYPE> out      = do_aperm_vec<RTYPE>(tab1, di1, perm);

  if (rest.length()==0){    //Rprintf("no marginalization needed\n");
    out.attr("dim")      = di1[ perm - 1 ];
    out.attr("dimnames") = dn1[ perm - 1];
    return out;
  } else {                  //Rprintf("marginalization needed\n");
    int ncells=1, margcells=1;
    for (int i=0; i<di1.length(); ++i)
      ncells *= di1[i];
    for (int i=0; i<marg_idx.length(); ++i)
      margcells *= di1[marg_idx[i]-1];
    int restcells = ncells / margcells;

    Vector<RTYPE> ret = no_init( margcells );
    typedef typename traits::storage_type<RTYPE>::type storage_t;
    storage_t sum;
    int offset;
    for (int j=0; j<margcells; ++j){
      sum    = 0;
      offset = restcells * j;
      for (int i=0; i<restcells; ++i)
		sum += out[i+offset];
      ret[j] = sum;
    }
    ret.attr("dim")      = di1[ marg_idx - 1 ];
    ret.attr("dimnames") = dn1[ marg_idx - 1];
    return ret;
  }
}

template <int RTYPE>
Vector<RTYPE> do_margi_tab(const Vector<RTYPE>& tab1, const IntegerVector& margi){
  List            dn1=tab1.attr("dimnames");
  IntegerVector   di1=tab1.attr("dim");
  CharacterVector vn1=dn1.names();
  CharacterVector margc=vn1[(IntegerVector) margi-1];
  return do_margc_tab<RTYPE>(tab1, margc);
}

// THE R INTERFACE:

// // [[Rcpp::export]]
SEXP tabMargc__(const SEXP& tab1, const SEXP& margc){
  int type = TYPEOF(tab1) ;
  switch( type ){
  case INTSXP  : return do_margc_tab<INTSXP> ( tab1, margc ) ;
  case REALSXP : return do_margc_tab<REALSXP>( tab1, margc ) ;
  }
  return R_NilValue ;
}

// // [[Rcpp::export]]
SEXP tabMargi__(const SEXP& tab1, const SEXP& margi){
  int type = TYPEOF(tab1) ;
  switch( type ){
  case INTSXP  : return do_margi_tab<INTSXP> ( tab1, margi ) ;
  case REALSXP : return do_margi_tab<REALSXP>( tab1, margi ) ;
  }
  return R_NilValue ;
}

// [[Rcpp::export]]
SEXP tabMarg__(const SEXP& tab1, const SEXP& marg){
  int ptype = TYPEOF(marg) ; // Rprintf("type=%d\n"2, type);
  switch( ptype ){
  case INTSXP  :
  case REALSXP : return tabMargi__( tab1, marg ) ;
  case STRSXP  : return tabMargc__( tab1, marg ) ;
  }
  return R_NilValue ;
}

// -------------------------------------------------------------------
// Algebraic operations on tables: +, -, *, /, ==
// Check if two tables are (numerically) equal
// -------------------------------------------------------------------

//[[Rcpp::export]]
NumericVector tabMult__(NumericVector tab1, NumericVector tab2){
	if (!dimnames_match_( tab1, tab2)) ::Rf_error("dimnames do not match");

  NumericVector out = tabExpand__(tab1, tab2);
  //Rf_PrintValue(out);
  int n1 = out.size(), n2=tab2.size(), n3=n1/n2, zz;
  for (int i=0; i<n2; ++i){
    for (int k=0; k<n3; ++k){
      zz = i+k*n2;
      //cout << "i=" << i << " k=" << k << " zz=" << zz << endl;
      out[zz] = tab2[i] * out[zz] ;
    }
  }
  return out;
}

//[[Rcpp::export]]
NumericVector tabDiv__(NumericVector tab1, NumericVector tab2){
	if (!dimnames_match_( tab1, tab2)) ::Rf_error("dimnames do not match");
  NumericVector out = tabExpand__(tab1, tab2);
  int n1 = out.size(), n2=tab2.size(), n3=n1/n2, zz;
  for (int i=0; i<n2; ++i){
    for (int k=0; k<n3; ++k){
      zz = i+k*n2;
      //cout << "i=" << i << " k=" << k << " zz=" << zz << endl;
      out[zz] = out[zz] / tab2[i] ;
    }
  }
  return out;
}


//[[Rcpp::export]]
NumericVector tabDiv0__(NumericVector tab1, NumericVector tab2){
	//Rcout << "tabDiv0__\n";
	if (!dimnames_match_( tab1, tab2)) ::Rf_error("dimnames do not match");
  NumericVector out = tabExpand__(tab1, tab2);
  double xx;
  int n1 = out.size(), n2=tab2.size(), n3=n1/n2, zz;
  for (int i=0; i<n2; ++i){
    for (int k=0; k<n3; ++k){
      zz = i+k*n2;
      //cout << "i=" << i << " k=" << k << " zz=" << zz << endl;
      xx = out[zz] / tab2[i] ;
			//Rcout << "xx: " << xx << std::endl;
      if (std::isinf( xx ) || NumericVector::is_na(xx)) out[zz] = 0;  else out[zz]=xx;
    }
  }
  return out;
}

//[[Rcpp::export]]
NumericVector tabAdd__(NumericVector tab1, NumericVector tab2){
	if (!dimnames_match_( tab1, tab2)) ::Rf_error("dimnames do not match");	
  NumericVector out = tabExpand__(tab1, tab2);
  int n1 = out.size(), n2=tab2.size(), n3=n1/n2, zz;
  for (int i=0; i<n2; ++i){
    for (int k=0; k<n3; ++k){
      zz = i+k*n2;
      //cout << "i=" << i << " k=" << k << " zz=" << zz << endl;
      out[zz] = tab2[i] + out[zz] ;
    }
  }
  return out;
}

//[[Rcpp::export]]
NumericVector tabSubt__(NumericVector tab1, NumericVector tab2){
	if (!dimnames_match_( tab1, tab2)) ::Rf_error("dimnames do not match");	
  NumericVector out = tabExpand__(tab1, tab2);  //Rf_PrintValue( out );
  int n1 = out.size(), n2=tab2.size(), n3=n1/n2, zz;
  for (int i=0; i<n2; ++i){
    for (int k=0; k<n3; ++k){
      zz = i+k*n2;
      //cout << "i=" << i << " k=" << k << " zz=" << zz << endl;
      out[zz] = out[zz] - tab2[i];
    }
  }
  return out;
}


//[[Rcpp::export]]
NumericVector tabOp__(const NumericVector& tab1, const NumericVector& tab2, const char op='*'){
	if (!dimnames_match_( tab1, tab2)) ::Rf_error("dimnames do not match");
  NumericVector out = tabExpand__(tab1, tab2);
  int n1 = out.size(), n2=tab2.size(), n3=n1/n2, zz;
  for (int i=0; i<n2; ++i){
    for (int k=0; k<n3; ++k){
      zz = i+k*n2;
      //cout << "i=" << i << " k=" << k << " zz=" << zz << endl;
      switch( op ){
      case '/' : { out[zz] = out[zz] / tab2[i] ; break; }
      case '*' : { out[zz] = out[zz] * tab2[i] ; break; }
      case '+' : { out[zz] = out[zz] + tab2[i] ; break; }
      case '-' : { out[zz] = out[zz] - tab2[i] ; break; }
      default : stop("'op' is an undefined operation");
      }
    }
  }
  return out;
}



//[[Rcpp::export]]
bool tabEqual__(NumericVector tab1, NumericVector tab2, double eps=1e-12){

  // List            dn1=tab1.attr("dimnames"), dn2=tab2.attr("dimnames");
  // CharacterVector vn1=dn1.names(),         vn2=dn2.names();
  CharacterVector vn1 = namesDimnames(tab1), vn2 = namesDimnames(tab2);

  if (seteq_( vn1, vn2 )){
    NumericVector dif = tabSubt__( tabExpand__( tab1, tab2 ), tab2 );
    return (sum( abs( dif ) ) < eps);
  } else {
	return false;
  }
}

// -----------------------------------------------------------
// Multiply and add lists of tables
// -----------------------------------------------------------

//[[Rcpp::export]]
NumericVector tabListMult__(const List & lst){
  int len_lst = lst.length();
  if (len_lst==0){
	return NumericVector(0);
  } else {
	NumericVector out = lst[0];
	for (int i=1; i<len_lst; ++i){
	  out = tabMult__(out, lst[i]);
	}
	return out;
  }
}

//[[Rcpp::export]]
NumericVector tabListAdd__(const List & lst){
  int len_lst = lst.length();
  if (len_lst==0){
	return NumericVector(0);
  } else {
	NumericVector out = lst[0];
	for (int i=1; i<len_lst; ++i){
	  out = tabAdd__(out, lst[i]);
	}
	return out;
  }
}












// /*** R
// library(gRbase)
// t1<- array(1:4, dim=c(2,2), dimnames=list(A=c(1,2), B=c(1,2)))
// t2<- array(11:14, dim=c(2,2), dimnames=list(C=c("c1","c2"), B=c(1,2)))
// t3<- array(1:8, dim=c(2,2,2), dimnames=list(A=c(1,2), B=c(1,2), D=1:2))


// tabExpand(t1, t2)

// tabExpand(t1, t1)
// tabExpand(t1, aperm(t1, 2:1))

// tabExpand(t3, t1)
// tabExpand(aperm(t3,3:1), t1)


// #t1mod <- tabExpand(t1, t2); t1mod
// r1 <- tabMult( t1, t2 )
// r2 <- tableOp( t1, t2 )
// #rr <- alignArrays(r1, r2)
// #max(abs(rr[[1]]-rr[[2]]))

// r1 <- tabMult( t1, t1 )
// r2 <- tableOp( t1, t1 )
// #rr <- alignArrays(r1, r2)
// #max(abs(rr[[1]]-rr[[2]]))

// r1 <- tabMult( t1, aperm(t1,2:1) )
// r2 <- tableOp( t1, aperm(t1,2:1) )
// #rr <- alignArrays(r1, r2)
// #max(abs(rr[[1]]-rr[[2]]))

// r1 <- tabMult( t3, t1 )
// r2 <- tableOp( t3, t1 )
// #rr <- alignArrays(r1, r2)
// #max(abs(rr[[1]]-rr[[2]]))


// r1 <- tabMult( t3, aperm(t1,2:1) )
// r2 <- tableOp( t3, aperm(t1,2:1) )
// #rr <- alignArrays(r1, r2)
// #max(abs(rr[[1]]-rr[[2]]))


// library(microbenchmark)
// microbenchmark(tableOp(t1,t2), tabMult(t1,t2))

// microbenchmark(tableOp(t3,t2), tabMult(t3,t2))


// microbenchmark(tableOp(t3,t1), tabMult(t3,t1))
// microbenchmark(tableOp(t1,t3), tabMult(t1,t3))





//  */



// // THIS WORKS BUT IT IS UGLY
// template <int RTYPE>
// Vector<RTYPE> do_apermi_tab(const Vector<RTYPE>& tab, const IntegerVector& permi){
//   List            dn1=tab.attr("dimnames");
//   IntegerVector   di1=tab.attr("dim");
//   Vector<RTYPE> out = do_aperm_vec<RTYPE>(tab, di1, permi);
//   out.attr("dim")     = di1[ permi - 1 ];
//   out.attr("dimnames")= dn1[ permi - 1 ];
//   return out;
// }

// template <int RTYPE>
// Vector<RTYPE> do_apermc_tab(const Vector<RTYPE>& tab, const CharacterVector& permc){
//   List            dn1=tab.attr("dimnames");
//   CharacterVector vn1=dn1.names();
//   return do_apermi_tab<RTYPE>(tab, match(permc, vn1));
// }


// // PERHAPS A BIT MORE ELEGANT



