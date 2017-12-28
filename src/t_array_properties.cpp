#include <Rcpp.h>
using namespace Rcpp;
using namespace std;

// Check that 'obj' is a list (should be a dimnames check, but will do for now).
//[[Rcpp::export]]
bool is_dimnames_(const SEXP& obj){
  bool isOK = true;
  int type = TYPEOF(obj) ; 
  if (!(type == VECSXP))
    isOK = false;
  return isOK;
}

// Check that 'tab' is numeric or integer vector
//[[Rcpp::export]]
bool is_number_vector_(const SEXP& obj){
  bool isOK = true;
  int type = TYPEOF(obj) ; 
  if (!(type == INTSXP | type == REALSXP))
    isOK = false;
  return isOK;
}


// FIXME: Check that obj is an array (vector with dim and named dimnames)
//[[Rcpp::export]]
bool is_named_array_(const SEXP& obj){
  bool isOK = true;
  if ( !is_number_vector_(obj) ) return( false );
  
  RObject obj2 = as<RObject>(obj);
  SEXP di = obj2.attr("dim");
  SEXP dn = obj2.attr("dimnames");
  // bool has_di = !Rf_isNull( di ) && Rf_length( di ) > 1 ;
  // bool has_dn = !Rf_isNull( dn ) && Rf_length( dn ) > 1 ;
  bool has_di = !Rf_isNull( di ) ;
  bool has_dn = !Rf_isNull( dn ) ;
  
  // Rcout << "has di: " << has_di << " has dn: " << has_dn << "\n" ;
  if (! ( has_di & has_dn ) ) {
    // Rcout << "false 1...\n";
    isOK = false;			
  } else {
    List dn2 = as<List>( dn );
    SEXP vn = dn2.attr( "names" );
    // Rcout << vn ;
    //if ( !(!Rf_isNull( vn ) && Rf_length( vn ) > 1))
    if ( Rf_isNull( vn ) ){
      // Rcout << "false 2...\n";
      isOK = false;
    }
  }
  return isOK;
}






//[[Rcpp::export]]
bool dimnames_match_(const SEXP& tab1, const SEXP& tab2, bool verbose=false){

  if ( !(is_named_array_( tab1 )) ) stop("'tab1' is not a named array");
  if ( !(is_named_array_( tab2 )) ) stop("'tab2' is not a named array");
  
  RObject tab11 = as<RObject>(tab1);
  RObject tab22 = as<RObject>(tab2);
  
  IntegerVector   di1=tab11.attr("dim"),      di2=tab22.attr("dim");
  List            dn1=tab11.attr("dimnames"), dn2=tab22.attr("dimnames");
  CharacterVector vn1=dn1.names(),           vn2=dn2.names();
  
  bool allOK=true;
	
  CharacterVector vn=intersect(vn1, vn2);
  // Rcout << "Common names : " << vn << endl;
  
  if ( vn.size() > 0 ){
    for (int i=0; i<vn.size(); ++i){		
      CharacterVector lev1 = dn1[  (string)vn[i] ], lev2 = dn2[ (string)vn[i] ];	
      // Rcout << "lev1: " << lev1 << "\n"; //	print( lev1 );
      // Rcout << "lev2: " << lev2 << "\n"; //	print( lev2 );	
      
      bool res = lev1.length() == lev2.length();
      if ( !res ){
	allOK = false;
	if ( verbose ){
	  Rcout << "Levels not of same length for variable " << (string)vn[i] << endl;
	  Rcout << "lev1: ";	print( lev1 );
	  Rcout << "lev2: ";	print( lev2 );
	}
	break;
      } else {
	res = is_true( all( lev1 == lev2 ) );
	if ( !res ){
	  allOK = false;
	  if ( verbose ){
	    Rcout << "Levels do not match for variable " << (string)vn[i] << endl;
	    Rcout << "lev1: ";	print( lev1 );
	    Rcout << "lev2: ";	print( lev2 );
	  }
	  break;
	}
      }
    }
  }
  return allOK;
}




/*** R

hec = hec2 = hec3 = HairEyeColor

dimnames(hec2) <- NULL
names(dimnames(hec3)) <- NULL

is_named_array_(hec)
is_named_array_(hec2)
is_named_array_(hec3)

***/

/*** R

library(gRbase)
tab1 <- parray(c("a","b","c"), levels=c(2,2,2), values=1)
tab2 <- parray(c("b","a","q"), levels=c(2,2,5), values=1)
dimnames_match_(tab1, tab2) # TRUE

tab1 <- parray(c("a","b","c"), levels=c(2,2,2), values=1)
tab2 <- parray(c("b","a","q"), levels=c(5,2,2), values=1)
dimnames_match_(tab1, tab2) # FALSE

tab1 <- parray(c("a","b","c"), levels=c(2,2,2), values=1)
tab2 <- parray(c("b","a","q"), levels=c(2,2,5), values=1)
dimnames(tab2)[[1]]<-c("foo","bar")
dimnames_match_(tab1, tab2) # FALSE

tab1 <- parray(c("a","b","c"), levels=c(2,2,2), values=1)
tab2 <- parray(c("b","a","q"), levels=c(2,2,5), values=1)
dimnames(tab2)[[1]]<-c("b2","b1")
dimnames_match_(tab1, tab2) # FALSE

library(microbenchmark)

microbenchmark( is.named.array(tab1), is_named_array_(tab1))

tab1 <- parray(c("a","b","c"), levels=c(2,2,2), values=1)
tab2 <- parray(c("b","a","q"), levels=c(2,2,5), values=1)

microbenchmark( dimnames_match(tab1,tab2), dimnames_match_(tab1,tab2))



 ***/

// // Here 'permi' is an integer vector
// // // [[Rcpp::export]]
// SEXP apermi__(const SEXP& tab, const SEXP& permi){
//   int type = TYPEOF(tab) ; // Rprintf("type=%d\n", type);
//   switch( type ){
//   case INTSXP  : return do_apermi_tab<INTSXP> ( tab, permi ) ;
//   case REALSXP : return do_apermi_tab<REALSXP>( tab, permi ) ;
//   case STRSXP  : return do_apermi_tab<STRSXP> ( tab, permi ) ;
//   }
//   return R_NilValue ;
// }
 
	
