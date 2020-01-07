/* ****************************************************

  Set operations for gRbase and related packages
  
  Author: Søren Højsgaard

  ***************************************************** */

#include <Rcpp.h>
//[[Rcpp::interfaces(r,cpp)]]
//[[Rcpp::depends(RcppEigen)]]

using namespace Rcpp;

/* *************************************************

   Implements:
   
   get_superset, get_subset, is_subsetof
   
   ************************************************* */

// //' @name internal_grbase_cpp
// //' @aliases is_subsetof__ get_superset__ get_subset__

IntegerVector get_superset_one_(CharacterVector x, List setlist){
  bool outb=false;
  int val=-1, k=0;
  
  for (int i=0; i<setlist.length(); ++i)
    {
      CharacterVector set=setlist[i];
      outb = (any(is_na(match(x, set))));
      outb = ! outb;
      if (outb){
	val = i+1;
	k   = 1;
	break;
      }
    }
  IntegerVector out = IntegerVector(k);
  out[0] = val;
  return out;
}

IntegerVector get_superset_all_(CharacterVector x, List setlist){
  IntegerVector vec(setlist.length());
  int k=0;
  
  for (int i=0; i<setlist.length(); ++i)
    {
      CharacterVector set=setlist[i];
      bool out = (any(is_na(match(x, set))));
      out = ! out;
      if (out) vec[k++] = i+1;
    }
  
  IntegerVector out = IntegerVector(k);
  if (k>0){
    for (int i=0; i<k; ++i)	out[i]=vec[i];
  }
  
  return out;
}

IntegerVector get_subset_one_(CharacterVector x, List setlist)
{
  bool outb=false;
  int val=-1, k=0;
  
  for (int i=0; i<setlist.length(); ++i)
    {
      CharacterVector set=setlist[i];
      outb = (any(is_na(match(set, x))));
      outb = ! outb;
      if (outb){
	val = i+1;
	k   = 1;
	break;
      }
    }
  IntegerVector out = IntegerVector(k);
  out[0] = val;
  return out;
}

IntegerVector get_subset_all_(CharacterVector x, List setlist){
  IntegerVector vec(setlist.length());
  int k=0;
  
  for (int i=0; i<setlist.length(); ++i)
    {
      CharacterVector set=setlist[i];
      bool out = (any(is_na(match(set, x))));
      out = ! out;
      if (out)
	vec[k++] = i+1;
    }
  
  IntegerVector out = IntegerVector(k);
  if (k > 0){
    for (int i=0; i<k; ++i) out[i]=vec[i];
  }
  return out;
}

// get_superset_ is used in gRain

//[[Rcpp::export]]
IntegerVector get_superset_(CharacterVector set, List setlist, bool all=false)
{
  if (all) return get_superset_all_(set, setlist);
  else return get_superset_one_(set, setlist);
}

//[[Rcpp::export]]
IntegerVector get_subset_(CharacterVector set, List setlist, bool all=false)
{
  if (all) return get_subset_all_(set, setlist);
  else return get_subset_one_(set, setlist);
}


// is_subsetof_ is used in gRain

//[[Rcpp::export]]
bool is_subsetof_(CharacterVector set, CharacterVector set2)
{
  if (set.length() > set2.length())
    return false;
  else {
    IntegerVector m = match(set, set2);
    //Rf_PrintValue(m);
    bool out = any(is_na(m));
    return !out;
  }
}

/* **************************************************************

   Implements:

   allSubsets_cpp(x)  - 'any' vector x
   allSubsets0_cpp(x) - integer vector x
      
   *************************************************************** */

// Works for integer input vector
//[[Rcpp::export]]
List allSubsets0_(const IntegerVector& x)
{
  int nx = x.length(), nout=pow(2., nx), i, k, ny=1;
  double z;
  List out( nout );
  out[0] = -1;
  
  for (i=0; i<nx; ++i){
    z = x[i];
    for (k=0; k<ny; ++k){
      IntegerVector tmp = out[k];
      tmp.push_back( z );
      out[ny + k] = tmp;
    }
    ny = 2 * ny;
  }
  
  for (i=1; i<nout; ++i){
    IntegerVector aa=out[i];
    int M = aa.length();
    IntegerVector vv = no_init( M-1 );
    for (k=1; k<M; ++k){
      vv[k-1] = aa[k];
    }
    out[i-1] = vv;
  }
  out.erase(out.end() - 1, out.end());
  return out;
}

template <int RTYPE>
List do_allSubsets (Vector<RTYPE> vn)
{
  IntegerVector sq = seq_len( vn.size() );
  List lst = allSubsets0_( sq );
  int N=lst.size(), i;
  for (i=0; i<N; ++i){
    lst[i] = vn[ IntegerVector( lst[i] )-1 ];
  }
  return lst;
}

// Works for any type of input vector
// [[Rcpp::export]]
SEXP allSubsets_( SEXP& x){
  int type = TYPEOF(x) ; //Rprintf("type=%i\n", type);
  switch( type ){
  case INTSXP  : return allSubsets0_( x ) ;
  case REALSXP : return do_allSubsets<REALSXP>( x ) ;
  case STRSXP  : return do_allSubsets<STRSXP> ( x ) ;
  }
  return R_NilValue ;
}


/*** R


allSubsets(1:5)
allSubsets(letters[1:5])

library(microbenchmark)
x <- 1:4
microbenchmark(allSubsets0_R( x ), allSubsets1_R( x ), allSubsets(x))

x <- 1:10
microbenchmark(allSubsets0_R( x ), allSubsets1_R( x ), allSubsets(x))

x <- 1:15
microbenchmark(allSubsets0_R( x ), allSubsets(x),times=10)

*/



/* *************************************************

   Implements:

   all_pairs

   ************************************************* */

CharacterMatrix do_names2pairs_(CharacterVector x, CharacterVector y){
  int lenx = x.length(), leny = y.length();
  int i,j,k;
    
  if (leny == 0){
    if (lenx == 1){
      CharacterMatrix out(lenx * leny, 2);
      return out;
    } else {
      CharacterMatrix out(lenx * (lenx - 1) / 2, 2);
      k = 0;
      for (i=0; i<lenx; ++i){
	for (j=i+1; j<lenx; ++j){
	  out(k,   0) = x[i];
	  out(k++, 1) = x[j];
	}
      }
      return out;
    }
  } else {
    CharacterMatrix out(lenx*leny, 2);
    k = 0;
    for (i=0; i<lenx; ++i){
      for (j=0; j<leny; ++j){
	out(k,   0) = x[i];
	out(k++, 1) = y[j];
      }
    }
    return out;
  }
}

CharacterMatrix sortmat_(CharacterMatrix X){
  CharacterMatrix X2 = clone(X);
  CharacterVector tmp(1);
  for (int i=0; i < X2.rows(); ++i){
    if (X2(i, 0) > X2(i, 1)){
      tmp[0]   = X2(i, 0);
      X2(i, 0) = X2(i, 1);
      X2(i, 1) = tmp[0];
    }
  }
  return X2;
}

// //' @name internal_grbase_cpp
// //' @aliases all_pairs__

//[[Rcpp::export]]
SEXP all_pairs__(CharacterVector x,
		 CharacterVector y=CharacterVector(0),
		 bool sort=false,
		 std::string result="matrix"){
  CharacterMatrix out = do_names2pairs_(x, y);
  
  if (sort) out = sortmat_(out);
  
  if (result == "list"){
    List outlist(out.nrow());
    for (int i=0; i < out.nrow(); ++i)
      outlist[i] = out(i, _);
    return (outlist);
  } else { 
    return(out);
  } 
}


/*** R
library(microbenchmark)
x <- letters[4:1]
y <- letters[4:6]
library(gRbase)
names2pairs(x, y, result="matrix")
names2pairsM(x, y)

names2pairs("x", NULL, result="matrix")
names2pairsM("x", character(0))

names2pairs(x, NULL, result="matrix")
names2pairsM(x, character(0))

m<-matrix(c("a","b","b","a"), ncol=2, byrow=T)


names2pairs2 <- function(x, y=NULL, sort=FALSE, result="list"){
  if (is.null(y))
    y <- character(0)
  out <- names2pairsM(x, y, sort, result)
  out
}

sortmat <- function(out){
     idx <- out[, 1] > out[, 2]
     out[idx, 1:2] <- out[idx, 2:1]
out
}

microbenchmark(sortmat(m), sortmat_(m))

microbenchmark(
  names2pairs(x, y, sort=FALSE, result="matrix"),
  names2pairsM(x, y, sort=FALSE, result="matrix"),
  names2pairs(x, y, sort=TRUE, result="matrix"),
  names2pairsM(x, y, sort=TRUE, result="matrix"),
	names2pairs("x", sort=FALSE, result="matrix"),
	names2pairsM("x", sort=FALSE, result="matrix"),
	names2pairs("x", sort=TRUE, result="matrix"),
	names2pairsM("x", sort=TRUE, result="matrix"),
	names2pairs(x, sort=FALSE, result="matrix"),
	names2pairsM(x, sort=FALSE, result="matrix"),
	names2pairs(x, sort=TRUE, result="matrix"),
	names2pairsM(x, sort=TRUE, result="matrix")
	)

*/







/*** R
x1 <- c("b","a")
x2 <- c("a","k")
set <- letters[1:4]
setlist <- list(x1, x2, c("a","b","k"))
str(setlist)

is_subsetof_(x1, set)
is_subsetof_(x2, set)

is_subsetof_(set, x1)
is_subsetof_(set, x2)

get_superset_(x1, setlist)
get_superset_(c("a","r"), setlist)

get_superset_(x1, setlist, all=T)
get_superset_(c("a","r"), setlist, all=T)

setlist <- list(x1, x2, c("a","b","k"), c("a","r","k"))
str(setlist)

get_subset_(x1, setlist)
get_subset_(c("a", "b", "k"), setlist, all=F)
get_subset_(c("a", "b", "k"), setlist, all=T)
get_subset_(x1, setlist, all=T)
get_subset_(c("a","r"), setlist, all=T)

*/

















































