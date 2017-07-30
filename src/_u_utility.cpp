/*
	Some set operations for gRbase and related packages

	Author: Søren Højsgaard

 */

#include <Rcpp.h>
//[[Rcpp::interfaces(r,cpp)]]
//[[Rcpp::depends(RcppEigen)]]

using namespace Rcpp;


IntegerVector get_superset_one_(CharacterVector x, List setlist){
  bool outb=false;
  //IntegerVector val= IntegerVector::create( NA_INTEGER );
	int val=-1, k=0;

  for (int i=0; i<setlist.length(); ++i){
		CharacterVector set=setlist[i];
		outb = (any(is_na(match(x, set))));
		outb = ! outb;
		if (outb){
			val=i+1;
			k=1;
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

  for (int i=0; i<setlist.length(); ++i){
		CharacterVector set=setlist[i];
		bool out = (any(is_na(match(x, set))));
		out = ! out;
		if (out)
			vec[k++] = i+1;
  }

	IntegerVector out = IntegerVector(k);
	if (k>0){
		for (int i=0; i<k; ++i)	out[i]=vec[i];
	}

	return out;
}










IntegerVector get_subset_one_(CharacterVector x, List setlist){
  bool outb=false;
  //IntegerVector val= IntegerVector::create( NA_INTEGER );
	int val=-1, k=0;

  for (int i=0; i<setlist.length(); ++i){
		CharacterVector set=setlist[i];
		outb = (any(is_na(match(set, x))));
		outb = ! outb;
		if (outb){
			val=i+1;
			k=1;
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

  for (int i=0; i<setlist.length(); ++i){
		CharacterVector set=setlist[i];
		bool out = (any(is_na(match(set, x))));
		out = ! out;
		if (out)
			vec[k++] = i+1;
  }

	IntegerVector out = IntegerVector(k);
	if (k>0){
		for (int i=0; i<k; ++i)	out[i]=vec[i];
	}

	return out;
}

//' @name internal
//' @aliases is_subsetof__ get_superset__ get_subset__

//[[Rcpp::export]]
bool is_subsetof__(CharacterVector set, CharacterVector set2){
  if (set.length() > set2.length())
    return false;
  else {
    IntegerVector m = match(set, set2);
    //Rf_PrintValue(m);
    bool out = any(is_na(m));
    return !out;
  }
}

//[[Rcpp::export]]
IntegerVector get_superset__(CharacterVector set, List setlist, bool all=false){
  if (all)
    return get_superset_all_(set, setlist);
  else
    return get_superset_one_(set, setlist);
}

//[[Rcpp::export]]
IntegerVector get_subset__(CharacterVector set, List setlist, bool all=false){
  if (all)
    return get_subset_all_(set, setlist);
  else
    return get_subset_one_(set, setlist);
}


// FIXME: ALIASES for gRain compatibility, June 2017: get_superset_; get_subset_; is_subset_of_

//[[Rcpp::export]]
IntegerVector get_superset_(CharacterVector set, List setlist, bool all=false){
  return get_superset__(set, setlist, all);
}

//[[Rcpp::export]]
IntegerVector get_subset_(CharacterVector set, List setlist, bool all=false){
  return get_subset__(set, setlist, all);
}

//[[Rcpp::export]]
bool is_subsetof_(CharacterVector set, CharacterVector set2){
  return is_subsetof__(set, set2);
}


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
get_subset_(c("a","b", "k"), setlist, all=F)

get_subset_(c("a","b", "k"), setlist, all=T)

get_subset_(x1, setlist, all=T)
get_subset_(c("a","r"), setlist, all=T)


*/



















// //[[Rcpp::export]]
// IntegerVector get_host_(CharacterVector x, List setlist){
//   bool out=false;
//   IntegerVector val= IntegerVector::create( NA_INTEGER );

//   for (int i=0; i<setlist.length(); ++i){
// 		CharacterVector set=setlist[i];
// 		out = (any(is_na(match(x, set))));
// 		out = ! out;
// 		if (out){
// 			val[0]=i+1;
// 			break;
// 		}
//   }
//   return val;
// }

// //[[Rcpp::export]]
// bool isin_(List setlist, CharacterVector x){
//   bool out=false;
//   for (int i=0; i<setlist.length(); ++i){
// 		CharacterVector set=setlist[i];
// 		out = (any(is_na(match(x, set))));
// 		out = ! out;
// 		if (out)
// 			break;
//   }
//   return out;
// }

// //[[Rcpp::export]]
// IntegerVector isin2_(List setlist, CharacterVector x){
//   IntegerVector vec(setlist.length());

//   for (int i=0; i<setlist.length(); ++i){
// 		CharacterVector set=setlist[i];
// 		bool out = (any(is_na(match(x, set))));
// 		out = ! out;
// 		if (out)
// 			vec[i] = 1;
//   }
//   return vec;
// }



// library(gRbase)
// library(microbenchmark)
// microbenchmark(
// is_subsetof(x, set),
// is_subsetof(x2, set),
// is.subsetof(x, set),
// is.subsetof(x2, set),
// is_subsetof(set, x),
// is_subsetof(set, x2),
// is.subsetof(set, x),
// is.subsetof(set, x2)
// )


// setlist <- list(letters[1:3], letters[3:6], letters[1:2])
// isin(setlist, x)
// isin(setlist, x, index=T)


// isinR <- function(setlist, x){
// unlist(lapply(setlist, function(set) any(is.na(match(x,set)))))
// }


// isin(setlist, x)
// isin(setlist, x, index=T)
// isinR(setlist, x)
// isin_(setlist, x)
// isin2_(setlist, x)

// microbenchmark(
// isin(setlist, x),
// isinR(setlist, x),
// isin_(setlist, x),
// isin2_(setlist, x)
// )

