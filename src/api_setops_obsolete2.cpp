#include <Rcpp.h>

using namespace Rcpp;

// ----------------------------------------
//
// ### Implementations on their way out ###
//
// ----------------------------------------



//[[Rcpp::export]]
bool is_subsetof_old(CharacterVector set, CharacterVector set2)
{
  if (set.length() > set2.length()) return false;
  else {
    IntegerVector m = match(set, set2);
    //Rf_PrintValue(m);
    bool out = any(is_na(m));
    return !out;
  }
}



IntegerVector get_subset_one_(CharacterVector x, List setlist)
{
  bool outb=false;
  int val=-1, k=0;
  
  for (int i=0; i<setlist.length(); ++i){
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
  
  for (int i=0; i<setlist.length(); ++i){
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
IntegerVector get_subset_old(CharacterVector set, List setlist, bool all=false)
{
  if (all) return get_subset_all_(set, setlist);
  else return get_subset_one_(set, setlist);
}


IntegerVector get_superset_one_(CharacterVector x, List setlist){
  bool outb=false;
  int val=-1, k=0;
  
  for (int i=0; i<setlist.length(); ++i) {
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
  
  for (int i=0; i<setlist.length(); ++i){
    CharacterVector set=setlist[i];
    bool out = (any(is_na(match(x, set))));
    out = ! out;
    if (out) vec[k++] = i+1;
  }
  
  IntegerVector out = IntegerVector(k);
  if (k > 0){
    for (int i=0; i<k; ++i) out[i]=vec[i];
  }
  
  return out;
}


//[[Rcpp::export]]
IntegerVector get_superset_old(CharacterVector set, List setlist, bool all=false)
{
  if (all) return get_superset_all_(set, setlist);
  else return get_superset_one_(set, setlist);
}









