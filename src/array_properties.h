#ifndef ARRAYPROP_H
#define ARRAYPROP_H

#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace std;

bool is_dimnames_(const SEXP& obj);
bool is_number_vector_(const SEXP& obj);
bool is_named_array_(const SEXP& obj);
bool dimnames_match_(const SEXP& tab1, const SEXP& tab2, bool verbose=false);

#endif
