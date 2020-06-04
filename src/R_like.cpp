#include <Rcpp.h>
using namespace Rcpp;
using namespace std;

Rcpp::IntegerVector order_(Rcpp::IntegerVector x) {
  if (is_true(any(duplicated(x)))) {
    Rf_warning("There are duplicates in 'x'; order not guaranteed to match that of R's base::order");
  }
  Rcpp::IntegerVector sorted = clone(x).sort();
  return match(sorted, x);
}

// See https://stackoverflow.com/questions/21609934/ordering-permutation-in-rcpp-i-e-baseorder
// [[Rcpp::plugins(cpp11)]]
template <int RTYPE>
IntegerVector order_impl(const Vector<RTYPE>& x, bool desc) {
    auto n = x.size();
    IntegerVector idx = no_init(n);
    std::iota(idx.begin(), idx.end(), static_cast<size_t>(1));
    if (desc) {
        auto comparator = [&x](size_t a, size_t b){ return x[a - 1] > x[b - 1]; };
        std::stable_sort(idx.begin(), idx.end(), comparator);
    } else {
        auto comparator = [&x](size_t a, size_t b){ return x[a - 1] < x[b - 1]; };
        std::stable_sort(idx.begin(), idx.end(), comparator);
        // simulate na.last
        size_t nas = 0;
        for (int i = 0; i < n; ++i, ++nas)
            if (!Vector<RTYPE>::is_na(x[idx[i] - 1])) break;
        std::rotate(idx.begin(), idx.begin() + nas, idx.end());
    }
    return idx;
}


// [[Rcpp::export]]
IntegerVector order2_(SEXP x, bool desc = false) {
    switch(TYPEOF(x)) {
    case INTSXP: return order_impl<INTSXP>(x, desc);
    case REALSXP: return order_impl<REALSXP>(x, desc);
    case STRSXP: return order_impl<STRSXP>(x, desc);
    default: stop("Unsupported type.");
    }
}
