## FIXME : This .h needs updates

SEXP tab_perm_(const SEXP& tab, const SEXP& perm);
SEXP tab_expand_(const SEXP& tab, const SEXP& aux);
SEXP tab_align_(const SEXP& tab1, const SEXP& tab2);
SEXP tab_marg_(const SEXP& tab, const SEXP& marg);

NumericVector tab_op_(const NumericVector& tab1, const NumericVector& tab2, const char op='*');
NumericVector tab_add_(const NumericVector& tab1, const NumericVector& tab2);
NumericVector tab_subt_(const NumericVector& tab1, const NumericVector& tab2);
NumericVector tab_mult_(const NumericVector& tab1, const NumericVector& tab2);
NumericVector tab_div_(const NumericVector& tab1, const NumericVector& tab2);
NumericVector tab_div0_(const NumericVector& tab1, const NumericVector& tab2);
bool tab_equal_(const NumericVector& tab1, const NumericVector& tab2, double eps=1e-12);

// FIXME: ALIASES for gRain compatibility; July 2017
SEXP tabMarg__(const SEXP& tab, const SEXP& marg);
NumericVector tabDiv0__(const NumericVector& tab1, const NumericVector& tab2);
NumericVector tabMult__(const NumericVector& tab1, const NumericVector& tab2);





