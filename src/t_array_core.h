## FIXME : This .h needs updates
## SEXP aperm__(const SEXP& tab, const SEXP& perm);
SEXP tab_perm_(const SEXP& tab, const SEXP& perm);
NumericVector tabExpand__(const NumericVector& t1, const NumericVector& t2);
NumericVector tabAlign__(const NumericVector& t1, const NumericVector& t2, double eps=1e-12);

NumericVector tabMult__(NumericVector t1, NumericVector t2);
NumericVector tabDiv__(NumericVector t1, NumericVector t2);
NumericVector tabDiv0__(NumericVector t1, NumericVector t2);
NumericVector tabAdd__(NumericVector t1, NumericVector t2);
NumericVector tabSubt__(NumericVector t1, NumericVector t2);

bool tabEqual__(NumericVector t1, NumericVector t2, double eps=1e-12);

##SEXP tabMargc__(const SEXP& t1, const SEXP& margc);
##SEXP tabMargi__(const SEXP& t1, const SEXP& margi);
##SEXP tabMarg__(const SEXP& t1, const SEXP& marg);
##SEXP tab_marg_(const SEXP& t1, const SEXP& marg);

SEXP tab_marg_(const SEXP& tab, const SEXP& marg){
