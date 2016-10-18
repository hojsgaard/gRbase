#include <Rcpp.h>
using namespace Rcpp;

// //[[Rcpp::export]]
CharacterMatrix do_names2pairs_(CharacterVector x, CharacterVector y){
	int lenx = x.length(), leny = y.length();
	int i,j,k;


	if (leny == 0){
		if (lenx == 1){
			CharacterMatrix out(lenx*leny, 2);
			return out;
		} else {
			CharacterMatrix out(lenx*(lenx-1)/2, 2);
			k=0;
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
		k=0;
		for (i=0; i<lenx; ++i){
			for (j=0; j<leny; ++j){
				out(k,   0) = x[i];
				out(k++, 1) = y[j];
			}
		}
		return out;
	}
}

// //[[Rcpp::export]]
CharacterMatrix sortmat_(CharacterMatrix X){
	CharacterMatrix X2=clone(X);
	CharacterVector tmp(1);
	for (int i=0; i<X2.rows(); ++i){
		if (X2(i,0) > X2(i,1)){
			tmp[0]=X2(i,0);
			X2(i,0)=X2(i,1);
			X2(i,1)=tmp[0];
		}
	}
	return X2;
}


//[[Rcpp::export]]
SEXP names2pairsM(CharacterVector x,
														 CharacterVector y=CharacterVector(0),
														 bool sort=false,
														 std::string result="matrix"){
	CharacterMatrix out = do_names2pairs_(x, y);
	if (sort)
		out = sortmat_(out);

	if (result=="list"){
		List outlist(out.nrow());
		for (int i=0; i<out.nrow(); ++i)
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

