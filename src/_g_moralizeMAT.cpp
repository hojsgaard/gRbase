/* 
   moralizeMAT: Moralizes dag
   
   Known issues: Does not check if graph is a dag, code not templated

*/

#include <RcppEigen.h>
//[[Rcpp::depends(RcppEigen)]]


#ifndef BEGIN_RCPP
#define BEGIN_RCPP
#endif

#ifndef END_RCPP
#define END_RCPP
#endif

using namespace Rcpp;

typedef Eigen::MappedSparseMatrix<double> MSpMat;
typedef Eigen::SparseMatrix<double> SpMat;

SEXP do_moralize_sp ( SEXP XX_){
  SpMat   X(as<MSpMat>(XX_));
  
  typedef Eigen::Triplet<double> T;
  std::vector<T> triplets;
  triplets.reserve(X.nonZeros() * 2);
  
  int nrX(X.rows());
  int kk, ll, vv;
  for (vv=0; vv<nrX; vv++){ /* consider vertex vv */
    for (kk=0; kk<nrX; kk++){
      if (X.coeff(kk, vv) != 0){     /* yes, kk->vv */
	for (ll=kk+1; ll<nrX; ll++){
	  if (X.coeff(ll, vv) != 0){ /* yes, ll->vv */
	    if ((X.coeff(kk, ll)==0) && (X.coeff(ll, kk)==0)){ /* kk not~ ll */
	      triplets.push_back(T(kk, ll, 1));
	      triplets.push_back(T(ll, kk, 1));
	    }
	  }
	}
      }
    }
  }
  
  SpMat ans(X.rows(), X.cols());
  ans.setFromTriplets(triplets.begin(), triplets.end());
  SpMat Xt(X.transpose());
  ans = ans + Xt + X;
  
  for (kk=0; kk<nrX; kk++){
    for (ll=kk+1; ll<nrX; ll++){
      if (ans.coeff(kk,ll)!=0){
	ans.coeffRef(kk,ll)=1;
	ans.coeffRef(ll,kk)=1;
      }
    }
  }

  ans.makeCompressed();
  S4 Xin(XX_), Xout( wrap( ans ));
  Xout.slot("Dimnames") = clone(List(Xin.slot("Dimnames")));  
  return Xout;
}

SEXP do_moralize_de ( SEXP XX_){
  NumericMatrix   X(as<NumericMatrix>(XX_));
  NumericMatrix   fill(X.nrow(), X.ncol());

  int nrX = X.nrow();
  int kk, ll, vv;
  for (vv=0; vv<nrX; vv++){ /* consider vertex vv */
    for (kk=0; kk<nrX; kk++){
      if (X(kk, vv) != 0){     /* yes, kk->vv */
  	for (ll=kk+1; ll<nrX; ll++){
  	  if (X(ll, vv) != 0){ /* yes, ll->vv */
  	    if ((X(kk, ll)==0) && (X(ll, kk)==0)){ /* kk not~ ll */
  	      fill(kk, ll) = 1;
	      fill(ll, kk) = 1;
  	    }
  	  }
  	}
      }
    }
  }

  double sum;
  for (kk=0; kk<nrX; kk++){
    for (ll=kk+1; ll<nrX; ll++){
      sum=fill(kk,ll)+X(kk,ll)+X(ll,kk);
      if (sum != 0) {
	fill(kk,ll)=1; fill(ll,kk)=1;
      }
    }
  }


  List dn = clone(List(X.attr("dimnames")));
  fill.attr("dimnames") = clone(List(X.attr("dimnames")));
  return fill;
}



// [[Rcpp::export]]
SEXP moralizeMAT ( SEXP XX_ ){
  int type = TYPEOF(XX_) ;
  //Rf_PrintValue(wrap(type));
  switch( type ){
  case INTSXP  : return do_moralize_de(XX_); // matrix - integer 
  case REALSXP : return do_moralize_de(XX_); // matrix - double
  case S4SXP   : return do_moralize_sp(XX_); // dgCMatrix
  }
  return R_NilValue ;
}



/*** R

M <- gRbase::dag(~a:b:c+b:d+c:d, result="Matrix")
m <- as(M, "matrix")
MM <- do_moralize_sp( M ); MM 
mm <- do_moralize_de(m); mm

library(gRbase)
dd <- random_dag(1:100, maxpar=5, wgt=.9)
MM2 <- as(dd, "dgCMatrix")
mm2 <- as(dd, "matrix")



library(microbenchmark)
microbenchmark(
do_moralize_de(mm2), do_moralize_sp(MM2),
gRbase::moralizeMAT(mm2), moralizeMAT(MM2),
moralizeMAT(mm2), moralizeMAT2(MM2)
)


 */
