/*
  MAXIMUM CARDINALITY SEARCH on undirected graph. 

  Input:
  X_ : adjacency matrix
  mcs0idx: A 0-based vector with the desidered ordering; followed as far as possible. 
  If NULL it is taken to be 0...ncol(X).

  Output: 
  Vector with ordering.
  If graph is not chordal, first value in vector is -1

  Known issues: 
  1) No check for symmetry of X
  2) If mcs0idx is given it must contain values 0...ncol(X) in some permutation

*/

#include <RcppEigen.h>
//[[Rcpp::depends(RcppEigen)]]
//[[Rcpp::interfaces(r,cpp)]]

#ifndef BEGIN_RCPP
#define BEGIN_RCPP
#endif

#ifndef END_RCPP
#define END_RCPP
#endif

using namespace Rcpp;
using namespace Eigen;
using namespace std;
using Eigen::Map;

typedef Eigen::MappedSparseMatrix<double> MSpMat;
typedef Eigen::SparseMatrix<double> SpMat;
typedef Eigen::Map<Eigen::MatrixXi> MapMati;
typedef Eigen::Map<Eigen::MatrixXd> MapMatd;
typedef Eigen::SparseVector<double> SpVec;
typedef SpVec::InnerIterator InIterVec;
typedef MSpMat::InnerIterator InIterMat;


bool do_is_complete_sparse (const MSpMat& X, SpVec sidx){
  int n = X.cols();
  if (X.rows() != n) throw std::invalid_argument("Sparse matrix X must be square");
  for (InIterVec ii_(sidx); ii_; ++ii_){
    int i0 = ii_.value() - 1;      //Rcpp::Rcout << "i0 = " << i0 << std::endl;
    InIterMat it(X, i0);           // iterator of the i0-column

    for (InIterVec kk_(sidx); kk_; ++kk_){
      int k0 = kk_.value() - 1;    //Rcpp::Rcout << " k0 = " << k0 << ", it.row =";
      if (k0 == i0) continue;
      bool foundit = false;
      for (; it; ++it) {           //Rcpp::Rcout << " " << it.row();
  	if (it.row() == k0) {
  	  foundit = true;
  	  ++it;
  	  break;
  	}
  	if (it.row() > k0) return false;
      }
      if (!foundit) return false;  //Rcpp::Rcout << std::endl;
    }
  }
  return true;
}

bool do_is_complete_dense( const NumericMatrix& X, const IntegerVector& idx){
  int i, j, k;
  
  int M = sum( idx );
  IntegerVector idx_s = no_init(M);
  for (k=0, i=0; i<idx.size(); ++i){
    if (idx[i] > 0) 
      idx_s[k++] = i ;
  }
  
  int N=idx_s.size();
  if (N==0){
    return true;
  } else {
    for (i=0; i<N-1; ++i){
      for (j=i+1; j<N; ++j){
		//Rprintf("i=%d j=%d idx_s[i]=%d idx_s[j]=%d\n", i, j, idx_s[i], idx_s[j]);
		if( X( idx_s[i], idx_s[j]) == 0) return false;
      }      
    }
    return true;
  }
}

//[[Rcpp::export]]
IntegerVector do_mcs_sparse  (const MSpMat& X, const IntegerVector& mcs0idx_ ){
  //const MSpMat   X(Rcpp::as<MSpMat>(XX_));
  
  int nrX(X.rows()), count=1;
  int ii_mark, max_pas, npasnbr, is_perfect=1;
  bool iscomp;
  Eigen::VectorXi mcs0idx(as<Eigen::VectorXi>(mcs0idx_));
  Eigen::VectorXi res(nrX), n_pas_nbr(nrX), pas_nbr(nrX);
  Eigen::VectorXd pas(nrX), act(nrX), tmp(nrX); ;
  SpVec pas_s(nrX), act_s(nrX), vec1_s(nrX), vec2_s(nrX), vv(nrX), uu(nrX);
  SpVec pas_nbr_s(nrX);

  pas.setZero();
  act.setOnes();
  pas_s = pas.sparseView();
  act_s = act.sparseView();
  n_pas_nbr.setZero();

  ii_mark = mcs0idx[0];
  pas[ii_mark] = 1;          act[ii_mark] = 0;
  pas_s = pas.sparseView();  act_s = act.sparseView();
  res[0] = ii_mark;
  
  // update number of passive nbrs for selected node
  vec1_s = X.col(ii_mark);
  for (InIterVec itjj(vec1_s); itjj; ++itjj){
    vec2_s    = X.col( itjj.index() );
    pas_nbr_s = vec2_s.cwiseProduct( pas_s );
    n_pas_nbr[ itjj.index() ] = pas_nbr_s.sum();
  }
  
  if(nrX>1){
    while(count<nrX){
      max_pas=-1;
      // loop over active nodes
      for (InIterVec jj_(act_s); jj_; ++jj_){
		if ( n_pas_nbr[ jj_.index() ] > max_pas ){
		  ii_mark = jj_.index();
		  max_pas = n_pas_nbr[ jj_.index() ];
		}
      }
      if ((n_pas_nbr[mcs0idx[count]]==max_pas) & (act[mcs0idx[count]]!=0)){
		ii_mark=mcs0idx[count];
      }
	  
      res[count] = ii_mark;
      pas[ii_mark] = 1;           act[ii_mark] = 0;
      pas_s = pas.sparseView();   act_s  = act.sparseView();
	  
      vec1_s     = X.col(ii_mark);
      pas_nbr_s  = vec1_s.cwiseProduct(pas_s);
      npasnbr    = pas_nbr_s.sum();
	  
      if (npasnbr>1){
		iscomp = do_is_complete_sparse( X, pas_nbr_s );
		if (!iscomp){
		  is_perfect=0;
		  break;
		}
      }
	  
      for (InIterVec itjj(vec1_s); itjj; ++itjj){
		vec2_s    = X.col( itjj.index() );
		pas_nbr_s = vec2_s.cwiseProduct( pas_s );
		n_pas_nbr[ itjj.index() ] = pas_nbr_s.sum();
      }
      count++;
    }
  }
  
  if (is_perfect==0) res[0]=-1;
  return wrap( res ) ;
}




//[[Rcpp::export]]
SEXP do_mcs_dense  ( const NumericMatrix& X, const IntegerVector& mcs0idx ){
  
  int nrX =X.rows(), count=1, i;
  int ii_mark, max_pas, npasnbr, is_perfect=1;
  IntegerVector res(nrX), n_pas_nbr(nrX), pas_nbr_s(nrX);
  NumericVector pas(nrX), act(nrX), tmp(nrX), vec1(nrX), vec2(nrX);
  
  std::fill(act.begin(), act.end(), 1);
  
  ii_mark = mcs0idx[0];
  res[0] = ii_mark;
  pas[ii_mark] = 1;  act[ii_mark] = 0;
  
  vec1 = X( _, ii_mark);
  for (i=0; i<nrX; ++i){
    if (vec1[i]!=0){
      vec2 = X( _, i);
      pas_nbr_s = vec2 * pas;
      n_pas_nbr[ i ] = sum( pas_nbr_s );
    }
  }
  
  if(nrX>1){
    while(count<nrX){
      max_pas=-1;
      // loop over active nodes
      for (i=0; i<nrX; ++i){
		if (act[i]!=0){
		  //j = act[i];
		  if (n_pas_nbr[ i ] > max_pas){
			ii_mark = i;
			max_pas = n_pas_nbr[ i ];
		  }
		}
      }
	  
      if ((n_pas_nbr[mcs0idx[count]]==max_pas) & (act[mcs0idx[count]]!=0)){
		ii_mark=mcs0idx[count];
      }
	  
      res[count] = ii_mark;
      pas[ii_mark] = 1; act[ii_mark] = 0;
      vec1     = X(_, ii_mark);
      pas_nbr_s  = vec1 * pas;
      npasnbr    = sum( pas_nbr_s );
	  
      if (npasnbr>1){
		bool iscomp = do_is_complete_dense(X, pas_nbr_s);
		if (!iscomp){
		  is_perfect=0; break;
		}
      }
	  
      for (i=0; i<nrX; ++i){
		if (vec1[i]!=0){
		  vec2 = X(_,i);
		  pas_nbr_s = vec2 * pas;
		  n_pas_nbr[ i ] = sum( pas_nbr_s );
		}
      }
      count++;
    }
  }
  
  if (is_perfect==0) res[0]=-1;
  // return wrap( res ) ;
  return res;
}

// [[Rcpp::export]]
SEXP mcsMAT0_ ( SEXP XX_, SEXP mcs0idx_=R_NilValue ){
  RObject zz_ = mcs0idx_;
  IntegerVector mcs0idx;
  int type = TYPEOF(XX_) ;  //Rf_PrintValue(wrap(type));
  switch( type ){
  case INTSXP  : 
  case REALSXP : {
    NumericMatrix X(as<NumericMatrix>(XX_));
    if (zz_.isNULL())
      mcs0idx = seq(0, X.ncol()-1);
    else
      mcs0idx = mcs0idx_;
    return do_mcs_dense ( X, mcs0idx ); 
  }
  case S4SXP   : {                               
    MSpMat X(as<MSpMat>(XX_));
    if (zz_.isNULL())
      mcs0idx = seq(0, X.cols()-1);
    else
      mcs0idx = mcs0idx_;
    return do_mcs_sparse( X, mcs0idx );
  } 
  }
  return R_NilValue ;
}




/*** R

library(gRbase)
M1  <- ug(~a:b:c:d + d:e:f + b:c:d, result="Matrix")
m1  <- as(M1, "matrix")
mcs0 <- 0:(ncol(M1)-1)
do_mcs_sparse(M1, mcs0)
do_mcs_dense(m1, mcs0)
do_mcs_sparse(M1, mcs0)

mcs0 <- rev(mcs0)
mcsMAT0_(M1, mcs0)
mcsMAT0_(m1, mcs0)
mcsMAT0_(M1)
mcsMAT0_(m1)

gg <-triangulate(moralize(random_dag(1:1000, maxpar = 30,wgt=.6) ))
M1 <- as(gg,"dgCMatrix")
m1  <- as(M1, "matrix")
mcs0 <- 0:(ncol(M1)-1)

library(microbenchmark)
microbenchmark( do_mcs_sparse(M1, mcs0), do_mcs_dense(m1, mcs0), 
mcsMAT0_(M1, mcs0), mcsMAT_(M1, mcs0), 
mcsMAT0_(m1, mcs0), mcsMAT_(m1, mcs0), 
times=10
)

*/


// bool do_is_complete_dense( const NumericMatrix& X, const IntegerVector idx){
//   int i, j, nrX=X.nrow();

//   for (i=0; i<nrX; ++i){
//     if (idx[i]!=0){
//       for (j=i+1; j<nrX; ++j){
// 	if (idx[j]!=0){
// 	  if (i>j)
// 	    if( X( i, j) == 0) return false;
// 	}
//       }
//     }
//   }
//   return true;
// }
