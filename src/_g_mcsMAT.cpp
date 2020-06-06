/*
  Maximum cardinality search on undirected graph. If fail, first returned value is -1
  Known issues: FIXME: No check for symmetry, code not templated. Messy with two implementations!
  Author: Soren Hojsgaard
*/

// #include <iostream>
// #include <iterator> // for ostream_iterator
#include <RcppEigen.h>
//[[Rcpp::depends(RcppEigen)]]

using namespace Rcpp;
using namespace Eigen;
using namespace std;
using Eigen::Map;

typedef Eigen::MappedSparseMatrix<double> MSpMat;
typedef Eigen::SparseMatrix<double> SpMat;
typedef Eigen::Map<Eigen::MatrixXi> MapMati;
typedef Eigen::Map<Eigen::MatrixXd> MapMatd;
typedef Eigen::SparseVector<double> SpVec;
typedef SpVec::InnerIterator InIter;
typedef SpVec::InnerIterator InIterVec;
typedef MSpMat::InnerIterator InIterMat;


/* ***********************************************************
  internal_mcsMAT_sp: Maximum cardinality search
  X: Adjacency matrix (Sparse!)
  OO_: Desired ordering; followed as far as possible.
 *************************************************************/

SEXP internal_mcsMAT_sp ( SpMat X, SEXP OO_ ){

  int nrX(X.rows());
  int count=1;
  
  Eigen::VectorXi O(as<Eigen::VectorXi>(OO_));
  Eigen::VectorXi res(nrX);
  
  // debugging info
  //int dd(as<int>(dd_)), ddd(as<int>(ddd_));
  //int dd=0, ddd=0;
  
  Eigen::VectorXi n_pas_nbr(nrX);
  Eigen::VectorXd pas(nrX), act(nrX);
  SpVec pas_s(nrX), act_s(nrX), vec1_s(nrX), vec2_s(nrX);
  SpVec pas_nbr_s(nrX);

  pas.setZero();
  act.setOnes();
  pas_s = pas.sparseView();
  act_s = act.sparseView();
  n_pas_nbr.setZero();

  int ii_mark, max_pas, n_nbr_req, n_nbr_obs, npasnbr, is_perfect=1;

  // if(dd)Rcout << "*INITIALIZATION" << endl;

  ii_mark = O[0];
  pas[ii_mark] = 1;
  act[ii_mark] = 0;
  pas_s = pas.sparseView();
  act_s = act.sparseView();
  
  // pas_s.coeffRef( ii_mark ) = 1;
  // act_s.coeffRef( ii_mark ) = 0;

  // if(dd)Rcout << " ** ii_mark=" << ii_mark << endl;
  // if(ddd)Rcout << "   pas  : " << pas.transpose() << endl;
  // if(ddd)Rcout << "   act  : " << act.transpose() << endl;

  res[0] = ii_mark;

  vec1_s = X.col(ii_mark);
  for (InIter itjj(vec1_s); itjj; ++itjj){
    vec2_s    = X.col( itjj.index() );
    pas_nbr_s = vec2_s.cwiseProduct( pas_s );
    n_pas_nbr[ itjj.index() ] = pas_nbr_s.sum();
  }
  
  if(nrX > 1){
    while(count < nrX){
      max_pas = -1;
      for (InIter jj_(act_s); jj_; ++jj_){
	if ( n_pas_nbr[ jj_.index() ] > max_pas ){
	  ii_mark = jj_.index();
	  max_pas = n_pas_nbr[ jj_.index() ];
	}
      }
      if ((n_pas_nbr[O[count]] == max_pas) & (act[O[count]] != 0)){
	ii_mark = O[count];
      }
      
      res[count]   = ii_mark;
      pas[ii_mark] = 1;
      act[ii_mark] = 0;
      pas_s  = pas.sparseView();
      act_s  = act.sparseView();
      
      pas_nbr_s  = X.col( ii_mark ).cwiseProduct(pas_s);
      npasnbr    = pas_nbr_s.sum();
	  
      n_nbr_obs = 0;
      n_nbr_req = 0;
      for (InIter it2(pas_nbr_s); it2; ++it2){
	for (InIter it3(pas_nbr_s); it3; ++it3){
	  n_nbr_req++;
	  n_nbr_obs += X.coeff(it2.index(), it3.index());
	}
      }
      n_nbr_obs /= 2;
      if (npasnbr == 0)
	n_nbr_req = 0;
      else
	n_nbr_req = npasnbr * (npasnbr - 1) / 2;
      
      if (n_nbr_req != n_nbr_obs){
	is_perfect=0;
	break;
      }
      
      vec1_s = X.col(ii_mark);
      for (InIter itjj(vec1_s); itjj; ++itjj){
	vec2_s    = X.col( itjj.index() );
	pas_nbr_s = vec2_s.cwiseProduct( pas_s );
	n_pas_nbr[ itjj.index() ] = pas_nbr_s.sum();
      }
      count++;
    }
  }

  // if(dd)Rcout << "*FINALIZE" << endl;
  
  if (is_perfect == 0)
    res[0] = -1;
  return(wrap(res));
}


SEXP do_mcsMAT_de ( SEXP XX_, SEXP OO_ ){
  MapMatd  Xd(as<MapMatd>(XX_));
  SpMat   X = Xd.sparseView();
  return internal_mcsMAT_sp(X, OO_);
}

SEXP do_mcsMAT_sp ( SEXP XX_, SEXP OO_ ){
  SpMat   Xd(as<SpMat>(XX_));
  return internal_mcsMAT_sp(Xd, OO_);
}

// [[Rcpp::export]]
SEXP mcsMAT__ ( SEXP XX_, SEXP OO_ ){
  int type = TYPEOF(XX_) ;
  //Rf_PrintValue(wrap(type));
  switch( type ){
  case INTSXP  : return do_mcsMAT_de(XX_, OO_); // matrix - integer
  case REALSXP : return do_mcsMAT_de(XX_, OO_); // matrix - double
  case S4SXP   : return do_mcsMAT_sp(XX_, OO_); // dgCMatrix
  default: Rf_error("Unsupported type");
  }
}




/*** R

library(gRbase)
M1  <- ug(~a:b:c:d + d:e:f + b:c:d, result="Matrix")
m1  <- as(M1, "matrix")
mcs0 <- 0:(ncol(M1)-1)
mcsMAT_(M1, mcs0)


 */

// library(microbenchmark)
// microbenchmark( do_mcs_sparse(M1, mcs0), do_mcs_dense(m1, mcs0), do_mcsMAT_sp(M1, mcs0), times=10)


// gg<-triangulate(moralize(random_dag(1:30, maxpar = 10,wgt=.6) ))
// M1 <- as(gg,"dgCMatrix")
// mcs0 <- 0:(ncol(M1)-1)
// #do_mcs_sparse(M1, mcs0)
// #do_mcsMAT_sp(M1, mcs0)

// library(microbenchmark)
// microbenchmark( do_mcs_sparse(M1, mcs0), mcs2_(M1, mcs0), do_mcsMAT_sp(M1, mcs0))


// //idx = c(2,3,4)
// //microbenchmark( foo(M1, idx), foo2(M1, idx))







// getcq <- function(M1){
// mcs0 <- do_mcsMAT_sp(M1, 0:(ncol(M1)-1))
// getcq_(M1, mcs0)




// library(microbenchmark)
// microbenchmark( getCliques(M1), getcq(M1), getcq_(M1, mcs0))

// gg<-triangulate(moralize(random_dag(1:200, maxpar = 10,wgt=.6) ))
// M1 <- as(gg,"dgCMatrix")
// mcs0 <- mcsMAT(M1, index = T)-1
// microbenchmark( getCliques(M1), getcq(M1), getcq_(M1, mcs0))


// oo  <- 1:ncol(M1)-1

// mcs0idx  <- do_mcsMAT_sp(M1, oo);
// vn  <- colnames(M1)
// cqlist <- gRbase::getCliques(M1)

// str( (x1 <- rip_internal( mcs0idx, vn, cqlist ) ) )
// str( (x2 <- ripMAT(M1)))

// oo  <- 1:ncol(M1)-1
// mcs0idx  <- do_mcsMAT_sp(M1, oo);
// vn  <- colnames(M1)
// cqlist <- gRbase::getCliques(M1)
// str( (x1 <- rip_internal( mcs0idx, vn, cqlist ) ) )
// str( (x2 <- ripMAT(M1)))


// ripMAT2 <- function(amat){
//   mcs0idx <- mcsMAT( amat, index=TRUE ) - 1
//   vn <- colnames( amat )
//   cqlist <- getCliques( amat )
//   rip_internal( mcs0idx, vn, cqlist)
// }


// require(microbenchmark)
// microbenchmark( ripMAT(M1), ripMAT2(M1) )





// oo  <- 1:ncol(M1)-1

// mcs0idx  <- do_mcsMAT_sp(M1, oo);
// vn  <- colnames(M1)
// cqlist <- gRbase::getCliques(M1)

// str( rip_internal( mcs0idx, vn, cqlist ) )
// str( ripMAT(M1))

// Rprof()
// for (i in 1:5000) ripMAT2(M1)
// Rprof(NULL)
// sumcs0idxaryRprof()


// #do_mcsMAT_sp(M1, oo)
// #do_mcsMAT_de(m1, oo)

// M1 <- ug(~a:b+b:c+c:d+d:e:f+f:a, result="Matrix")
// m1 <- as(M1, "matrix")
// oo <- sample(1:ncol(M1)-1)

// #do_mcsMAT_sp(M1, oo)
// #do_mcsMAT_de(m1, oo)

// library(microbenchmark)
// microbenchmark(
// mcsMAT_(M1, oo),
// mcsMAT_(m1, oo)
// #do_mcsMAT_sp(M1, oo),
// #do_mcsMAT_de(m1, oo)
// )

// dd <- random_dag(1:100, max=5, wgt=.8)
// MCS0IDX <- as(dd, "dgCMatrix")
// MCS0IDX <- MCS0IDX + Matrix::t(MM)
// mm <- as(MM, "matrix")

// oo <- sample(1:ncol(MM)-1)

// library(microbenchmark)
// microbenchmark(
// #do_mcsMAT_sp(MM, oo),
// #do_mcsMAT_de(mm, oo),
// mcsMAT_(MM, oo),
// mcsMAT_(mm, oo),
// gRbase::mcsMAT(mm),
// gRbase::mcsMAT(MM)
// )
