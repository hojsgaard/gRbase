// **********************************************************************
// rip_internal: Returns RIP-ordering of cliques of chordal graph
// together with children parents; that is, the function returns a
// junction tree.
//
// mcs0idx: Perfect ordering (0-based of nodes)
// vn: Node names (mcs0idx referes to this set)
// cqlist: The cliques.

// Known issues: Code not templated; unclear where this code is used.

// **********************************************************************

#ifndef RIPCLIQUE_H
#define RIPCLIQUE_H

#include "R_like.h"
#include <RcppEigen.h>
#include "_g_mcsMAT2.h"

//[[Rcpp::depends(RcppEigen)]]
//[[Rcpp::interfaces(r,cpp)]]

// //[[Rcpp::depends(RcppEigen,RcppArmadillo,gRbase)]]
// #include <gRbase.h>
// using namespace gRbase

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


bool is_subset_of_ (CharacterVector v1, CharacterVector v2){
  if ( setdiff( v1, v2 ).size() > 0){
    return false;
  } else {
    return true;
  }
}


//[[Rcpp::export]]
List rip_internal(IntegerVector mcs0idx, CharacterVector vn, List cqlist){
  int ncq = cqlist.size();
  IntegerVector ord=no_init( ncq ), tmp=no_init( ncq ), zzz=no_init( ncq );

  CharacterVector mcs_vn( vn.size() );
  for (int i=0; i<vn.size(); ++i){
    mcs_vn[i] = vn[ mcs0idx[i] ]; //pas på; mcs0idx er 0-based her!!!
  }
  //Rprintf("mcs_vn:"); print(mcs_vn);

  for (int i=0; i<ncq; ++i){
    CharacterVector cq = cqlist[i]; //print( cq );  print( match( cq, vn) );
    ord[i] = max( match( cq, mcs_vn) );
  }
  ord = order_(ord);

  List cqlist2( ncq ), separators( ncq );
  IntegerVector pavec( ncq ), chvec( ncq ), host( vn.size() );

  for (int i=0; i<ncq; ++i){
    CharacterVector v = cqlist(ord[i] - 1);
    cqlist2(i) = v;
  }

  CharacterVector cq = cqlist2( 0 );
  CharacterVector past = cq;
  // update host
  IntegerVector idx = match(cq, vn);
  for (int k=0; k<cq.size(); ++k) host[idx[k] - 1] = 1;

  for (int i=1; i<ncq; ++i){
    CharacterVector cq = cqlist2(i);         
    // update host
    IntegerVector idx = match(cq, vn);
    for (int k=0; k<cq.size(); ++k) host[idx[k] - 1] = i + 1;

    CharacterVector isect = intersect( past, cq ); 
    separators(i) = isect;
    if (isect.size()>0){
      for (int j=i-1; j>=0; --j){
	CharacterVector cq2 = cqlist2( j );     
	if (is_subset_of_( isect, cq2 ) ){      
	  pavec[i] = j+1 ;
	  chvec[j] = i+1 ;
	  break;
	}
      }
    }
    past = union_( past, cq );
  }

  List out=List::create(_["nodes"]=vn,
			_["cliques"]=cqlist2,
			_["separators"]=separators,
			_["parents"]=pavec,
			_["children"]=chvec,
			_["host"]=host);
  out.attr("class")="ripOrder";
  return out;

}


/* *******************************************************
  getcq_: Returns cliques of chordal graph:
  XX_: Adjacency matrix
  mcs0idx_: Perfect ordering of nodes
  Notice: NO checks for being chordal
  ***************************************************** */

SEXP do_getcq_sparse( SEXP XX_, const IntegerVector& mcs0idx_){

  MSpMat   X(as<MSpMat>(XX_));
  S4     Xin(XX_);
  List vnl = clone(List(Xin.slot("Dimnames")));
  CharacterVector vn=vnl[0];
  Eigen::VectorXi mcs0idx(as<Eigen::VectorXi>(mcs0idx_));
  int nrX(X.rows()), i, j, k, l, past;
  SpVec pas( nrX ), vec_s( nrX ), vec2_s( nrX );
  IntegerVector ggg( nrX );
  
  pas.setZero();
  for (i=0; i<nrX; ++i){
    j = mcs0idx[i];  // Rprintf("i=%d, j=%d, past=%d\n", i, j, past);
    vec_s = X.col( j );
    vec2_s = vec_s.cwiseProduct(pas);
    past = vec2_s.sum();
    pas.coeffRef(i) = 1;
    ggg[ mcs0idx[i] ] = past;
  }
  // cout << vec_s.transpose() << endl; cout << pas.transpose() << endl;

  IntegerVector ladder( nrX );
  for (i=0; i<nrX-1; ++i){
    if(ggg[i] + 1 > ggg[i + 1]) ladder[i] = 1;
  }
  ladder[nrX - 1] = 1; //Rprintf("ladder: "); print( ladder );
  int ncq = sum(ladder);
  List cqlist(ncq);
  pas.setZero();
  l=0;
  for (i=0; i<nrX; ++i){
    if (ladder[i] > 0){
      j = mcs0idx[i];
      vec_s  = X.col( j );
      vec2_s = vec_s.cwiseProduct(pas);
      past = vec2_s.sum();   //Rprintf("i=%d, j=%d, past=%d\n", i, j, past);
      IntegerVector cq(past+1);
      //cout << "vec2_s " << vec2_s.transpose() << endl; print( cq );
      k=0;
      for (InIterVec it2(vec2_s); it2; ++it2){
  	cq[k++]=it2.index();
      }
      cq[past] = j;
      CharacterVector cq2(past+1);
      for (k=0; k<past+1;++k) cq2[k]=vn[cq[k]];
      cqlist[l++] = cq2;     //print( cq );
    }
    pas.coeffRef(i) = 1;
  }
  return cqlist; //List::create( cqlist );
  //return List::create(1);
}



SEXP do_getcq_dense(NumericMatrix X, const IntegerVector& mcs0idx){

  List vnl = clone(List(X.attr("dimnames")));
  CharacterVector vn=vnl[0];

  int nrX = X.rows(), i, ii, j, k, l, past;
  
  IntegerVector pas( nrX ), vec_s( nrX ), vec2_s( nrX );
  IntegerVector ggg( nrX );
  
  // pas.setZero();
  for (i=0; i<nrX; ++i){
    j = mcs0idx[i];  // Rprintf("i=%d, j=%d, past=%d\n", i, j, past);
    vec_s = X(_, j );
    vec2_s = vec_s * pas ;
    past = sum( vec2_s );
    pas[i] = 1;
    ggg[ mcs0idx[i] ] = past;
  }
  
  // // cout << vec_s.transpose() << endl; cout << pas.transpose() << endl;

  IntegerVector ladder( nrX );
  for (i=0; i<nrX - 1; ++i){
    if( ggg[i] + 1>ggg[i+1]) ladder[i] = 1;
  }
  ladder[nrX - 1]=1; //Rprintf("ladder: "); print( ladder );
  int ncq = sum( ladder );
  List cqlist(ncq);
  for (i=0; i<nrX; ++i) pas[i]=0;
  // pas.setZero();
  l=0;
  for (i=0; i<nrX; ++i){
    if (ladder[i]>0){
      j = mcs0idx[i];
      vec_s  = X(_, j );
      vec2_s = vec_s * pas;
      past = sum( vec2_s ) ;   //Rprintf("i=%d, j=%d, past=%d\n", i, j, past);
      IntegerVector cq(past+1);
      //cout << "vec2_s " << vec2_s.transpose() << endl; print( cq );
      k=0;
      for (ii=0; ii<nrX; ++ii){
	if (vec2_s[ii] != 0)
	  cq[k++] = ii;
      }
      cq[past] = j;
      CharacterVector cq2(past+1);
      for (k=0; k<past + 1; ++k) cq2[k]=vn[cq[k]];
      cqlist[l++] = cq2;     //print( cq );
    }
    pas[i] = 1;
  }
  return cqlist; //List::create( cqlist );
  //return List::create(1);
}


// [[Rcpp::export]]
SEXP getCliquesDec__ (SEXP XX_, SEXP mcs0idx_=R_NilValue){
  int type = TYPEOF(XX_) ;  //print(wrap(type));
  IntegerVector mcs0idx ;   // = mcsMAT0_( XX_ );
  RObject zz_ = mcs0idx_;
  
  if (zz_.isNULL())
    mcs0idx = mcsMAT0_( XX_ );
  else
    mcs0idx = mcs0idx_;

  if (mcs0idx[0] < 0)
    return R_NilValue ;
  
  switch( type ){
  case INTSXP  : 
  case REALSXP : return do_getcq_dense ( XX_, mcs0idx ); 
  case S4SXP   : {                               
    MSpMat X(as<MSpMat>(XX_));
    return do_getcq_sparse( XX_, mcs0idx );
  }
  default: stop("Unsupported type.");  
  }
  return R_NilValue ;
}


#endif



/*** R

library(gRbase)
M1  <- ug(~a:b:c:d + d:e:f + b:c:d, result="Matrix")
m1  <- as(M1, "matrix")
mcs0 <- 0:(ncol(M1)-1)
#do_mcs_sparse(M1, mcs0)
#do_mcs_dense(m1, mcs0)
#do_mcs_sparse(M1, mcs0)
#mcs0 <- rev(mcs0)
#mcsMAT0_(M1, mcs0)
#mcsMAT0_(m1, mcs0)
#mcsMAT0_(M1)
#mcsMAT0_(m1)

oo <- mcsMAT0_(M1)
do_getcq_sparse(M1, oo)

getCliquesDec_(M1)
getCliquesDec_(m1)

microbenchmark::microbenchmark(
getCliques(M1), getCliquesDec_(M1),
getCliques(m1), getCliquesDec_(m1)
)

gg <-triangulate(moralize(random_dag(1:100, maxpar = 30,wgt=.6) ))
M1 <- as(gg,"dgCMatrix")
m1  <- as(M1, "matrix")

oo <- mcsMAT0_(M1)

microbenchmark::microbenchmark(
getCliques(M1), getCliquesDec_(M1), getCliquesDec_(M1, oo),
getCliques(m1), getCliquesDec_(m1), getCliquesDec_(m1, oo)
)

microbenchmark::microbenchmark(
mcsMAT0_(M1), mcsMAT0_(m1)
)

*/


// gg <-triangulate(moralize(random_dag(1:1000, maxpar = 30,wgt=.6) ))
// M1 <- as(gg,"dgCMatrix")
// m1  <- as(M1, "matrix")
// mcs0 <- 0:(ncol(M1)-1)

// library(microbenchmark)
// microbenchmark( do_mcs_sparse(M1, mcs0), do_mcs_dense(m1, mcs0), 
// mcsMAT0_(M1, mcs0), mcsMAT_(M1, mcs0), 
// mcsMAT0_(m1, mcs0), mcsMAT_(m1, mcs0), 
// times=10
// )
