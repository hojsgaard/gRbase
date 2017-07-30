/*
  Triangulation of undirected graph with k vertices.
  
  1) Start with all vertices marked as active (coded as 1); set i:=k
  
  2) While there are still active vertices {
  - Select an active vertex v that optimizes some criterion c(v)
  - Label v with the number i; i.e. set v_i = v
  - For the set C_i consisting of v_i and its active neigbours.
  - Fill in edges where none exist between all pairs of vertices in C_i
  - Mark v_i as non-active (coded as 0) and set i:=i-1
  }

  Active neighbours are refered to as anbr

  Known issues: Does not check if graph is undirected; code not templated

*/

#include <RcppEigen.h>
//[[Rcpp::depends(RcppEigen)]]

#ifndef BEGIN_RCPP
#define BEGIN_RCPP
#endif

#ifndef END_RCPP
#define END_RCPP
#endif

typedef Eigen::SparseMatrix<double> SpMat;
typedef Eigen::Map<Eigen::MatrixXi> MapMati;
typedef Eigen::Map<Eigen::MatrixXd> MapMatd;
typedef Eigen::SparseVector<double> SpVec;
typedef SpVec::InnerIterator InIter;

using namespace Rcpp;

SpMat internal_triangulateMAT_sp (SpMat X, SEXP LL_){
  
  using namespace std;
  int dd=0, ddd=0; // debugging info
  Eigen::VectorXd L(as<Eigen::VectorXd>(LL_));
	
  int i, n_anbr, ii_mark, n_nbr_need, n_nbr_obs, nrX(X.rows()), n_active(X.rows());
  double spsize, min_spsize, max_size;
  
  Eigen::VectorXi active(nrX), anbr(nrX), n_anbr_vec(nrX);
  Eigen::VectorXd size_of_active_closure_space(nrX);
  
  SpVec active_sp(nrX), active2_sp(nrX);
  SpVec anbr_sp(X.cols()), anbr2_sp(X.cols());
  SpMat fill(X.rows(), X.cols());
  
  typedef Eigen::Triplet<double> T;
  std::vector<T> triplets;
  triplets.reserve(X.nonZeros() * 2);
  //triplets.reserve(X.rows()*X.cols());
  
  active.setOnes();
  active_sp = active.sparseView();
  max_size = L.sum();
  if(dd) Rcout << "* Initialization\n";
  if(ddd)Rcout << "   active   :  " << active.transpose() << endl;
  if(ddd)Rcout << "   active_sp :  " << active_sp.transpose();
  if(ddd)Rcout << "   max_size :  " << max_size << endl;
  
  // Update the size_of_active_closure_space
  if(dd)Rcout << "* Find statespace size of (active) closure of each node \n";
  for ( i=0; i<nrX; i++){ 
    spsize = L[ i ];
    anbr_sp  = X.col( i ).cwiseProduct(active_sp); 
    n_anbr  = anbr_sp.sum();
    n_anbr_vec[ i ] = n_anbr;
    
    for (InIter _j2(anbr_sp); _j2; ++_j2)
      spsize += L[_j2.index()];
    
    size_of_active_closure_space[i] = spsize;
  }
  
  if(ddd)Rcout << "   size_of_active_closure_space          : " << 
	   size_of_active_closure_space.transpose() << endl;
  if(ddd)Rcout << "   n_anbr_vec (number of actice nbrs) : " << 
	   n_anbr_vec.transpose() << endl;
  
  if(dd)Rcout << "* Iteration \n";
  while (n_active>0){
    if(ddd) Rcout << "  Active : " << active.transpose() << "\n";
    min_spsize = max_size;
    ii_mark = 0;
    
    for (InIter it1(active_sp); it1; ++it1){
      if (size_of_active_closure_space[ it1.index() ] < min_spsize){
	min_spsize = size_of_active_closure_space[ it1.index() ];
	ii_mark    = it1.index();
      }
    }  
    if(dd)Rcout << "  Node=" << ii_mark << " min_spsize=" << min_spsize << 
	    " Number of active=" << n_active << "\n";
    
    anbr_sp        = X.col(ii_mark).cwiseProduct(active_sp);  
    n_anbr        = anbr_sp.sum();
    
    if(ddd)Rcout << "   Active      : " << active_sp.transpose() ;
    if(ddd)Rcout << "   Active nbr's: " << anbr_sp.transpose();
    if(ddd)Rcout << "   Col         : " << X.col(ii_mark).transpose() ;
    
    if (n_anbr <= 1){ // No fill-in is necessary
      if(ddd) Rcout << "   node=" << ii_mark << 
		": case1: At most one active nb; we are done\n";
    } else { 
      if(ddd)Rcout << "   node=" << ii_mark << 
	       ": case2: More than one nbr; check if fill-in is necessary\n";
      n_nbr_obs=0;
      for (InIter it2(anbr_sp); it2; ++it2){
	for (InIter it3(anbr_sp); it3; ++it3){
	  n_nbr_obs += X.coeff(it2.index(), it3.index());
	  if (it2.index()!=it3.index()){
	    triplets.push_back(T(it2.index(), it3.index(), 1));
	    triplets.push_back(T(it3.index(), it2.index(), 1));
	  }
	}
      }
      fill.setFromTriplets(triplets.begin(), triplets.end());
      
      n_nbr_need = (int) fill.sum()/2;
      if(ddd) Rcout << "   node=" << ii_mark << ": n_anbr=" <<n_anbr<< 
		" n_nbr_obs="<<n_nbr_obs<< " n_nbr_need=" << n_nbr_need <<  "\n";
      triplets.clear();
      
      if( n_nbr_need == n_nbr_obs){ 
	if(ddd)Rcout << "   node=" << ii_mark << 
		 ": case2.1: Active boundary is complete; we are done\n";
      } else { 
	if(ddd)Rcout << "   node=" << ii_mark << 
		 ": case2.2: A fill in is needed\n";
	X += fill;
	for (i=0; i < nrX; i++){ 
	  for (SpMat::InnerIterator _j2(X, i); _j2; ++_j2){
	    X.coeffRef(_j2.row(),_j2.col())=1;
	  }
	}
      }
    } // end of case2
    
    
    // Update active vector
    active[ii_mark] = 0;
    active_sp        = active.sparseView();
    
    // anbr_sp : Holds the active nbrs of a given node: anbr(i)
    // for each j in anbr(i), the state space size needs revision 
    // because node i has been marked as passive

    if(ddd)Rcout<<"   Updating the size_of_active_closure_space\n" ;
    for (InIter _j3(anbr_sp); _j3; ++_j3){
      anbr2_sp = X.col( _j3.index() ).cwiseProduct(active_sp);
      //if(ddd) Rcout << "anbr2_sp : " << anbr2_sp.transpose();
      spsize     = L[ _j3.index() ];
      for (InIter _j2(anbr2_sp); _j2; ++_j2)
	spsize += L[ _j2.index() ];
      
      size_of_active_closure_space[_j3.index()] = spsize;      
    }
    if(ddd)Rcout << "   size_of_active_closure_space (updated): " << 
	     size_of_active_closure_space.transpose() << endl;
    
    n_active--;
  } 
  
  X.makeCompressed();
  return X;
}


// Coerce to sparse; triangulate and coerce back to dense again.
SEXP do_triangulateMAT_de ( SEXP XX_, SEXP OO_ ){

  NumericMatrix Xin(XX_);
  List dn = clone(List(Xin.attr("dimnames")));

  MapMatd Xd(as<MapMatd>(XX_));
  SpMat   out = internal_triangulateMAT_sp(Xd.sparseView(), OO_);
  
  Eigen::MatrixXd Xout( out );
  Xin = wrap(Xout);
  Xin.attr("dimnames") = dn;
  return Xin ;
}


SEXP do_triangulateMAT_sp ( SEXP XX_, SEXP OO_ ){
  S4 Xin(wrap(XX_));
  List dn = clone(List(Xin.slot("Dimnames")));

  SpMat   Xd(as<SpMat>(XX_));
  SpMat out = internal_triangulateMAT_sp(Xd, OO_);
  
  S4 Xout(wrap( out ));
  Xout.slot("Dimnames") = dn;
  return Xout;  
}


// [[Rcpp::export]]
SEXP triangulateMAT__ (SEXP adjmat_, SEXP nstates_){
  int type = TYPEOF(adjmat_) ;
  //Rf_PrintValue(wrap(type));
  switch( type ){
  case INTSXP  : return do_triangulateMAT_de(adjmat_, nstates_); // matrix - integer
  case REALSXP : return do_triangulateMAT_de(adjmat_, nstates_); // matrix - double
  case S4SXP   : return do_triangulateMAT_sp(adjmat_, nstates_); // dgCMatrix
  }
  return R_NilValue ;
}


// [[Rcpp::export]]
SEXP triang_mcwh_MAT__ (SEXP adjmat_, SEXP nstates_){
  return triangulateMAT__(adjmat_, nstates_);
}

















/*** R
library(Matrix)
library(gRbase)

m <- ug(~a:b+b:c+c:d+d:a, result="matrix")
M <- ug(~a:b+b:c+c:d+d:a, result="Matrix")

triangulateMAT_(M, rep(2, ncol(M)))
triangulateMAT_(m, rep(2, ncol(M)))

library(gRbase)
library(Rgraphviz)
uG <- ug(~a1:a5 + a2:a3:a5 + a2:a3:a6 + a6:a4, result="Matrix")
g <- triangulate(as(uG,"graphNEL"))
# g is equivalent to original graph which is as expected
#par(mfrow=c(2,2))
#plot(as(uG, "graphNEL"))
#plot(g)

## now we just rearrange the matrix
names <- sort(colnames(uG))
uG2 <- uG[names,names]
g2 <- as(uG2,"graphNEL")
h <- triangulate(as(uG[names,names],"graphNEL"))
## this graph is different from g (!!) it has one added edge (!!)
#plot(g2)
#plot(h)

triangulateMAT_(uG, rep(2, ncol(uG2)))
uG

triangulateMAT_(uG2, rep(2, ncol(uG2)))
uG2

 */

// library(microbenchmark)
// microbenchmark(
// triangulateMAT_(M, rep(2, ncol(M))),
// triangulateMAT_(m, rep(2, ncol(M))),
// triangulateMAT(M),
// triangulateMAT(m) )
