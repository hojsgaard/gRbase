/*
  Triangulation by specification of elimination order.

	Input: Sparse N x N adjacency matrix and elimination order (given as a
	permutation of 0, 1, N-1)

	Output: Sparse N x N matrix adjacency for triangulated graph.

	Details:

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

// [[Rcpp::export]]
SEXP do_triangulate_elo ( SEXP X_, SEXP ELO_ ){

  SpMat   X(as<SpMat>(X_));
  Eigen::VectorXd ELO(as<Eigen::VectorXd>(ELO_));
  bool pp=false;
  int i, v, nrX(X.rows()), n_anbr, n_nbr_obs = -1;
  //  if (X.cols() != nrX) throw std::invalid_argument("Sparse matrix X must be square");

  // Rcout << "--- Entry: --- " << std::endl;
  // Rcout << "X: " << std::flush; Rf_PrintValue(wrap(X));
  // Rcout << "ELO: " << std::flush; Rf_PrintValue(wrap(ELO));
  
  Eigen::VectorXi active(nrX);
  SpVec active_(nrX), anbr_(nrX);
  SpMat fill(nrX, nrX);
  active.setOnes();
  active_ = active.sparseView();

  typedef Eigen::Triplet<double> T;
  std::vector<T> triplets;
  int triplet_size = X.nonZeros() * 2;
  triplets.reserve(triplet_size);

  for (i = 0; i < nrX; i++){
    fill.setZero();
    //if (pp) {Rcout << "fill " << std::flush; Rf_PrintValue(wrap(fill));}
    //Rcout << "fill (before): " << fill << std::flush;
    triplets.resize(triplet_size);    
    v = ELO[i];
    if (pp) Rcout << "node: " << v << std::flush;
    if (pp) Rcout << "active_: " << active_ << std::flush;
    if (pp) Rcout << "X.col: " << X.col(v) << std::flush;

    anbr_ = X.col( v ).cwiseProduct( active_ );
    n_anbr  = anbr_.sum();
    if (pp) Rcout << anbr_ << "n_anbr: " << n_anbr << std::flush;
    
    if (n_anbr > 1){
      n_nbr_obs = 0;
      for (InIter it2(anbr_); it2; ++it2){
  	for (InIter it3(anbr_); it3; ++it3){
  	  if (it2.index() > it3.index()){
  	    int edge = X.coeff(it2.index(), it3.index());
  	    n_nbr_obs += edge;
  	    //if (pp) Rcout << "it2: " << it2.index() << " it3: " << it3.index() <<
  	    // 					" X: " << X.coeff(it2.index(), it3.index()) << std::flush;
  	    if (edge == 0){
  	      triplets.push_back(T(it2.index(), it3.index(), 1-edge));
  	      //if (pp) Rcout << "push_back " << it2.index() << ", " << it3.index() << ", 1" << std::flush;
  	      triplets.push_back(T(it3.index(), it2.index(), 1-edge));
  	      //if (pp) Rcout << "push_back " << it3.index() << ", " << it2.index() << ", 1" << std::flush;
  	    }
  	  }
  	}
      }
      if (pp) Rcout << "n_nbr_obs: " << n_nbr_obs << std::flush;
      fill.setFromTriplets(triplets.begin(), triplets.end());
      if (pp) Rcout << "fill: " << fill << std::flush;
      if (pp) Rf_PrintValue(wrap(fill));
      X += fill;
      if (pp) Rcout << "X: " << X << std::flush;
    }
    active_.coeffRef(v) = 0;
    active_.prune(0);
    if (pp) Rf_PrintValue(wrap(X));
    //print(wrap(anbr_));

  }

  X.prune(0.0);
  return( wrap(X) );
}

