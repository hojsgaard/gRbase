#include <RcppEigen.h>
//[[Rcpp::depends(RcppEigen)]]

using namespace Rcpp;

template <typename TT>
SEXP do_topoSortMAT_ ( SEXP X_ ){
  const TT X(as<TT>(X_));
  
  int i, j, k=0, count=0, ll=0, flagsum=0;
  int ncX(X.rows());
  Eigen::VectorXi indegree(ncX), flag(ncX), ans(ncX);
  
  for (i = 0; i < ncX; i++) {
    indegree[i] = 0; flag[i] = 0; ans[i] = 0;
  }
  for (j = 0; j < ncX; j++)
    for (i = 0; i < ncX; i++)
      indegree[j] = indegree[j] +  X.coeff(i,j);
  
  /*   Rcout<<"indegree: ";for (i=0;i<ncX;i++) Rcout << indegree[i]<<  " " ; Rcout << std::endl;*/
  /*   Rcout<<"flag    : ";for (i=0;i<ncX;i++) Rcout << flag[i]<<      " " ; Rcout << std::endl;*/
  while (count < ncX){
    /* Rcout << "count=" << count << std::endl;*/
    for (k = 0; k < ncX; k++){
      /* Rcout <<" k="<<k<<" indeg="<<indegree[k]<<" flag="<<flag[k] << std::endl;*/
      if ( (indegree[k] == 0) && (flag[k] == 0) ){
				/*Rcout << "   no incomming:" << k << std::endl;*/
				ans[ll++] = k+1;
				flag[k]   = 1;
				flagsum++;
				for (j = 0; j < ncX; j++){
					/*  Rcout <<"k,j="<<k<<","<<j<<" entry=" << X.coeff(k,j) << std::endl;*/
					if (X.coeff(k, j) == 1){
						indegree[j]--;
						/* Rcout <<" updating indegree at entry="<<j<<std::endl;*/
					}
				}
      }
      /* Rcout<<"indegree: ";for (i=0;i<ncX;i++) Rcout << indegree[i]<< " " ; Rcout << std::endl;	*/
    }
    if (flagsum==ncX)
      break;
    count++;
    /* Rcout<<"flag    : ";for (i=0;i<ncX;i++) Rcout << flag[i]<< " " ; Rcout << std::endl;	*/
  }
  if (flagsum<ncX)
    ans[0] = -1;

  return( wrap(ans) );
}
