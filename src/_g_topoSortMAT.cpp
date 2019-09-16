/* **************************************************
  
   Check matrix properties: 
   adjmat_ : a sparse or dense matrix

   topoSortMAT_ : Returns topological ordering of variables if graph is acyclic. 
   If cyclic, the first return value is -1
   
   isdagMAT_ (is square and topoSortMAT_ gives ordering)

   Author: Soren Hojsgaard, November 2014

**************************************************** */

#include <RcppEigen.h>
#include "_g_topoSortMAT.h"
//[[Rcpp::depends(RcppEigen)]]

using namespace Rcpp;

#ifndef BEGIN_RCPP
#define BEGIN_RCPP
#endif

#ifndef END_RCPP
#define END_RCPP
#endif

typedef Eigen::MappedSparseMatrix<double> MSpMat;
typedef Eigen::Map<Eigen::MatrixXd> MapMatd;
typedef Eigen::Map<Eigen::MatrixXi> MapMati;

// [[Rcpp::export]]
SEXP topo_sortMAT_ ( SEXP adjmat_ ){
  int type = TYPEOF(adjmat_) ;
  //Rf_PrintValue(wrap(type));
  switch( type ){
  case INTSXP  : return do_topoSortMAT_<MapMati>( adjmat_ );
  case REALSXP : return do_topoSortMAT_<MapMatd>( adjmat_ );
  case S4SXP   : return do_topoSortMAT_<MSpMat>( adjmat_ ); 
  }
  return R_NilValue ;
}





/*** R

library(gRbase)
dag11  <- gRbase::dag(~a + b:a + c:a:b + d:c:e + e:a + g:f)
dag11m <- gRbase::dag(~a + b:a + c:a:b + d:c:e + e:a + g:f, result="matrix")
dag11M <- gRbase::dag(~a + b:a + c:a:b + d:c:e + e:a + g:f, result="Matrix")

##topoSort(dag11)
## topoSort(dag11m)
##topoSort(dag11M)

library(microbenchmark)
microbenchmark(
#topoSortMAT_sp(dag11M),
#topoSortMAT_st(dag11m),
topoSortMAT_(dag11M),
topoSortMAT_(dag11m)
)


##C_topoSortMAT_st(dag11m)


*/




// //[[Rcpp::export]]
// SEXP topoSortMAT_sp ( SEXP XX_ ){
//   //   typedef Eigen::Map<Eigen::MatrixXi> MapMati;
//   //   const MapMati X(Rcpp::as<MapMati>(XX_));
//   typedef Eigen::MappedSparseMatrix<double> MSpMat;
//   const MSpMat   X(as<MSpMat>(XX_));

//   int ii, jj, kk=0, count=0, ll=0, flagsum=0;
//   int ncX(X.rows());
//   Eigen::VectorXi indegree(ncX);
//   Eigen::VectorXi flag(ncX);
//   Eigen::VectorXi ans(ncX);

//   for (ii = 0; ii < ncX; ii++) {
//     indegree[ii] = 0; flag[ii] = 0; ans[ii] = 0;
//   }
//   for (jj = 0; jj < ncX; jj++)
//     for (ii = 0; ii < ncX; ii++)
//       indegree[jj] = indegree[jj] +  X.coeff(ii,jj);

//   /*   Rcout<<"indegree: ";for (ii=0;ii<ncX;ii++) Rcout << indegree[ii]<<" " ; Rcout << std::endl;*/
//   /*   Rcout<<"flag    : ";for (ii=0;ii<ncX;ii++) Rcout << flag[ii]<<" " ; Rcout << std::endl;*/
//   while (count < ncX){
//     /* Rcout << "count=" << count << std::endl;*/
//     for (kk = 0; kk < ncX; kk++){
//       /* Rcout <<" kk="<<kk<<" indeg="<<indegree[kk]<<" flag="<<flag[kk] << std::endl;*/
//       if ((indegree[kk] == 0) && (flag[kk] == 0)){
// 	/*Rcout << "   no incomming:" << kk << std::endl;*/
// 	ans[ll++] = kk+1;
// 	flag[kk]  = 1;
// 	flagsum++;
// 	for (jj = 0; jj < ncX; jj++){
// 	  /*  Rcout <<"kk,jj="<<kk<<","<<jj<<" entry=" << X.coeff(kk,jj) << std::endl;*/
// 	  if (X.coeff(kk,jj) == 1){
// 	    indegree[jj]--;
// 	    /* Rcout <<" updating indegree at entry="<<jj<<std::endl;*/
// 	  }
// 	}
//       }
//       /* Rcout<<"indegree: ";for (ii=0;ii<ncX;ii++) Rcout << indegree[ii]<<" " ; Rcout << std::endl;	*/
//     }
//     if (flagsum==ncX)
//       break;
//     count++;
//     /* Rcout<<"flag    : ";for (ii=0;ii<ncX;ii++) Rcout << flag[ii]<<" " ; Rcout << std::endl;	*/
//   }
//   if (flagsum<ncX)
//     ans[0] = -1;
//   return(wrap(ans));
// }


// //[[Rcpp::export]]
// SEXP topoSortMAT_st ( SEXP XX_ ){
//   typedef Eigen::Map<Eigen::MatrixXd> MapMatd;
//   const MapMatd X(Rcpp::as<MapMatd>(XX_));
//   //typedef Eigen::MappedSparseMatrix<double> MSpMat;
//   //const MSpMat   X(as<MSpMat>(XX_));

//   int ii, jj, kk=0, count=0, ll=0, flagsum=0;
//   int ncX(X.rows());
//   Eigen::VectorXi indegree(ncX);
//   Eigen::VectorXi flag(ncX);
//   Eigen::VectorXi ans(ncX);

//   for (ii = 0; ii < ncX; ii++) {
//     indegree[ii] = 0; flag[ii] = 0; ans[ii] = 0;
//   }
//   for (jj = 0; jj < ncX; jj++)
//     for (ii = 0; ii < ncX; ii++)
//       indegree[jj] = indegree[jj] +  X.coeff(ii,jj);

//   /*   Rcout<<"indegree: ";for (ii=0;ii<ncX;ii++) Rcout << indegree[ii]<<" " ; Rcout << std::endl;*/
//   /*   Rcout<<"flag    : ";for (ii=0;ii<ncX;ii++) Rcout << flag[ii]<<" " ; Rcout << std::endl;*/
//   while (count < ncX){
//     /* Rcout << "count=" << count << std::endl;*/
//     for (kk = 0; kk < ncX; kk++){
//       /* Rcout <<" kk="<<kk<<" indeg="<<indegree[kk]<<" flag="<<flag[kk] << std::endl;*/
//       if ((indegree[kk] == 0) && (flag[kk] == 0)){
// 	/*Rcout << "   no incomming:" << kk << std::endl;*/
// 	ans[ll++] = kk+1;
// 	flag[kk]  = 1;
// 	flagsum++;
// 	for (jj = 0; jj < ncX; jj++){
// 	  /*  Rcout <<"kk,jj="<<kk<<","<<jj<<" entry=" << X.coeff(kk,jj) << std::endl;*/
// 	  if (X.coeff(kk,jj) == 1){
// 	    indegree[jj]--;
// 	    /* Rcout <<" updating indegree at entry="<<jj<<std::endl;*/
// 	  }
// 	}
//       }
//       /* Rcout<<"indegree: ";for (ii=0;ii<ncX;ii++) Rcout << indegree[ii]<<" " ; Rcout << std::endl;	*/
//     }
//     if (flagsum==ncX)
//       break;
//     count++;
//     /* Rcout<<"flag    : ";for (ii=0;ii<ncX;ii++) Rcout << flag[ii]<<" " ; Rcout << std::endl;	*/
//   }
//   if (flagsum<ncX)
//     ans[0] = -1;
//   return(wrap(ans));
// }



















