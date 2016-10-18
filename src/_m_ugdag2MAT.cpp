/*
  ugList2xxx: Creates symmetric adjacency matrix from generators g1,...,gn.
  If g1 is (1,2,3) there will be 1s in entries (1,2),(1,3),(2,3),(2,1),(3,1),(3,2)
  (2,1) and (3,1)

  dagList2xxx: Creates adjacency matrix from generators g1,...,gn.
  If g1 is (1,2,3) then 1 is a parent child of 2 and 3 and there will be 1s in entry
  (2,1) and (3,1); hence g1 is in the "to-from" format

 */

#include <RcppEigen.h>
//[[Rcpp::interfaces(r,cpp)]]
//[[Rcpp::depends(RcppEigen)]]

#ifndef BEGIN_RCPP
#define BEGIN_RCPP
#endif

#ifndef END_RCPP
#define END_RCPP
#endif

using namespace Rcpp;

typedef Eigen::SparseMatrix<double> SpMat;
typedef Eigen::MappedSparseMatrix<double> MSpMat;
typedef Eigen::Map<Eigen::MatrixXd> MapMatd;
typedef Eigen::Triplet<double> T;

SpMat internal_dagList2dgCMatrix ( List LL, CharacterVector vn ){
  int n = vn.length();
  SpMat fill(n, n);
  std::vector<T> triplets;
  triplets.reserve(n * n);
  int ngen = LL.length(), genlen;
  CharacterVector gen;
  IntegerVector geni;
  
  for (int ii=0; ii<ngen; ii++){
    gen = LL[ii];
    geni = match(gen, vn);
    genlen = gen.length();
    //Rf_PrintValue(gen);
    if (genlen>1){
      for (int jj=1; jj<genlen; jj++){
		triplets.push_back(T(geni[jj]-1, geni[0]-1, 1));
      }
    }
  }
  fill.setFromTriplets(triplets.begin(), triplets.end());

  for (int ii=0; ii<fill.rows(); ii++){
    for (SpMat::InnerIterator inner_jj(fill, ii); inner_jj; ++inner_jj){
      fill.coeffRef(inner_jj.row(),inner_jj.col())=1;
    }
  }
  return fill;
}

SpMat internal_ugList2dgCMatrix ( List LL, CharacterVector vn ){
  int n = vn.length();
  SpMat fill(n, n);
  std::vector<T> triplets;
  triplets.reserve(n * n);
  int ngen = LL.length(), genlen;
  CharacterVector gen;
  IntegerVector geni;

  for (int ii=0; ii<ngen; ii++){
    gen = LL[ii];
    geni = match(gen, vn);
    genlen = gen.length();
    //Rf_PrintValue(gen);
    if (genlen>1){
      for (int jj=0; jj<genlen-1; jj++){
		for (int kk=jj+1; kk<genlen; kk++){
		  triplets.push_back(T(geni[jj]-1, geni[kk]-1, 1));
		  triplets.push_back(T(geni[kk]-1, geni[jj]-1, 1));
		}
      }
    }
  }
  fill.setFromTriplets(triplets.begin(), triplets.end());
  
  for (int ii=0; ii<fill.rows(); ii++){
    for (SpMat::InnerIterator inner_jj(fill, ii); inner_jj; ++inner_jj){
      fill.coeffRef(inner_jj.row(),inner_jj.col())=1;
    }
  }
  return fill;
}

SEXP inline setnames_sp(SpMat AA, CharacterVector vn) {
  S4 Xout(wrap(AA));
  List dn = List::create(vn, vn);
  Xout.slot("Dimnames") = dn;
  return Xout;
}

//[[Rcpp::export]]
SEXP dagList2dgCMatrix ( List LL, CharacterVector vn ){
  SpMat AA = internal_dagList2dgCMatrix(LL, vn);
  return setnames_sp(AA, vn);
}

//[[Rcpp::export]]
SEXP ugList2dgCMatrix ( List LL, CharacterVector vn ){
  SpMat AA = internal_ugList2dgCMatrix(LL, vn);
  return setnames_sp(AA, vn);
}

SEXP inline setnames_de( SpMat AA, CharacterVector vn) {
  Eigen::MatrixXd dMat(AA);
  NumericMatrix Xout(wrap(dMat));
  List dn = List::create(vn, vn);
  Xout.attr("dimnames") = dn;
  return Xout;
}

//[[Rcpp::export]]
SEXP dagList2matrix ( List LL, CharacterVector vn ){
  SpMat AA = internal_dagList2dgCMatrix(LL, vn);
  return setnames_de(AA, vn);
}


//[[Rcpp::export]]
SEXP ugList2matrix ( List LL, CharacterVector vn ){
  SpMat AA = internal_ugList2dgCMatrix(LL, vn);
  return setnames_de(AA, vn);
}

//[[Rcpp::export]]
List adjList2tfList(List LL){
  CharacterVector NN = LL.names();
  int n = LL.length() ;
  int mm=0;
  for (int ii=0; ii<n; ii++){
    mm += as<CharacterVector>(LL[ii]).size();
  }

  List out(mm);
  for (int ii=0, mm=0; ii<n; ii++){
    String parent = NN[ii];
    CharacterVector childVec = LL[ii];
    for (int jj=0; jj<childVec.length(); jj++){
      out[mm++] = CharacterVector::create( childVec[jj], parent );
    }
  }
  return out;
}

//[[Rcpp::export]]
List adjList2ftList(List LL){
  CharacterVector NN = LL.names();
  int n = LL.length() ;
  int mm=0;
  for (int ii=0; ii<n; ii++){
    mm += as<CharacterVector>(LL[ii]).size();
  }

  List out(mm);
  for (int ii=0, mm=0; ii<n; ii++){
    String parent = NN[ii];
    CharacterVector childVec = LL[ii];
    for (int jj=0; jj<childVec.length(); jj++){
      out[mm++] = CharacterVector::create( parent, childVec[jj] );
    }
  }
  return out;
}


//[[Rcpp::export]]
CharacterMatrix adjList2ftM(List LL){
  CharacterVector NN = LL.names();
  int n = LL.length() ;
  int mm=0;
  for (int ii=0; ii<n; ii++){
    mm += as<CharacterVector>(LL[ii]).size();
  }

  CharacterMatrix out(mm, 2);

  for (int ii=0, mm=0; ii<n; ii++){
    String parent = NN[ii];
    CharacterVector childVec = LL[ii];
    for (int jj=0; jj<childVec.length(); jj++){
      out(mm++,_) = CharacterVector::create( parent, childVec[jj] );
    }
  }
  return out;
}

//[[Rcpp::export]]
CharacterMatrix adjList2tfM(List LL){
  CharacterVector NN = LL.names();
  int n = LL.length() ;
  int mm=0;
  for (int ii=0; ii<n; ii++){
    mm += as<CharacterVector>(LL[ii]).size();
  }

  CharacterMatrix out(mm, 2);

  for (int ii=0, mm=0; ii<n; ii++){
    String parent = NN[ii];
    CharacterVector childVec = LL[ii];
    for (int jj=0; jj<childVec.length(); jj++){
      out(mm++,_) = CharacterVector::create( childVec[jj], parent );
    }
  }
  return out;
}


//[[Rcpp::export]]
SEXP adjList2matrix(List LL){
  List tfList = adjList2tfList( LL );
  CharacterVector vn = LL.names();
  return dagList2matrix(tfList, vn);
}

//[[Rcpp::export]]
SEXP adjList2dgCMatrix(List LL){
  List tfList = adjList2tfList( LL );
  CharacterVector vn = LL.names();
  return dagList2dgCMatrix(tfList, vn);
}



/*** R

g <- list(c("A","B"), c("B","C"))
vn <- unique(unlist(g))

ugList2matrix(g, vn)

data(cad1, package="gRbase")
library(gRim)
use <- c(1,2,3,9:14)
cad1 <- cad1[,use]
s <- dmod(~.^., data=cad1)


ugList2matrix(terms(s), unique(unlist(terms(s))))
M<-ugList2dgCMatrix(terms(s), unique(unlist(terms(s))));M






*/


// //[[Rcpp::export]]
// SEXP do_dgCMatrix2matrix ( SEXP XX_ ){
//   S4   DD(wrap(XX_));
//   List dn = clone(List(DD.slot("Dimnames")));
//   MSpMat X(as<MSpMat>(XX_));
//   Eigen::MatrixXd dMat(X);
//   NumericMatrix Xout(wrap(dMat));
//   Xout.attr("dimnames") = dn;
//   return Xout;
// }
