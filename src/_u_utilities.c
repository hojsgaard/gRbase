/* **************************************************************
 C code for ...
 Søren Højsgaard

 FIXME: NOT SURE IF THIS IS EVER USED?
** *************************************************************/

#include <string.h>
#include <stdlib.h>
#include <Rdefines.h>
#include "_utils_print.h"

/* ************************************************************* */


#define getDims(A) INTEGER(coerceVector(getAttrib(A, R_DimSymbol), INTSXP))

SEXP R_rowSums(SEXP X)
{
  int    *xdims, ii, jj, nrx, ncx;
  double *xptr, *ansptr, sum;
  SEXP ans;
  xdims = getDims(X);
  nrx = xdims[0];
  ncx = xdims[1];

  PROTECT(X = coerceVector(X, REALSXP));
  xptr  = REAL(X);
  PROTECT(ans = allocVector(REALSXP, nrx));
  ansptr = REAL(ans);

  for (ii=0; ii<nrx; ii++){
    sum=0;
    for (jj=0; jj<ncx; jj++){
      sum=sum+xptr[ii+nrx*jj];
    }
    ansptr[ii] = sum;
  }

  UNPROTECT(2);
  return(ans);
}


SEXP R_colSums(SEXP X)
{
  int    *xdims, ii, jj, nrx, ncx;
  double *xptr, *ansptr, sum;
  SEXP ans;
  xdims = getDims(X);
  nrx = xdims[0];
  ncx = xdims[1];

  PROTECT(X = coerceVector(X, REALSXP));
  xptr  = REAL(X);
  PROTECT(ans = allocVector(REALSXP, ncx));
  ansptr = REAL(ans);

  for (jj=0; jj<ncx; jj++){
    sum=0;
    for (ii=0; ii<nrx; ii++){
      sum=sum+xptr[ii+nrx*jj];
    }
    ansptr[jj] = sum;
  }

  UNPROTECT(2);
  return(ans);
}

SEXP R_colwiseProd(SEXP V, SEXP X)
{

  int    *xdims, nrx, ncx, ii, jj, kk, len_V;
  double *xptr, *vptr;
  xdims = getDims(X);
  nrx = xdims[0];
  ncx = xdims[1];
  len_V = length(V);

  PROTECT(X = coerceVector(X, REALSXP));
  xptr  = REAL(X);

  PROTECT(V = coerceVector(V, REALSXP));
  vptr = REAL(V);

  double *ansptr;
  SEXP ans;
  PROTECT(ans = allocMatrix(REALSXP, nrx, ncx));
  ansptr = REAL(ans);
  
  kk = 0;
  for (jj=0; jj<ncx; jj++){
    //Rprintf("kk=%i len_V=%i\n", kk, len_V);
    for (ii=0; ii<nrx; ii++){
      ansptr[ii+nrx*jj] = vptr[kk]*xptr[ii+nrx*jj];
    }
    kk++;
    if ((kk==len_V)){
      //Rprintf("HERE: kk=%i len_V=%i\n", kk, len_V);
      kk=0;
    }
  }

  UNPROTECT(3);
  return(ans);
}



