/* **************************************************************
 C code for operations on table entries
 Used in the gRim package
 Søren Højsgaard
** *************************************************************/

#include <string.h>
#include <stdlib.h>
#include <Rdefines.h>
#include <R.h>
#include "_utils_print.h"

/* ************************************************************* */

void C_cell2entry(int *cell, int *adim, int *ndim, int *ans);
void C_cell2entry2(int *cell, int *plevels, int *ndim, int *ans);

/* ********* */

void C_nextCell(int *cell, int *adim, int *ndim);
SEXP R_nextCell(SEXP _cell, SEXP _adim);

/* ********* */

void C_nextCellSlice(int *cell, int *adim, int *ndim, 
		    int *slice_set, int *slice_len);

void C_nextCellSlice_indic(int *cell, int *adim, int *ndim, 
			  int *marg_indic, int *slice_len); 
/* ********* */

SEXP R_slice2entry(SEXP _slicecell, SEXP _sliceset, SEXP _adim);

void C_slice2entry(int *cell, int *slice_set, int *adim, 
		      int *ndim, int *slice_len, 
		      int *ans, int *ans_len);

void C_slice2entryPrim(int *cell, int *plevels, int *adim, 
			  int *ndim, int *marg_indic, int *slice_len, 
			  int *ans, int *ans_len);

/* ********* */

int* make_marg_indic(int *adim, int *slice_set, int *slice_len);
int* make_prod_fact(int *adim, int *ndim);

/* ********* */

void C_getCellNumber(int *cell, int *perm, int *pvec, int *len, int *ans);

/* ********* */

SEXP R_permuteCellEntries(SEXP _perm, SEXP _adim);
void C_permuteCellEntries(int *perm, int *adim, int *ndim, int *entry_new, int *len_entry);

/* ********* */

/* ************************************************************* */



void C_cell2entry(int *cell, int *adim, int *ndim, int *ans){
  int ii, ss=1, res=cell[0]-1;
  for (ii=1;ii<*ndim;ii++){
    ss = ss * adim[ii-1];
    res = res + (cell[ii]-1) * ss;
  }
  *ans = res + 1;
}

void C_cell2entry2(int *cell, int *plevels, int *ndim, int *ans){
  int ii;
  for (ii=0; ii<*ndim; ii++){
    *ans = *ans + (cell[ii] -1) * plevels[ii];
  }
  *ans = *ans + 1;
}

void C_nextCell(int *cell, int *adim, int *ndim){
  int jj, n_init=0;  
  for (jj=0; jj<*ndim; jj++){
    //Rprintf("jj %i\n", jj);  
    if (cell[jj] < adim[jj]){
      cell[jj] = cell[jj] + 1;
      break;
    } else {
      cell[jj] = 1;
      n_init++;
    }
  }
/*   if (n_init == *ndim){ */
/*     cell[0] = -1; */
/*   } */

} 


SEXP R_nextCell(SEXP _cell, SEXP _adim){
  
  int *Cptr, *Fptr, *ansptr, ndim;
  SEXP ans;
  
  PROTECT(_cell = coerceVector(_cell, INTSXP));
  PROTECT(_adim = coerceVector(_adim, INTSXP));
  
  ndim    = length(_cell);
  Cptr  = INTEGER(_cell);
  Fptr  = INTEGER(_adim);
  
  PROTECT(ans = allocVector(INTSXP, ndim));
  ansptr = INTEGER(ans);
  
  C_nextCell(Cptr, Fptr, &ndim);
  
  for (int ii=0; ii<ndim; ii++)
    ansptr[ii] = Cptr[ii];
  
  UNPROTECT(3);
  return(ans);
}



int* make_marg_indic(int *adim, int *slice_set, int *slice_len){
  int ii, *marg_indic;
  marg_indic    = (int *) R_alloc(*adim, sizeof(int)); 
  for (ii=0; ii<*adim; ii++){
    marg_indic[ii] = 0;
  } 
  for (ii=0; ii<*slice_len; ii++){
    //Rprintf(" ii %i  slice_set[ii] %i \n", ii, slice_set[ii]);
    marg_indic[slice_set[ii]-1] = 1;
  } 
  return(marg_indic);
}

void C_nextCellSlice_indic(int *cell, int *adim, int *ndim, 
			  int *marg_indic, int *slice_len)
{

  int jj, n_init=0, not_slice_len=0;

  not_slice_len = *ndim - *slice_len;

  for (jj=0; jj<*ndim; jj++){
    if (marg_indic[jj]==0){
      //Rprintf("jj %i\n", jj);  
      if (cell[jj] < adim[jj]){
	//Rprintf("kkkkk\n");
	cell[jj] = cell[jj] + 1;
	break;
      } else {
	cell[jj] = 1;
	n_init++;
      } 
    }
  }
  //Rprintf("n_init=%d\n", n_init);
  if (n_init == not_slice_len){
    cell[0] = -1;
  }
}

void C_nextCellSlice(int *cell, int *adim, int *ndim, 
		    int *slice_set, int *slice_len){
  int *marg_indic;
  marg_indic = make_marg_indic(ndim, slice_set, slice_len);
  C_nextCellSlice_indic(cell, adim, ndim, marg_indic, slice_len);
} 

int* make_prod_fact(int *adim, int *ndim){
  int ii, *plevels;
  plevels  = (int *) R_alloc(*ndim, sizeof(int)); 
  plevels[0]=1;
  if (*ndim > 1){
    for (ii=1; ii<*ndim; ii++){
      plevels[ii] =  (adim[ii-1]*plevels[ii-1]);
    }
  }
  return(plevels);
}

void C_slice2entryPrim(int *cell, int *plevels, int *adim, 
			  int *ndim, int *marg_indic, int *slice_len, 
			  int *ans, int *ans_len){
  
  int ii, entry;
  for (ii=0;ii<*ans_len;ii++)
    {
      entry = 0;
      C_cell2entry2(cell, plevels, ndim, &entry); 
      //Rprintf(" ii: %i entry: %i cell:", ii, entry); printveci(cell, adim); Rprintf("\n");
      //C_nextCellSlice(cell, adim, adim, slice_set, slice_len);
      C_nextCellSlice_indic(cell, adim, ndim, marg_indic, slice_len); // This is faster...
      ans[ii] = entry;
    } 
  //Rprintf("\n");
}

void C_slice2entry(int *cell, int *slice_set, int *adim, 
		      int *ndim, int *slice_len, 
		      int *ans, int *ans_len){

  int *plevels, *marg_indic;
  marg_indic = make_marg_indic(ndim, slice_set, slice_len);
  plevels    = make_prod_fact(adim, ndim);

/*   Rprintf("C_slice2entry:\n"); */
/*   Rprintf("ans_len=%d\n", *ans_len); */
/*   printveci(marg_indic, adim); Rprintf("\n"); */
/*   printveci(plevels,    adim); Rprintf("\n"); */
/*   printveci(cell,       adim); Rprintf("\n"); */

  C_slice2entryPrim(cell, plevels, adim, adim, marg_indic, slice_len, ans, ans_len);
}


SEXP R_slice2entry(SEXP _slicecell, SEXP _sliceset, SEXP _adim)
{
  
  int ii, *marg_cell, *slice_set, *adim, *ansptr, *tmpptr, *cell, ndim, slice_len, ans_len=1;
  SEXP ans, tmp ;

  PROTECT(_slicecell = coerceVector(_slicecell, INTSXP));
  PROTECT(_sliceset  = coerceVector(_sliceset,  INTSXP));
  PROTECT(_adim  = coerceVector(_adim,  INTSXP));

  ndim   = length(_adim);
  slice_len   = length(_slicecell);
  marg_cell  = INTEGER(_slicecell);
  slice_set   = INTEGER(_sliceset);
  adim    = INTEGER(_adim);

  int *plevels, *marg_indic;
  // marg_indic: [0,1,1,0,0] if entry 2,3 are fixed
  marg_indic = make_marg_indic(&ndim, slice_set, &slice_len);
  plevels    = make_prod_fact(adim, &ndim);

  // Need to know dimension of result
  PROTECT(tmp = allocVector(INTSXP, ndim));
  tmpptr = INTEGER(tmp);
  for (ii=0; ii<ndim; ii++)
    tmpptr[ii] = adim[ii];
  //  printveci(tmpptr, &adim); Rprintf("\n");
  for (ii=0;ii<slice_len;ii++)
    tmpptr[slice_set[ii]-1] = 1;
  //printveci(tmpptr, &adim); Rprintf("<- tmpptr\n");
  for (ii=0; ii< ndim; ii++)
    ans_len = ans_len * tmpptr[ii];
  PROTECT(ans = allocVector(INTSXP, ans_len));
  ansptr = INTEGER(ans);
  
  // Create initial cell
  cell    = (int *) R_alloc(ndim, sizeof(int)); 
  for (ii=0; ii<ndim; ii++)
    cell[ii] = 1;
  for (ii=0; ii<slice_len; ii++)
    cell[slice_set[ii]-1] = marg_cell[ii];

/*   Rprintf("R_slice2entry:\n"); */
/*   Rprintf("ans_len=%d\n", ans_len); */
/*   printveci(marg_indic, &adim); Rprintf("\n"); */
/*   printveci(plevels, &adim); Rprintf("\n"); */
/*   printveci(cell, &adim); Rprintf("\n"); */

  //  printveci(ansptr, &ans_len);  
  C_slice2entryPrim(cell, plevels, adim, &ndim, marg_indic, &slice_len, ansptr, &ans_len);
  // printveci(ansptr, &ans_len);

  UNPROTECT(5);
  return(ans);
}


/* 
## ###############################################################
## getCellNumber
## -------------
## A table is defined over factors, e.g. A,B,C with levels
## |A|,|B|,|C|. First factor in the table varies fastest.
## To each cell there is an entry number 1,2,..K
## where K=|A||B||C|. (Just think of the numbers 1,2,..K
## being stored in the table.
##
## A permuted table can have factors B,A,C. The function
## returns the entry of a cell in the permuted table.
##
## Example: All factors are binary. The cell (1,2,2) has
## entry 7 in the original A-B-C-table. The B-A-C-table is
## formed by permuting the factors as 2,1,3. In the new
## the cell (1,2,2) (in the old table) has entry 6.
##
## Arguments:
## 'nlev': the levels of the factors in the original table.
## 'perm': match(c("B","A","C"), c("A","B","C")) -> 2,1,3
## 'cell': (1,2,2) is a cell in the original table
## Output: The entry of the cell in the new table
## ###############################################################
*/ 


void C_getCellNumber(int *cell, int *perm, int *pvec, int *ndim, int *cell_number)
{  
  *cell_number = 0;
  for (int ii=0; ii<*ndim; ii++){
    *cell_number = *cell_number + pvec[ii] * (cell[perm[ii]-1] - 1);
  }
  *cell_number = *cell_number+1;
}



/* 
## Permute cell entries 
*/

void C_permuteCellEntries(int *perm, int *adim, int *ndim, int *entry_new, int *len_entry)
{ 

  int *cell, *pvec, *adim_new, ii, ans=0;

  cell  = (int *) R_alloc(*ndim, sizeof(int));
  for (ii=0; ii<*ndim; ii++) cell[ii] = 1;

  pvec  = (int *) R_alloc(*ndim, sizeof(int));
  pvec[0] = 1;
  if (*ndim>1){
    for (ii=1; ii<*ndim; ii++)
      //pvec[ii] = (int) pvec[ii-1]*adim[ii];
      pvec[ii] = pvec[ii-1]*adim[ii-1];
  }
  //printveci(pvec, ndim);Rprintf("\n");

  adim_new  = (int *) R_alloc(*ndim, sizeof(int));
  for (ii=0; ii<*ndim;ii++)
    adim_new[ii] = adim[perm[ii]-1];

  for (ii=0; ii<*len_entry; ii++){
    C_getCellNumber(cell, perm, pvec, ndim, &ans);
    //Rprintf("cell:\n");   printveci(cell, ndim);
    //Rprintf("ans %i\n", ans);
    entry_new[ii] = ans;
    C_nextCell(cell, adim_new, ndim);
  }
  //printveci(entry_new, len_entry);
}

void C_permuteCellEntries2(int *perm, int *adim, int *ndim, int *entry_new, int *len_entry, int *pvec)
{ 

  int *cell,  *adim_new, ii, ans=0;

  cell  = (int *) R_alloc(*ndim, sizeof(int));
  for (ii=0; ii<*ndim; cell[ii++] = 1);

  adim_new  = (int *) R_alloc(*ndim, sizeof(int));
  for (ii=0; ii<*ndim;ii++)
    adim_new[ii] = adim[perm[ii]-1];

  for (ii=0; ii<*len_entry; ii++){
    C_getCellNumber(cell, perm, pvec, ndim, &ans);
    entry_new[ii] = ans;
    C_nextCell(cell, adim_new, ndim);
  }
}


SEXP R_permuteCellEntries(SEXP _perm, SEXP _adim)
{

  int ndim, *ansptr, *perm_ptr, *adim_ptr, ii, n_entries=1;
  SEXP ans;

  PROTECT(_perm = coerceVector(_perm, INTSXP));
  PROTECT(_adim = coerceVector(_adim, INTSXP));

  ndim = length(_adim);
  perm_ptr = INTEGER(_perm);
  adim_ptr = INTEGER(_adim);

  for (ii=0; ii<ndim; ii++){
    n_entries = n_entries * adim_ptr[ii];
  }

  PROTECT(ans = allocVector(INTSXP, n_entries));
  ansptr = INTEGER(ans);

  C_permuteCellEntries(perm_ptr, adim_ptr, &ndim, ansptr, &n_entries);

  UNPROTECT(3);
  return(ans);
}



