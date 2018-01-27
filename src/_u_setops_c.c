/* **********************************************************

   Code is used in gRbase
   
   Functions for set operations, most notably maxset and minset
   
   * maxset: Finds the maximal elements of a set, i.e. elements
   contained in other elements are discarded are marked with a 0,
   elements not contained in other sets are marked with a 1.
   
   * minset: Does the opposite of maxset
   
   * maxset and minset are used in removeRedundant in R
   
   * isin checks if an element (a vector) is contained in a set 
   (a list)
   
   * the subsetof-functions are internal.
   
   Created while in Bristol, 2007/2008
   Søren Højsgaard

** **********************************************************/

#include <string.h>
#include <Rdefines.h>

void C_subsetof1(char **setxsety, int *sx, int *ex, int *sy, int *ey, int *ans);
void C_subsetof2(char **qset, int *len_qset, 
		 char **setlist, int *start_set, int *end_set, int *ans);
void C_maxset(char **setlist, int *ends, int *nset, int *keepvec);
void C_minset(char **setlist, int *ends, int *nset, int *keepvec);

void C_isin(char **qset, int *len_qset, char **setlist, int *ends, int *len_setlist, int *keepvec);

/*
  Returns vector with entries 0 meaning that the elements are
  contained in other elements while 1 means that they are maximal
*/
void C_maxset(char **setlist, int *ends, int *nset, int *keepvec)
{
  
  int ii, jj;
  int ans;

  int starts[*nset];
  starts[0]=1;
  keepvec[0]=1;
  for (ii=1; ii<*nset; ii++){ 
    starts[ii] = ends[ii-1]+1;
    keepvec[ii] = 1;
  }

  for (ii=0; ii<*nset-1; ii++){ 
    if (keepvec[ii]==1){
      /* Rprintf("SET 1: %i %i %i\n", ii, starts[ii], ends[ii]);    */
      for (jj=ii+1; jj<*nset; jj++){
	if (keepvec[jj]==1){
	  /* Rprintf("  SET 2: %i %i %i\n", jj, starts[jj], ends[jj]);   */
	  C_subsetof1(setlist, &starts[jj], &ends[jj], &starts[ii], &ends[ii], &ans); 
	  /* Rprintf("  ans: %i \n", ans);    */
	  if (ans==1)
	    keepvec[jj] = 0;
	  else
	    keepvec[jj] = 1;
	}
      }
    }
  }
}


void C_minset(char **setlist, int *ends, int *nset, int *keepvec)
{
  
  int ii, jj;
  int ans;

  int starts[*nset];
  starts[0]=1;
  keepvec[0]=1;
  for (ii=1; ii<*nset; ii++){ 
    starts[ii] = ends[ii-1]+1;
    keepvec[ii] = 1;
  }

  for (ii=0; ii<*nset-1; ii++){ 
    if (keepvec[ii]==1){
      /*       Rprintf("SET 1: no: %i start: %i end: %i\n", ii, starts[ii], ends[ii]);     */
      for (jj=ii+1; jj<*nset; jj++){
	if (keepvec[jj]==1){
	  /* 	  Rprintf("  SET 2: %i %i %i\n", jj, starts[jj], ends[jj]);    */
	  C_subsetof1(setlist, &starts[ii], &ends[ii], &starts[jj], &ends[jj],  &ans); 
	  /* 	  Rprintf("  ans: %i \n", ans);     */
	  if (ans==1)
	    keepvec[jj] = 0;
	  else
	    keepvec[jj] = 1;
	}
      }
    }
  }
}



/* ************************************************************ */
/* ************************************************************ */


/* C_subsetof1: 
   Returns 0 if x is subset of y and 1 otherwise.
   - setxsety is c(setx, sety)
   - x and y are defined through the indices (sx,ex) and (sy,ey) in setxsety, 
   for example 1,3,4,7 says setx is setxsety[1:3] and sety is setxsety[4:7]
   - Notice that indexing follows R-standard, i.e. starts from 1      */
void C_subsetof1(char **setxsety, int *sx, int *ex, int *sy, int *ey, int *ans)
{
/*   Rprintf("    sx %i ex %i sy %i ey %i\n", *sx, *ex, *sy, *ey ); */
  int ii, jj, nmatch=0, nx;
  
  nx   = *ex-*sx+1;
  *ans = 0;
  
  for (ii=*sx-1; ii<*ex; ii++){
    /* Rprintf("    ii %i %s\n", ii, setxsety[ii]);  */
    for (jj=*sy-1; jj<*ey; jj++){
      /* Rprintf("      jj %i %s\n", jj, setxsety[jj]);  */
      if (!strcmp(setxsety[ii],setxsety[jj])){
	nmatch++;
	/* 	Rprintf("  match...%i %i %s %s %i\n", ii, jj, setxsety[ii], setxsety[jj], nmatch);   */
	break;
      }      
    }
    if (nmatch == nx){
      *ans = 1;
      break;
    }
  }
/*    Rprintf(" ans:  %i \n", *ans);   */
}




/* C_subsetof2:
   Returns 1 if qset (queryset) is contained in the subset of setlist defined by
   (start_set, end_set).
*/
void C_subsetof2(char **qset, int *len_qset, 
		 char **setlist, int *start_set, int *end_set, int *ans)
{
  /*   Rprintf("    start_set %i end_set %i sy %i ey %i\n", *start_set, *end_set, *sy, *ey ); */
  int ii, jj, nmatch=0; //, nx;
  
  //nx   = *end_set-*start_set+1;
  *ans = 0;
  
  for (ii=0; ii<*len_qset; ii++){
    /* Rprintf("    ii %i %s\n", ii, setlist[ii]);   */
    for (jj=*start_set-1; jj<*end_set; jj++){
      /* Rprintf("      jj %i %s\n", jj, setlist[jj]);   */
      if (!strcmp(qset[ii],setlist[jj])){
	nmatch++;
	/* Rprintf("  match...%i %i %s %s %i\n", ii, jj, setlist[ii], setlist[jj], nmatch);   */
	break;
      }      
    }
    if (nmatch == *len_qset){
      *ans = 1;
      break;
    }
  }
  /* Rprintf(" ans:  %i \n", *ans);    */
}

/* ************************************************************ */
/* ************************************************************ */


void C_isin(char **qset, int *len_qset, char **setlist, int *ends, int *len_setlist, int *keepvec)
{
  
  int ii, ans, starts[*len_setlist];

  starts[0]=1;
  keepvec[0]=1;
  for (ii=1; ii<*len_setlist; ii++){ 
    starts[ii]  = ends[ii-1]+1;
    keepvec[ii] = 1;
  }

  for (ii=0; ii<*len_setlist; ii++){ 
    C_subsetof2(qset, len_qset, setlist, &starts[ii], &ends[ii], &ans); 
    keepvec[ii] = ans;
  }
}













