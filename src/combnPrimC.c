/* combnPrim.f -- translated by f2c (version 20060506).

   You must link the resulting object file with libf2c:
   on Microsoft Windows system, link with libf2c.lib;
   on Linux or Unix systems, link with .../path/to/libf2c.a -lm
   or, if you install libf2c.a in a standard place, with -lf2c -lm
   -- in that order, at the end of the command line, as in
   cc *.o -lf2c -lm
   Source for libf2c is in /netlib/f2c/libf2c.zip, e.g.,
   
   http://www.netlib.org/f2c/libf2c.zip
*/

#include "f2c.h"

/* Subroutine */ int sla_combn__(int *nsel, int *ncand, int *list, int *j)
{
    /* System generated locals */
  integer i__1;

  /* Local variables */
  static integer i__, m;
  static logical more;
  static integer nmax, listi;

/* + */
/*     - - - - - - */
/*      C O M B N */
/*     - - - - - - */

/*  Generate the next combination, a subset of a specified size chosen */
/*  from a specified number of items. */

/*  Given: */
/*     NSEL     i        number of items (subset size) */
/*     NCAND    i        number of candidates (set size) */

/*  Given and returned: */
/*     LIST     i(NSEL)  latest combination, LIST(1)=0 to initialize */

/*  Returned: */
/*     J        i        status: -1 = illegal NSEL or NCAND */
/*                                0 = OK */
/*                               +1 = no more combinations available */

/*  Notes: */

/*  1) NSEL and NCAND must both be at least 1, and NSEL must be less */
/*     than or equal to NCAND. */

/*  2) This routine returns, in the LIST array, a subset of NSEL integers */
/*     chosen from the range 1 to NCAND inclusive, in ascending order. */
/*     Before calling the routine for the first time, the caller must set */
/*     the first element of the LIST array to zero (any value less than 1 */
/*     will do) to cause initialization. */

/*  2) The first combination to be generated is: */

/*        LIST(1)=1, LIST(2)=2, ..., LIST(NSEL)=NSEL */

/*     This is also the combination returned for the "finished" (J=1) */
/*     case. */

/*     The final permutation to be generated is: */

/*        LIST(1)=NCAND, LIST(2)=NCAND-1, ..., LIST(NSEL)=NCAND-NSEL+1 */

/*  3) If the "finished" (J=1) status is ignored, the routine */
/*     continues to deliver combinations, the pattern repeating */
/*     every NCAND!/(NSEL!*(NCAND-NSEL)!) calls. */

/*  4) The algorithm is by R.F.Warren-Smith (private communication). */

/*  P.T.Wallace   Starlink   25 August 1999 */

/*  Copyright (C) 1999 Rutherford Appleton Laboratory */

/*  License: */
/*    This program is free software; you can redistribute it and/or modify */
/*    it under the terms of the GNU General Public License as published by */
/*    the Free Software Foundation; either version 2 of the License, or */
/*    (at your option) any later version. */

/*    This program is distributed in the hope that it will be useful, */
/*    but WITHOUT ANY WARRANTY; without even the implied warranty of */
/*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the */
/*    GNU General Public License for more details. */

/*    You should have received a copy of the GNU General Public License */
/*    along with this program (see SLA_CONDITIONS); if not, write to the */
/*    Free Software Foundation, Inc., 59 Temple Place, Suite 330, */
/*    Boston, MA  02111-1307  USA */

/* - */
/*  Validate, and set status. */
    /* Parameter adjustments */
    --list;

    /* Function Body */
    if (*nsel < 1 || *ncand < 1 || *nsel > *ncand) {
			*j = -1;
			goto L9999;
    } else {
			*j = 0;
    }
		/*  Just starting? */
    if (list[1] < 1) {
			/*     Yes: return 1,2,3... */
			i__1 = *nsel;
			for (i__ = 1; i__ <= i__1; ++i__) {
				list[i__] = i__;
			}
    } else {
			/*     No: find the first selection that we can increment. */
			/*     Start with the first list item. */
			i__ = 1;
			/*     Loop. */
			more = TRUE_;
			while(more) {
				/*        Current list item. */
				listi = list[i__];
				/*        Is this the final list item? */
				if (i__ >= *nsel) {
					/*           Yes:  comparison value is number of candidates plus one. */
					nmax = *ncand + 1;
				} else {
					/*           No:  comparison value is next list item. */
					nmax = list[i__ + 1];
				}
				/*        Can the current item be incremented? */
				if (nmax - listi > 1) {
					/*           Yes:  increment it. */
					list[i__] = listi + 1;
					/*           Reinitialize the preceding items. */
					i__1 = i__ - 1;
					for (m = 1; m <= i__1; ++m) {
						list[m] = m;
					}
					/*           Break. */
					more = FALSE_;
				} else {
					/*           Can't increment the current item:  is it the final one? */
					if (i__ >= *nsel) {
						/*              Yes:  set the status. */
						*j = 1;
						/*              Restart the sequence. */
						i__1 = *nsel;
						for (i__ = 1; i__ <= i__1; ++i__) {
							list[i__] = i__;
						}
						/*              Break. */
						more = FALSE_;
					} else {
						/*              No:  next list item. */
						++i__;
					}
				}
			}
    }
 L9999:
    return 0;
} /* sla_combn__ */

