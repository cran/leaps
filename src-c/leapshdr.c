/* leapshdr.f -- translated by f2c (version 19950110).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Subroutine */ int makeqr_(np, nn, weights, txmat, yvec, d, rbar, thetab, 
	sserr, ier)
integer *np, *nn;
doublereal *weights, *txmat, *yvec, *d, *rbar, *thetab, *sserr;
integer *ier;
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i, nrbar;
    extern /* Subroutine */ int includ_();

/*     Calls INCLUD to construct Banachiewicz factorisation */


/*     local variables */
    /* Parameter adjustments */
    --thetab;
    --d;
    --yvec;
    --weights;
    --txmat;
    --rbar;

    /* Function Body */
    *ier = 0;
    nrbar = *np * (*np - 1) / 2;
    i__1 = *nn - 1;
    for (i = 0; i <= i__1; ++i) {
	includ_(np, &nrbar, &weights[i + 1], &txmat[i * *np + 1], &yvec[i + 1]
		, &d[1], &rbar[1], &thetab[1], sserr, ier);
	if (*ier != 0) {
	    return 0;
	}
/* L10: */
    }
    return 0;
} /* makeqr_ */

