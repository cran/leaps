/* leaps.f -- translated by f2c (version 19950110).
   You must link the resulting object file with the libraries:
	-lf2c -lm   (in that order)
*/

#include "f2c.h"

/* Table of constant values */

static integer c__9 = 9;
static integer c__1 = 1;
static integer c__3 = 3;

/*      PROGRAM START */

/*     This is the starting program for the SUBSETS package of programs. */
/*     It forms the upper-triangular Banachiewicz factorization of the */
/*     input data. */
/*     Free-format input is assumed, i.e. with data fields separated by */
/*     spaces, CR's, tabs or commas.   N.B. Some Fortran compilers will */
/*     not accept tabs and/or commas as delimiters. */
/*     Warning: Some Fortran compilers will not allow free format input */
/*     of character data.   This program inputs the names of variables */
/*     in free format. */

/*     Latest revision - 16 August 1992 */

/* $$$c     IMPLICIT NONE */
/* $$$      integer npmax, dimu */
/* $$$      parameter (npmax=50, dimu=npmax*(npmax+1)/2) */
/*$$$      DOUBLE PRECISION U(dimu), EL(0:npmax), RHS(0:npmax), X(0:npmax),*/
/* $$$     +                 WT, ONE, Y, RESSQ */
/* $$$      CHARACTER ANS, FNAME*20, VNAME(0:npmax)*8, YNAME*8, TEXT*79 */
/*$$$      INTEGER LIN, YPOS, IPOS, I, K, ICONST, NCOLS, NOBS, NRBAR, IER,*/
/* $$$     +        LINE1, LOUT */
/* $$$      LOGICAL OK, LSEL */
/* $$$      DATA WT/1.D0/, ONE/1.D0/, LSEL/.FALSE./ */
/* $$$ */
/* $$$C */
/* $$$C     Set unit numbers for I/O in the data statement below. */
/* $$$C */
/* $$$      DATA LIN/5/, LOUT/6/ */
/* $$$C */
/* $$$C     Ask for details of the data file. */
/* $$$C */
/* $$$   10 WRITE(LOUT, 900) */
/* $$$  900 FORMAT(' Name of data file = ? ') */
/* $$$      READ(LIN, *) FNAME */
/* $$$C */
/* $$$C     Add extension .dat if none has been entered, */
/* $$$C     detected by the lack of a '.' */
/* $$$C */
/* $$$      IF (INDEX(FNAME, '.') .EQ. 0) THEN */
/* $$$	IPOS = INDEX(FNAME, ' ') */
/* $$$	FNAME = FNAME(1:IPOS-1) // '.dat' */
/* $$$      END IF */
/* $$$C */
/* $$$C     Check that file exists. */
/* $$$C */
/* $$$      INQUIRE(FILE=FNAME, EXIST=OK) */
/* $$$      IF (.NOT. OK) THEN */
/* $$$	WRITE(*, 910) FNAME */
/* $$$  910   FORMAT(' *** File not found - ', a, ' **') */
/* $$$	GO TO 10 */
/* $$$      END IF */
/* $$$C */
/* $$$C     Display first part of file. */
/* $$$C */
/* $$$      OPEN(10, FILE=FNAME, STATUS='OLD') */
/* $$$      WRITE(*, *)'Start of your data file follows' */
/* $$$      DO 20 I = 1, 12 */
/* $$$	READ(10, '(A)') TEXT */
/* $$$	WRITE(*, '(1X, A)') TEXT */
/* $$$   20 CONTINUE */
/* $$$      REWIND 10 */
/* $$$C */
/* $$$      WRITE(LOUT, 920) */
/* $$$  920 FORMAT(' How many X-variables ? ') */
/* $$$      READ(LIN, *) K */
/* $$$      WRITE(LOUT, 930) */
/* $$$  930 FORMAT('+Do you want a constant in the model ? ') */
/* $$$      READ(LIN, *) ANS */
/* $$$      ICONST = 0 */
/* $$$      IF(ANS.EQ.'Y' .OR. ANS .EQ. 'y') ICONST = 1 */
/* $$$      NCOLS = K + ICONST */
/* $$$      NRBAR = NCOLS * (NCOLS - 1) / 2 */
/* $$$C */
/* $$$C     Get position of dependant variable. */
/* $$$C */
/* $$$      WRITE(*, *)'Is dependant variable at end ? (Y/N): ' */
/* $$$      READ(*, *) ANS */
/* $$$      IF (ANS .EQ. 'Y' .OR. ANS .EQ. 'y') THEN */
/* $$$	YPOS = K+1 */
/* $$$      ELSE */
/* $$$	WRITE(*, *)'Enter no. of position of dependant variable: ' */
/* $$$	READ(*, *) YPOS */
/* $$$	IF (YPOS .LT. 1) YPOS = 1 */
/* $$$	IF (YPOS .GT. K) YPOS = K + 1 */
/* $$$      END IF */
/* $$$C */
/* $$$C     Enter variable names, read them from file, or set defaults. */
/* $$$C */
/* $$$      VNAME(0) = 'Constant' */
/* $$$      WRITE(*, *)'Are variable names in data file ? (Y/N): ' */
/* $$$      READ(*, *) ANS */
/* $$$      IF (ANS .EQ. 'Y' .OR. ANS .EQ. 'y') THEN */
/* $$$	WRITE(*, *)'Which line do names start on ? ' */
/* $$$	READ(*, *) LINE1 */
/* $$$	IF (LINE1 .GT. 1) THEN */
/* $$$	  DO 30 I = 1, LINE1-1 */
/* $$$   30     READ(10, *) */
/* $$$	END IF */
/* $$$	IF (YPOS .GT. K) THEN */
/* $$$	  READ(10, *) (VNAME(I),I=1,K), YNAME */
/* $$$	ELSE IF (YPOS .EQ. 1) THEN */
/* $$$	  READ(10, *) YNAME, (VNAME(I),I=1,K) */
/* $$$	ELSE */
/* $$$	  READ(10, *) (VNAME(I),I=1,YPOS-1), YNAME, */
/* $$$     +                        (VNAME(I),I=YPOS,K) */
/* $$$	END IF */
/* $$$	REWIND 10 */
/* $$$      ELSE */
/* $$$	WRITE(*, *)'Do you want to name variables ? (Y/N): ' */
/* $$$	READ(*, '(a)') ANS */
/* $$$	IF (ANS .EQ. 'Y' .OR. ANS .EQ. 'y') THEN */
/* $$$	  WRITE(*, *)'Variable names may contain up to 8 characters' */
/* $$$	  WRITE(*, *)'Name for dependant (Y) variable = ? ' */
/* $$$	  READ(*, '(a)') YNAME */
/* $$$	  DO 40 I = 1, K */
/* $$$	    WRITE(*, *)'Name for variable', I, ' = ? ' */
/* $$$	    READ(*, '(a)') VNAME(I) */
/* $$$   40     CONTINUE */
/* $$$	ELSE */
/* $$$	  DO 50 I = 1, K */
/* $$$	    WRITE(VNAME(I), 940) I */
/* $$$  940       FORMAT('XVAR(', I2, ')') */
/* $$$   50     CONTINUE */
/* $$$	  YNAME = 'Dept.var' */
/* $$$	END IF */
/* $$$      END IF */
/* $$$C */
/* $$$      WRITE(*, *)'Which line does the data start on ? ' */
/* $$$      READ(*, *) LINE1 */
/* $$$      IF (LINE1 .GT. 1) THEN */
/* $$$	DO 60 I = 1, LINE1-1 */
/* $$$   60   READ(10, *) */
/* $$$      END IF */
/* $$$C */
/* $$$C     Read in data and form the upper-triangular factorization. */
/* $$$C */
/* $$$      IF (ICONST .EQ. 1) THEN */
/* $$$	CALL CLEAR(NCOLS, NRBAR, EL, U, RHS, RESSQ, IER) */
/* $$$      ELSE */
/* $$$	CALL CLEAR(NCOLS, NRBAR, EL(1), U, RHS(1), RESSQ, IER) */
/* $$$      END IF */
/* $$$      NOBS = 1 */
/* $$$      X(0) = ONE */
/* $$$C */
/* $$$C     Case is skipped if spurious characters are found (e.g. for */
/* $$$C     missing values). */
/* $$$C */
/* $$$   70 CONTINUE */
/* $$$      IF (YPOS .GT. K) THEN */
/* $$$	READ(10, *, ERR=70, END=80) (X(I),I=1,K), Y */
/* $$$      ELSE IF (YPOS .EQ. 1) THEN */
/* $$$	READ(10, *, ERR=70, END=80) Y, (X(I),I=1,K) */
/* $$$      ELSE */
/* $$$	READ(10, *, ERR=70, END=80) (X(I),I=1,YPOS-1), Y, */
/* $$$     +                              (X(I),I=YPOS,K) */
/* $$$      END IF */
/* $$$      IF (ICONST .EQ. 1) THEN */
/* $$$	CALL INCLUD(NCOLS, NRBAR, WT, X, Y, EL, U, RHS, RESSQ, IER) */
/* $$$      ELSE */
/* $$$	CALL INCLUD(NCOLS, NRBAR, WT, X(1), Y, EL(1), U, RHS(1), RESSQ, */
/* $$$     +              IER) */
/* $$$      END IF */
/* $$$      NOBS = NOBS + 1 */
/* $$$      GO TO 70 */
/* $$$C */
/* $$$C     Change extension to .red for output file. */
/* $$$C */
/* $$$   80 IPOS = INDEX(FNAME, '.') */
/* $$$      FNAME(IPOS+1:IPOS+3) = 'red' */
/* $$$      NOBS = NOBS - 1 */
/* $$$C */
/*$$$C     Write U, EL, RHS & the residual sum of squares (RESSQ) to disk.*/
/* $$$C */
/* $$$      OPEN(9, FILE=FNAME, STATUS='NEW', ACCESS='SEQUENTIAL', */
/* $$$     +          FORM='UNFORMATTED') */
/* $$$      WRITE(9) K, ICONST, NCOLS, NOBS, NRBAR, LSEL */
/* $$$      IF (ICONST .EQ. 0) THEN */
/* $$$	WRITE(9) YNAME, (VNAME(I),I=1,K) */
/* $$$	WRITE(9) (U(I),I=1,NRBAR), (EL(I),I=1,K), (RHS(I),I=1,K), RESSQ */
/* $$$      ELSE */
/* $$$	WRITE(9) YNAME, (VNAME(I),I=0,K) */
/* $$$	WRITE(9) (U(I),I=1,NRBAR), (EL(I),I=0,K), (RHS(I),I=0,K), RESSQ */
/* $$$      END IF */
/* $$$      ENDFILE 9 */
/* $$$C */
/* $$$      END */
/* Subroutine */ int clear_(np, nrbar, d, rbar, thetab, sserr, ier)
integer *np, *nrbar;
doublereal *d, *rbar, *thetab, *sserr;
integer *ier;
{
    /* Initialized data */

    static doublereal zero = 0.;

    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i;


/*     ALGORITHM AS274  APPL. STATIST. (1992) VOL.41, NO. 2 */

/*     Sets arrays to zero prior to calling INCLUD */


/*     Local variables */


    /* Parameter adjustments */
    --thetab;
    --d;
    --rbar;

    /* Function Body */

/*     Some checks. */

    *ier = 0;
    if (*np < 1) {
	*ier = 1;
    }
    if (*nrbar < *np * (*np - 1) / 2) {
	*ier += 2;
    }
    if (*ier != 0) {
	return 0;
    }

    i__1 = *np;
    for (i = 1; i <= i__1; ++i) {
	d[i] = zero;
	thetab[i] = zero;
/* L10: */
    }
    i__1 = *nrbar;
    for (i = 1; i <= i__1; ++i) {
/* L20: */
	rbar[i] = zero;
    }
    *sserr = zero;
    return 0;
} /* clear_ */

/* $$$      SUBROUTINE INCLUD(NP, NRBAR, WEIGHT, XROW, YELEM, D, */
/* $$$     +      RBAR, THETAB, SSERR, IER) */
/* $$$C */
/* $$$C     ALGORITHM AS274  APPL. STATIST. (1992) VOL.41, NO. 2 */
/* $$$C     Modified from algorithm AS 75.1 */
/* $$$C */
/* $$$C     Calling this routine updates d, rbar, thetab and sserr by the */
/*$$$C     inclusion of xrow, yelem with the specified weight.   The number*/
/* $$$C     of columns (variables) may exceed the number of rows (cases). */
/* $$$C */
/* $$$C**** WARNING: The elements of XROW are overwritten  **** */
/* $$$C */
/* $$$      INTEGER NP, NRBAR, IER */
/* $$$      DOUBLE PRECISION WEIGHT, XROW(NP), YELEM, D(NP), RBAR(*), */
/* $$$     +    THETAB(NP), SSERR */
/* $$$C */
/* $$$C     Local variables */
/* $$$C */
/* $$$      INTEGER I, K, NEXTR */
/* $$$      DOUBLE PRECISION ZERO, W, Y, XI, DI, WXI, DPI, CBAR, SBAR, XK */
/* $$$C */
/* $$$      DATA ZERO/0.D0/ */
/* $$$C */
/* $$$C     Some checks. */
/* $$$C */
/* $$$      IER = 0 */
/* $$$      IF (NP .LT. 1) IER = 1 */
/* $$$      IF (NRBAR .LT. NP*(NP-1)/2) IER = IER + 2 */
/* $$$      IF (IER .NE. 0) RETURN */
/* $$$C */
/* $$$      W = WEIGHT */
/* $$$      Y = YELEM */
/* $$$      NEXTR = 1 */
/* $$$      DO 30 I = 1, NP */
/* $$$C */
/*$$$C     Skip unnecessary transformations.   Test on exact zeroes must be*/
/* $$$C     used or stability can be destroyed. */
/* $$$C */
/* $$$	IF (W .EQ. ZERO) RETURN */
/* $$$	XI = XROW(I) */
/* $$$	IF (XI .EQ. ZERO) THEN */
/* $$$	  NEXTR = NEXTR + NP - I */
/* $$$	  GO TO 30 */
/* $$$	END IF */
/* $$$	DI = D(I) */
/* $$$	WXI = W * XI */
/* $$$	DPI = DI + WXI*XI */
/* $$$	CBAR = DI / DPI */
/* $$$	SBAR = WXI / DPI */
/* $$$	W = CBAR * W */
/* $$$	D(I) = DPI */
/* $$$	IF (I .EQ. NP) GO TO 20 */
/* $$$	DO 10 K = I+1, NP */
/* $$$	  XK = XROW(K) */
/* $$$	  XROW(K) = XK - XI * RBAR(NEXTR) */
/* $$$	  RBAR(NEXTR) = CBAR * RBAR(NEXTR) + SBAR * XK */
/* $$$	  NEXTR = NEXTR + 1 */
/* $$$   10   CONTINUE */
/* $$$   20   XK = Y */
/* $$$	Y = XK - XI * THETAB(I) */
/* $$$	THETAB(I) = CBAR * THETAB(I) + SBAR * XK */
/* $$$   30 CONTINUE */
/* $$$C */
/*$$$C     Y * SQRT(W) is now equal to Brown & Durbin's recursive residual.*/
/* $$$C */
/* $$$      SSERR = SSERR + W * Y * Y */
/* $$$C */
/* $$$      RETURN */
/* $$$      END */
/* Subroutine */ int add1_(np, nrbar, d, rbar, thetab, first, last, tol, ss, 
	sxx, sxy, smax, jmax, ier)
integer *np, *nrbar;
doublereal *d, *rbar, *thetab;
integer *first, *last;
doublereal *tol, *ss, *sxx, *sxy, *smax;
integer *jmax, *ier;
{
    /* Initialized data */

    static doublereal zero = 0.;

    /* System generated locals */
    integer i__1, i__2;
    doublereal d__1;

    /* Builtin functions */
    double sqrt();

    /* Local variables */
    static doublereal diag, ssqx;
    static integer j;
    static doublereal dy;
    static integer inc, col, pos, row;


/*     Calculate the reduction in residual sum of squares when one */
/*     variable, selected from those in positions FIRST .. LAST, is */
/*     added, given that the variables in positions 1 .. FIRST-1 (if */
/*     any) are already included. */


/*     Local variables */

    /* Parameter adjustments */
    --sxy;
    --sxx;
    --ss;
    --tol;
    --thetab;
    --d;
    --rbar;

    /* Function Body */

/*     Check call arguments */

    *jmax = 0;
    *smax = zero;
    *ier = 0;
    if (*first > *np) {
	*ier = 1;
    }
    if (*last < *first) {
	*ier += 2;
    }
    if (*first < 1) {
	*ier += 4;
    }
    if (*last > *np) {
	*ier += 8;
    }
    if (*nrbar < *np * (*np - 1) / 2) {
	*ier += 16;
    }
    if (*ier != 0) {
	return 0;
    }

/*     Accumulate sums of squares & products from row FIRST */

    i__1 = *last;
    for (j = *first; j <= i__1; ++j) {
	sxx[j] = zero;
	sxy[j] = zero;
/* L10: */
    }
    inc = *np - *last;
    pos = (*first - 1) * (*np + *np - *first) / 2 + 1;
    i__1 = *last;
    for (row = *first; row <= i__1; ++row) {
	diag = d[row];
	dy = diag * thetab[row];
	sxx[row] += diag;
	sxy[row] += dy;
	i__2 = *last;
	for (col = row + 1; col <= i__2; ++col) {
/* Computing 2nd power */
	    d__1 = rbar[pos];
	    sxx[col] += diag * (d__1 * d__1);
	    sxy[col] += dy * rbar[pos];
	    ++pos;
/* L20: */
	}
	pos += inc;
/* L30: */
    }

/*     Incremental sum of squares for a variable = SXY * SXY / SXX. */
/*     Calculate whenever sqrt(SXX) > TOL for that variable. */

    i__1 = *last;
    for (j = *first; j <= i__1; ++j) {
	ssqx = sxx[j];
	if (sqrt(ssqx) > tol[j]) {
/* Computing 2nd power */
	    d__1 = sxy[j];
	    ss[j] = d__1 * d__1 / sxx[j];
	    if (ss[j] > *smax) {
		*smax = ss[j];
		*jmax = j;
	    }
	} else {
	    ss[j] = zero;
	}
/* L40: */
    }

    return 0;
} /* add1_ */

/* Subroutine */ int bakwrd_(np, nrbar, d, rbar, thetab, first, last, vorder, 
	tol, rss, bound, nvmax, ress, ir, nbest, lopt, il, wk, iwk, ier)
integer *np, *nrbar;
doublereal *d, *rbar, *thetab;
integer *first, *last, *vorder;
doublereal *tol, *rss, *bound;
integer *nvmax;
doublereal *ress;
integer *ir, *nbest, *lopt, *il;
doublereal *wk;
integer *iwk, *ier;
{
    /* System generated locals */
    integer lopt_dim1, lopt_offset, ress_dim1, ress_offset, i__1, i__2;

    /* Local variables */
    static integer need, jmin;
    static doublereal smin;
    extern /* Subroutine */ int drop1_();
    static integer i, j1;
    extern /* Subroutine */ int vmove_(), report_();
    static integer pos;


/*     Backward elimination from variables in positions FIRST .. LAST. */
/*     If FIRST > 1, variables in positions prior to this are forced in. 
*/
/*     If LAST < NP, variables in positions after this are forced out. */
/*     On exit, the array VORDER contains the numbers of the variables */
/*     in the order in which they were deleted. */


/*     Local variables */


/*     Check call arguments */

    /* Parameter adjustments */
    --rss;
    --tol;
    --vorder;
    --thetab;
    --d;
    --rbar;
    --bound;
    ress_dim1 = *ir;
    ress_offset = ress_dim1 + 1;
    ress -= ress_offset;
    lopt_dim1 = *il;
    lopt_offset = lopt_dim1 + 1;
    lopt -= lopt_offset;
    --wk;

    /* Function Body */
    *ier = 0;
    if (*first >= *np) {
	*ier = 1;
    }
    if (*last <= 1) {
	*ier += 2;
    }
    if (*first < 1) {
	*ier += 4;
    }
    if (*last > *np) {
	*ier += 8;
    }
    if (*nrbar < *np * (*np - 1) / 2) {
	*ier += 16;
    }
    if (*iwk < *last << 1) {
	*ier += 32;
    }
    if (*nbest > 0) {
	need = *nvmax * (*nvmax + 1) / 2;
	if (*ir < *nvmax) {
	    *ier += 64;
	}
	if (*il < need) {
	    *ier += 128;
	}
    }
    if (*ier != 0) {
	return 0;
    }

/*     For POS = LAST, ..., FIRST+1 call DROP1 to find best variable to */
/*     find which variable to drop next. */

    j1 = *last + 1;
    i__1 = *first + 1;
    for (pos = *last; pos >= i__1; --pos) {
	drop1_(np, nrbar, &d[1], &rbar[1], &thetab[1], first, &pos, &tol[1], &
		wk[1], &wk[j1], &smin, &jmin, ier);
	if (jmin > 0 && jmin < pos) {
	    vmove_(np, nrbar, &vorder[1], &d[1], &rbar[1], &thetab[1], &rss[1]
		    , &jmin, &pos, &tol[1], ier);
	    if (*nbest > 0) {
		i__2 = pos - 1;
		for (i = jmin; i <= i__2; ++i) {
/* L10: */
		    report_(&i, &rss[i], &bound[1], nvmax, &ress[ress_offset],
			     ir, nbest, &lopt[lopt_offset], il, &vorder[1]);
		}
	    }
	}
/* L20: */
    }

    return 0;
} /* bakwrd_ */

/* Subroutine */ int drop1_(np, nrbar, d, rbar, thetab, first, last, tol, ss, 
	wk, smin, jmin, ier)
integer *np, *nrbar;
doublereal *d, *rbar, *thetab;
integer *first, *last;
doublereal *tol, *ss, *wk, *smin;
integer *jmin, *ier;
{
    /* Initialized data */

    static doublereal large = 1e35;
    static doublereal zero = 0.;

    /* System generated locals */
    integer i__1, i__2, i__3;
    doublereal d__1;

    /* Builtin functions */
    double sqrt();

    /* Local variables */
    static integer i, j;
    static doublereal x, d1, d2;
    static integer inc, col;
    static doublereal rhs;
    static integer pos, row, pos1;


/*     Calculate the increase in the residual sum of squares when */
/*     variable J is dropped from the model, for J = FIRST, ..., LAST. */


/*     Local variables */

    /* Parameter adjustments */
    --tol;
    --thetab;
    --d;
    --rbar;
    --wk;
    --ss;

    /* Function Body */

/*     Check call arguments */

    *jmin = 0;
    *smin = large;
    *ier = 0;
    if (*first > *np) {
	*ier = 1;
    }
    if (*last < *first) {
	*ier += 2;
    }
    if (*first < 1) {
	*ier += 4;
    }
    if (*last > *np) {
	*ier += 8;
    }
    if (*nrbar < *np * (*np - 1) / 2) {
	*ier += 16;
    }
    if (*ier != 0) {
	return 0;
    }

/*     POS1 = position of first element of row FIRST in RBAR. */

    pos1 = (*first - 1) * (*np + *np - *first) / 2 + 1;
    inc = *np - *last;

/*     Start of outer cycle for the variable to be dropped. */

    i__1 = *last;
    for (j = *first; j <= i__1; ++j) {
	d1 = d[j];
	if (sqrt(d1) < tol[j]) {
	    ss[j] = zero;
	    *smin = zero;
	    *jmin = j;
	    goto L50;
	}
	rhs = thetab[j];
	if (j == *last) {
	    goto L40;
	}

/*     Copy row J of RBAR into WK. */

	pos = pos1;
	i__2 = *last;
	for (i = j + 1; i <= i__2; ++i) {
	    wk[i] = rbar[pos];
	    ++pos;
/* L10: */
	}
	pos += inc;

/*     Lower the variable past each row. */

	i__2 = *last;
	for (row = j + 1; row <= i__2; ++row) {
	    x = wk[row];
	    d2 = d[row];
	    if (abs(x) * sqrt(d1) < tol[row] || d2 == zero) {
		pos = pos + *np - row;
		goto L30;
	    }
/* Computing 2nd power */
	    d__1 = x;
	    d1 = d1 * d2 / (d2 + d1 * (d__1 * d__1));
	    i__3 = *last;
	    for (col = row + 1; col <= i__3; ++col) {
		wk[col] -= x * rbar[pos];
		++pos;
/* L20: */
	    }
	    rhs -= x * thetab[row];
	    pos += inc;
L30:
	    ;
	}
L40:
	ss[j] = rhs * d1 * rhs;
	if (ss[j] < *smin) {
	    *jmin = j;
	    *smin = ss[j];
	}

/*     Update position of first element in row of RBAR. */

L50:
	if (j < *last) {
	    pos1 = pos1 + *np - j;
	}

/* L60: */
    }

    return 0;
} /* drop1_ */

/* Subroutine */ int exadd1_(ivar, rss, bound, nvmax, ress, ir, nbest, lopt, 
	il, vorder, smax, jmax, ss, wk, last)
integer *ivar;
doublereal *rss, *bound;
integer *nvmax;
doublereal *ress;
integer *ir, *nbest, *lopt, *il, *vorder;
doublereal *smax;
integer *jmax;
doublereal *ss, *wk;
integer *last;
{
    /* Initialized data */

    static doublereal zero = 0.;

    /* System generated locals */
    integer lopt_dim1, lopt_offset, ress_dim1, ress_offset, i__1, i__2;

    /* Local variables */
    static doublereal temp;
    static integer i, j, ltemp, jm;
    static doublereal sm, ssbase;
    extern /* Subroutine */ int report_();


/*     Update the NBEST subsets of IVAR variables found from a call */
/*     to subroutine ADD1. */


/*     Local variables */

    /* Parameter adjustments */
    --bound;
    ress_dim1 = *ir;
    ress_offset = ress_dim1 + 1;
    ress -= ress_offset;
    lopt_dim1 = *il;
    lopt_offset = lopt_dim1 + 1;
    lopt -= lopt_offset;
    --wk;
    --ss;
    --vorder;
    --rss;

    /* Function Body */

    if (*jmax == 0) {
	return 0;
    }
    if (*ivar <= 0) {
	return 0;
    }
    if (*ivar > *nvmax) {
	return 0;
    }
    ltemp = vorder[*ivar];
    jm = *jmax;
    sm = *smax;
    if (*ivar > 1) {
	ssbase = rss[*ivar - 1];
    }
    if (*ivar == 1) {
	ssbase = rss[*ivar] + ss[1];
    }
    i__1 = *last;
    for (j = *ivar; j <= i__1; ++j) {
/* L10: */
	wk[j] = ss[j];
    }

    i__1 = *nbest;
    for (i = 1; i <= i__1; ++i) {
	temp = ssbase - sm;
	if (temp >= bound[*ivar]) {
	    goto L40;
	}
	vorder[*ivar] = vorder[jm];
	if (jm == *ivar) {
	    vorder[*ivar] = ltemp;
	}
	report_(ivar, &temp, &bound[1], nvmax, &ress[ress_offset], ir, nbest, 
		&lopt[lopt_offset], il, &vorder[1]);
	if (i >= *nbest) {
	    goto L40;
	}
	wk[jm] = zero;
	sm = zero;
	jm = 0;
	i__2 = *last;
	for (j = *ivar; j <= i__2; ++j) {
	    if (wk[j] <= sm) {
		goto L20;
	    }
	    jm = j;
	    sm = wk[j];
L20:
	    ;
	}
	if (jm == 0) {
	    goto L40;
	}
/* L30: */
    }

/*     Restore VORDER(IVAR) */

L40:
    vorder[*ivar] = ltemp;

    return 0;
} /* exadd1_ */

/* Subroutine */ int forwrd_(np, nrbar, d, rbar, thetab, first, last, vorder, 
	tol, rss, bound, nvmax, ress, ir, nbest, lopt, il, wk, iwk, ier)
integer *np, *nrbar;
doublereal *d, *rbar, *thetab;
integer *first, *last, *vorder;
doublereal *tol, *rss, *bound;
integer *nvmax;
doublereal *ress;
integer *ir, *nbest, *lopt, *il;
doublereal *wk;
integer *iwk, *ier;
{
    /* System generated locals */
    integer lopt_dim1, lopt_offset, ress_dim1, ress_offset, i__1;

    /* Local variables */
    static integer need, jmax;
    static doublereal smax;
    static integer j1, j2;
    extern /* Subroutine */ int vmove_(), exadd1_();
    static integer pos;
    extern /* Subroutine */ int add1_();


/*     Forward selection from variables in positions FIRST .. LAST. */
/*     If FIRST > 1, variables in positions prior to this are forced in. 
*/
/*     If LAST < NP, variables in positions after this are forced out. */
/*     On exit, the array VORDER contains the numbers of the variables */
/*     in the order in which they were added. */


/*     Local variables */


/*     Check call arguments */

    /* Parameter adjustments */
    --rss;
    --tol;
    --vorder;
    --thetab;
    --d;
    --rbar;
    --bound;
    ress_dim1 = *ir;
    ress_offset = ress_dim1 + 1;
    ress -= ress_offset;
    lopt_dim1 = *il;
    lopt_offset = lopt_dim1 + 1;
    lopt -= lopt_offset;
    --wk;

    /* Function Body */
    *ier = 0;
    if (*first >= *np) {
	*ier = 1;
    }
    if (*last <= 1) {
	*ier += 2;
    }
    if (*first < 1) {
	*ier += 4;
    }
    if (*last > *np) {
	*ier += 8;
    }
    if (*nrbar < *np * (*np - 1) / 2) {
	*ier += 16;
    }
    if (*iwk < *last * 3) {
	*ier += 32;
    }
    if (*nbest > 0) {
	need = *nvmax * (*nvmax + 1) / 2;
	if (*ir < *nvmax) {
	    *ier += 64;
	}
	if (*il < need) {
	    *ier += 128;
	}
    }
    if (*ier != 0) {
	return 0;
    }

/*     For POS = FIRST .. LAST-1, call ADD1 to find best variable to put 
*/
/*     into position POS. */

    j1 = *last + 1;
    j2 = *last + j1;
    i__1 = *last - 1;
    for (pos = *first; pos <= i__1; ++pos) {
	add1_(np, nrbar, &d[1], &rbar[1], &thetab[1], &pos, last, &tol[1], &
		wk[1], &wk[j1], &wk[j2], &smax, &jmax, ier);
	if (*nbest > 0) {
	    exadd1_(&pos, &rss[1], &bound[1], nvmax, &ress[ress_offset], ir, 
		    nbest, &lopt[lopt_offset], il, &vorder[1], &smax, &jmax, &
		    wk[1], &wk[j1], last);
	}

/*     Move the best variable to position POS. */

	if (jmax > pos) {
	    vmove_(np, nrbar, &vorder[1], &d[1], &rbar[1], &thetab[1], &rss[1]
		    , &jmax, &pos, &tol[1], ier);
	}
/* L10: */
    }

    return 0;
} /* forwrd_ */

/* Subroutine */ int initr_(np, nvmax, nbest, bound, ress, ir, lopt, il, 
	vorder, rss, ier)
integer *np, *nvmax, *nbest;
doublereal *bound, *ress;
integer *ir, *lopt, *il, *vorder;
doublereal *rss;
integer *ier;
{
    /* Initialized data */

    static doublereal large = 1e35;

    /* System generated locals */
    integer lopt_dim1, lopt_offset, ress_dim1, ress_offset, i__1, i__2, i__3;

    /* Local variables */
    static integer best, nvar, i, pos;


/*     Initialize the recording of best subsets */


/*     Local variables */

    /* Parameter adjustments */
    --rss;
    --vorder;
    --bound;
    ress_dim1 = *ir;
    ress_offset = ress_dim1 + 1;
    ress -= ress_offset;
    lopt_dim1 = *il;
    lopt_offset = lopt_dim1 + 1;
    lopt -= lopt_offset;

    /* Function Body */

/*     Check call arguments */

    *ier = 0;
    if (*nbest <= 0) {
	*ier = 1;
    }
    if (*nvmax <= 0) {
	*ier += 2;
    }
    if (*nvmax > *np) {
	*ier += 4;
    }
    if (*ir < *nvmax) {
	*ier += 8;
    }
    if (*il < *nvmax * (*nvmax + 1) / 2) {
	*ier += 16;
    }
    if (*ier != 0) {
	return 0;
    }

/*     Initialize arrays BOUND, RESS & LOPT */

    i__1 = *nbest;
    for (best = 1; best <= i__1; ++best) {
	pos = 1;
	i__2 = *nvmax;
	for (nvar = 1; nvar <= i__2; ++nvar) {
	    if (best == 1) {
		ress[nvar + best * ress_dim1] = rss[nvar];
	    } else {
		ress[nvar + best * ress_dim1] = large;
	    }
	    if (best == *nbest) {
		bound[nvar] = ress[nvar + *nbest * ress_dim1];
	    }
	    i__3 = nvar;
	    for (i = 1; i <= i__3; ++i) {
		if (best == 1) {
		    lopt[pos + best * lopt_dim1] = vorder[i];
		} else {
		    lopt[pos + best * lopt_dim1] = 0;
		}
		++pos;
/* L10: */
	    }
/* L20: */
	}
/* L30: */
    }

    return 0;
} /* initr_ */

/* Subroutine */ int report_(pos, ssq, bound, nvmax, ress, ir, nbest, lopt, 
	il, vorder)
integer *pos;
doublereal *ssq, *bound;
integer *nvmax;
doublereal *ress;
integer *ir, *nbest, *lopt, *il, *vorder;
{
    /* Initialized data */

    static doublereal under1 = .9999;
    static doublereal over1 = 1.0001;

    /* System generated locals */
    integer lopt_dim1, lopt_offset, ress_dim1, ress_offset, i__1, i__2;

    /* Local variables */
    static integer rank, i, j, k, l, listj, l0, jj;


/*     Update record of the best NBEST subsets of POS variables, if */
/*     necessary, using SSQ. */


/*     Local variables */

    /* Parameter adjustments */
    --vorder;
    --bound;
    ress_dim1 = *ir;
    ress_offset = ress_dim1 + 1;
    ress -= ress_offset;
    lopt_dim1 = *il;
    lopt_offset = lopt_dim1 + 1;
    lopt -= lopt_offset;

    /* Function Body */

/*     If residual sum of squares (SSQ) for the new subset > the */
/*     appropriate bound, return. */

/* L20: */
    if (*pos > *nvmax) {
	return 0;
    }
    if (*ssq >= bound[*pos]) {
	return 0;
    }

/*     Find rank of the new subset */

    i__1 = *nbest;
    for (rank = 1; rank <= i__1; ++rank) {
	if (*ssq <= ress[*pos + rank * ress_dim1]) {
	    goto L40;
	}
/* L30: */
    }
L40:
    l0 = *pos * (*pos - 1) / 2;

/*     Check that the subset is not a duplicate of one which has already 
*/
/*     been recorded. */

    jj = rank;
    if (*ssq > under1 * ress[*pos + rank * ress_dim1]) {
	goto L50;
    }
    if (rank == 1) {
	goto L90;
    }
    if (*ssq > over1 * ress[*pos + (rank - 1) * ress_dim1]) {
	goto L90;
    }
    jj = rank - 1;
L50:
    i__1 = *pos;
    for (j = 1; j <= i__1; ++j) {
	listj = vorder[j];
	l = l0;
	i__2 = *pos;
	for (i = 1; i <= i__2; ++i) {
	    ++l;
	    if (listj == lopt[l + jj * lopt_dim1]) {
		goto L70;
	    }
/* L60: */
	}
	goto L80;
L70:
	;
    }
    return 0;
L80:
    --jj;
    if (jj > 0 && jj == rank - 1) {
	goto L50;
    }

/*     Record new subset, and move down the other records. */

L90:
    if (rank == *nbest) {
	goto L110;
    }
    j = *nbest - rank;
    i__1 = j;
    for (i = 1; i <= i__1; ++i) {
	jj = *nbest - i;
	ress[*pos + (jj + 1) * ress_dim1] = ress[*pos + jj * ress_dim1];
	l = l0;
	i__2 = *pos;
	for (k = 1; k <= i__2; ++k) {
	    ++l;
	    lopt[l + (jj + 1) * lopt_dim1] = lopt[l + jj * lopt_dim1];
/* L100: */
	}
    }
L110:
    ress[*pos + rank * ress_dim1] = *ssq;
    l = l0;
    i__2 = *pos;
    for (k = 1; k <= i__2; ++k) {
	++l;
	lopt[l + rank * lopt_dim1] = vorder[k];
/* L120: */
    }
    bound[*pos] = ress[*pos + *nbest * ress_dim1];
} /* report_ */

/* Subroutine */ int seqrep_(np, nrbar, d, rbar, thetab, first, last, vorder, 
	tol, rss, bound, nvmax, ress, ir, nbest, lopt, il, wk, iwk, ier)
integer *np, *nrbar;
doublereal *d, *rbar, *thetab;
integer *first, *last, *vorder;
doublereal *tol, *rss, *bound;
integer *nvmax;
doublereal *ress;
integer *ir, *nbest, *lopt, *il;
doublereal *wk;
integer *iwk, *ier;
{
    /* Initialized data */

    static doublereal zero = 0.;

    /* System generated locals */
    integer lopt_dim1, lopt_offset, ress_dim1, ress_offset, i__1, i__2;

    /* Local variables */
    static integer need, best, jmax, from;
    static doublereal smax;
    static integer size, i;
    static doublereal ssred;
    static integer count, j1, j2, start;
    extern /* Subroutine */ int vmove_(), exadd1_();
    static integer nv;
    extern /* Subroutine */ int add1_();


/*     Sequential replacement algorithm applied to the variables in */
/*     positions FIRST, ..., LAST. */
/*     If FIRST > 1, variables in positions prior to this are forced in. 
*/
/*     If LAST < NP, variables in positions after this are forced out. */


/*     Local variables */

    /* Parameter adjustments */
    --rss;
    --tol;
    --vorder;
    --thetab;
    --d;
    --rbar;
    --bound;
    ress_dim1 = *ir;
    ress_offset = ress_dim1 + 1;
    ress -= ress_offset;
    lopt_dim1 = *il;
    lopt_offset = lopt_dim1 + 1;
    lopt -= lopt_offset;
    --wk;

    /* Function Body */

/*     Check call arguments */

    *ier = 0;
    if (*first >= *np) {
	*ier = 1;
    }
    if (*last <= 1) {
	*ier += 2;
    }
    if (*first < 1) {
	*ier += 4;
    }
    if (*last > *np) {
	*ier += 8;
    }
    if (*nrbar < *np * (*np - 1) / 2) {
	*ier += 16;
    }
    if (*iwk < *last * 3) {
	*ier += 32;
    }
    if (*nbest > 0) {
	need = *nvmax * (*nvmax + 1) / 2;
	if (*ir < *nvmax) {
	    *ier += 64;
	}
	if (*il < need) {
	    *ier += 128;
	}
    }
    if (*ier != 0 || *nbest <= 0) {
	return 0;
    }

    j1 = *last + 1;
    j2 = j1 + *last;
/* Computing MIN */
    i__1 = *nvmax, i__2 = *last - 1;
    nv = min(i__1,i__2);

/*     Outer loop; SIZE = current size of subset being considered. */

    i__1 = nv;
    for (size = *first; size <= i__1; ++size) {
	count = 0;
	start = *first;
L10:
	ssred = zero;
	best = 0;
	from = 0;

/*     Find the best variable from those in positions SIZE+1, ..., LAS
T */
/*     to replace the one in position SIZE.   Then rotate variables in
 */
/*     positions START, ..., SIZE. */

	i__2 = size;
	for (i = start; i <= i__2; ++i) {
	    add1_(np, nrbar, &d[1], &rbar[1], &thetab[1], &size, last, &tol[1]
		    , &wk[1], &wk[j1], &wk[j2], &smax, &jmax, ier);
	    if (jmax > size) {
		exadd1_(&size, &rss[1], &bound[1], nvmax, &ress[ress_offset], 
			ir, nbest, &lopt[lopt_offset], il, &vorder[1], &smax, 
			&jmax, &wk[1], &wk[j1], last);
		if (smax > ssred) {
		    ssred = smax;
		    best = jmax;
		    if (i < size) {
			from = size + start - i - 1;
		    } else {
			from = size;
		    }
		}
	    }
	    if (i < size) {
		vmove_(np, nrbar, &vorder[1], &d[1], &rbar[1], &thetab[1], &
			rss[1], &size, &start, &tol[1], ier);
	    }
/* L20: */
	}

/*     If any replacement reduces the RSS, make the best one. */
/*     Move variable from position FROM to SIZE. */
/*     Move variable from position BEST to FIRST. */

	if (best > size) {
	    if (from < size) {
		vmove_(np, nrbar, &vorder[1], &d[1], &rbar[1], &thetab[1], &
			rss[1], &from, &size, &tol[1], ier);
	    }
	    vmove_(np, nrbar, &vorder[1], &d[1], &rbar[1], &thetab[1], &rss[1]
		    , &best, first, &tol[1], ier);
	    count = 0;
	    start = *first + 1;
	} else {
	    ++count;
	}

/*     Repeat until COUNT = SIZE - START + 1 */

	if (count <= size - start) {
	    goto L10;
	}
/* L30: */
    }

    return 0;
} /* seqrep_ */

/* Subroutine */ int xhaust_(np, nrbar, d, rbar, thetab, first, last, vorder, 
	tol, rss, bound, nvmax, ress, ir, nbest, lopt, il, wk, dimwk, iwk, 
	dimiwk, ier)
integer *np, *nrbar;
doublereal *d, *rbar, *thetab;
integer *first, *last, *vorder;
doublereal *tol, *rss, *bound;
integer *nvmax;
doublereal *ress;
integer *ir, *nbest, *lopt, *il;
doublereal *wk;
integer *dimwk, *iwk, *dimiwk, *ier;
{
    /* System generated locals */
    integer lopt_dim1, lopt_offset, ress_dim1, ress_offset, i__1, i__2, i__3;

    /* Local variables */
    static integer need, jmax;
    static doublereal temp, smax;
    static integer i, j1, j2;
    extern /* Subroutine */ int vmove_(), exadd1_();
    static integer newpos;
    extern /* Subroutine */ int report_();
    static integer ipt, row;
    extern /* Subroutine */ int add1_();


/*     Exhaustive search algorithm, using leaps and bounds, applied to */
/*     the variables in positions FIRST, ..., LAST. */
/*     If FIRST > 1, variables in positions prior to this are forced in. 
*/
/*     If LAST < NP, variables in positions after this are forced out. */


/*     Local variables */


/*     Check call arguments */

    /* Parameter adjustments */
    --rss;
    --tol;
    --vorder;
    --thetab;
    --d;
    --rbar;
    --bound;
    ress_dim1 = *ir;
    ress_offset = ress_dim1 + 1;
    ress -= ress_offset;
    lopt_dim1 = *il;
    lopt_offset = lopt_dim1 + 1;
    lopt -= lopt_offset;
    --wk;
    --iwk;

    /* Function Body */
    *ier = 0;
    if (*first >= *np) {
	*ier = 1;
    }
    if (*last <= 1) {
	*ier += 2;
    }
    if (*first < 1) {
	*ier += 4;
    }
    if (*last > *np) {
	*ier += 8;
    }
    if (*nrbar < *np * (*np - 1) / 2) {
	*ier += 16;
    }
    if (*dimwk < *last * 3 || *dimiwk < *nvmax) {
	*ier += 32;
    }
    if (*nbest > 0) {
	need = *nvmax * (*nvmax + 1) / 2;
	if (*ir < *nvmax) {
	    *ier += 64;
	}
	if (*il < need) {
	    *ier += 128;
	}
    }
    if (*ier != 0 || *nbest <= 0) {
	return 0;
    }

    j1 = *last + 1;
    j2 = j1 + *last;

/*     Record subsets contained in the initial ordering, including check 
*/
/*     for variables which are linearly related to earlier variables. */
/*     This should be redundant if the user has first called SING and */
/*     INITR. */

    i__1 = *nvmax;
    for (row = *first; row <= i__1; ++row) {
	if (d[row] <= tol[row]) {
	    *ier = -999;
	    return 0;
	}
	report_(&row, &rss[row], &bound[1], nvmax, &ress[ress_offset], ir, 
		nbest, &lopt[lopt_offset], il, &vorder[1]);
/* L10: */
    }

/*     IWK(I) contains the upper limit for the I-th simulated DO-loop for 
*/
/*     I = FIRST, ..., NVMAX-1. */
/*     IPT points to the current DO loop. */

    i__1 = *nvmax;
    for (i = *first; i <= i__1; ++i) {
/* L20: */
	iwk[i] = *last;
    }

/*     Innermost loop. */
/*     Find best possible variable for position NVMAX from those in */
/*     positions NVMAX, .., IWK(NVMAX). */

L30:
    add1_(np, nrbar, &d[1], &rbar[1], &thetab[1], nvmax, &iwk[*nvmax], &tol[1]
	    , &wk[1], &wk[j1], &wk[j2], &smax, &jmax, ier);
    exadd1_(nvmax, &rss[1], &bound[1], nvmax, &ress[ress_offset], ir, nbest, &
	    lopt[lopt_offset], il, &vorder[1], &smax, &jmax, &wk[1], &wk[j1], 
	    &iwk[*nvmax]);

/*     Move to next lower numbered loop which has not been exhausted. */

    ipt = *nvmax - 1;
L40:
    if (ipt >= iwk[ipt]) {
	--ipt;
	if (ipt >= *first) {
	    goto L40;
	}
	return 0;
    }

/*     Lower variable from position IPT to position IWK(IPT). */
/*     Record any good new subsets found by the move. */

    newpos = iwk[ipt];
    vmove_(np, nrbar, &vorder[1], &d[1], &rbar[1], &thetab[1], &rss[1], &ipt, 
	    &newpos, &tol[1], ier);
/* Computing MIN */
    i__2 = *nvmax, i__3 = newpos - 1;
    i__1 = min(i__2,i__3);
    for (i = ipt; i <= i__1; ++i) {
/* L50: */
	report_(&i, &rss[i], &bound[1], nvmax, &ress[ress_offset], ir, nbest, 
		&lopt[lopt_offset], il, &vorder[1]);
    }

/*     Reset all ends of loops for I >= IPT. */

    i__1 = *nvmax;
    for (i = ipt; i <= i__1; ++i) {
/* L60: */
	iwk[i] = newpos - 1;
    }

/*     If residual sum of squares for all variables above position NEWPOS 
*/
/*     is greater than BOUND(I), no better subsets of size I can be found 
*/
/*     inside the current loop. */

    temp = rss[newpos - 1];
    i__1 = *nvmax;
    for (i = ipt; i <= i__1; ++i) {
	if (temp > bound[i]) {
	    goto L80;
	}
/* L70: */
    }
    if (iwk[*nvmax] > *nvmax) {
	goto L30;
    }
    ipt = *nvmax - 1;
    goto L40;
L80:
    ipt = i - 1;
    if (ipt < *first) {
	return 0;
    }
    goto L40;

} /* xhaust_ */


/* Subroutine */ int efroym_(np, nrbar, d, rbar, thetab, first, last, fin, 
	fout, size, nobs, vorder, tol, rss, bound, nvmax, ress, ir, nbest, 
	lopt, il, wk, iwk, ier)
integer *np, *nrbar;
doublereal *d, *rbar, *thetab;
integer *first, *last;
doublereal *fin, *fout;
integer *size, *nobs, *vorder;
doublereal *tol, *rss, *bound;
integer *nvmax;
doublereal *ress;
integer *ir, *nbest, *lopt, *il;
doublereal *wk;
integer *iwk, *ier;
{
    /* Initialized data */

    static doublereal one = 1.;
    static doublereal eps = 1e-16;
    static doublereal zero = 0.;

    /* Format strings */
    static char fmt_900[] = "(\002 F-to-enter = \002,f10.2)";
    static char fmt_910[] = "(\002 F-to-drop variable: \002,i4,\002 = \002,f\
10.2)";

    /* System generated locals */
    integer lopt_dim1, lopt_offset, ress_dim1, ress_offset, i__1;

    /* Builtin functions */
    integer s_wsle(), do_lio(), e_wsle(), s_wsfe(), do_fio(), e_wsfe();

    /* Local variables */
    static doublereal base;
    static integer need, jmin, jmax;
    static doublereal smin, smax;
    extern /* Subroutine */ int drop1_();
    static doublereal f;
    static integer i, j1, j2;
    extern /* Subroutine */ int vmove_(), exadd1_(), report_();
    static doublereal var;
    extern /* Subroutine */ int add1_();

    /* Fortran I/O blocks */
    static cilist io___92 = { 0, 6, 0, 0, 0 };
    static cilist io___96 = { 0, 6, 0, fmt_900, 0 };
    static cilist io___99 = { 0, 6, 0, fmt_910, 0 };



/*     Efroymson's stepwise regression from variables in positions FIRST, 
*/
/*     ..., LAST.  If FIRST > 1, variables in positions prior to this are 
*/
/*     forced in.  If LAST < NP, variables in positions after this are */
/*     forced out. */

/*     IMPLICIT NONE */

/*     Local variables */

    /* Parameter adjustments */
    --rss;
    --tol;
    --vorder;
    --thetab;
    --d;
    --rbar;
    --bound;
    ress_dim1 = *ir;
    ress_offset = ress_dim1 + 1;
    ress -= ress_offset;
    lopt_dim1 = *il;
    lopt_offset = lopt_dim1 + 1;
    lopt -= lopt_offset;
    --wk;

    /* Function Body */

/*     Check call arguments */

    *ier = 0;
    if (*first >= *np) {
	*ier = 1;
    }
    if (*last <= 1) {
	*ier += 2;
    }
    if (*first < 1) {
	*ier += 4;
    }
    if (*last > *np) {
	*ier += 8;
    }
    if (*nrbar < *np * (*np - 1) / 2) {
	*ier += 16;
    }
    if (*iwk < *last * 3) {
	*ier += 32;
    }
    if (*nbest > 0) {
	need = *nvmax * (*nvmax + 1) / 2;
	if (*ir < *nvmax) {
	    *ier += 64;
	}
	if (*il < need) {
	    *ier += 128;
	}
    }
    if (*fin < *fout || *fin <= zero) {
	*ier += 256;
    }
    if (*nobs <= *np) {
	*ier += 512;
    }
    if (*ier != 0) {
	return 0;
    }

/*     EPS approximates the smallest quantity such that the calculated */
/*     value of (1 + EPS) is > 1.   It is used to test for a perfect fit 
*/
/*     (RSS = 0). */

L10:
    if (one + eps <= one) {
	eps += eps;
	goto L10;
    }

/*     SIZE = number of variables in the current subset */

    *size = *first - 1;
    j1 = *last + 1;
    j2 = *last + j1;

/*     Find the best variable to add next */

L20:
    i__1 = *size + 1;
    add1_(np, nrbar, &d[1], &rbar[1], &thetab[1], &i__1, last, &tol[1], &wk[1]
	    , &wk[j1], &wk[j2], &smax, &jmax, ier);
    if (*nbest > 0) {
	i__1 = *size + 1;
	exadd1_(&i__1, &rss[1], &bound[1], nvmax, &ress[ress_offset], ir, 
		nbest, &lopt[lopt_offset], il, &vorder[1], &smax, &jmax, &wk[
		1], &wk[j1], last);
    }
    s_wsle(&io___92);
    do_lio(&c__9, &c__1, "Best variable to add: ", 22L);
    do_lio(&c__3, &c__1, (char *)&vorder[jmax], (ftnlen)sizeof(integer));
    e_wsle();

/*     Calculate 'F-to-enter' value */

    if (*size > 0) {
	base = rss[*size];
    } else {
	base = rss[1] + wk[1];
    }
    var = (base - smax) / (*nobs - *size - 1);
    if (var < eps * base) {
	*ier = -1;
	f = zero;
    } else {
	f = smax / var;
    }
    s_wsfe(&io___96);
    do_fio(&c__1, (char *)&f, (ftnlen)sizeof(doublereal));
    e_wsfe();

/*     Exit if F < FIN or IER < 0 (perfect fit) */

    if (f < *fin || *ier < 0) {
	return 0;
    }

/*     Add the variable to the subset (in position FIRST). */

    ++(*size);
    if (jmax > *first) {
	vmove_(np, nrbar, &vorder[1], &d[1], &rbar[1], &thetab[1], &rss[1], &
		jmax, first, &tol[1], ier);
    }

/*     See whether a variable entered earlier can be deleted now. */

L30:
    if (*size <= *first) {
	goto L20;
    }
    i__1 = *first + 1;
    drop1_(np, nrbar, &d[1], &rbar[1], &thetab[1], &i__1, size, &tol[1], &wk[
	    1], &wk[j1], &smin, &jmin, ier);
    var = rss[*size] / (*nobs - *size);
    f = smin / var;
    s_wsfe(&io___99);
    do_fio(&c__1, (char *)&vorder[jmin], (ftnlen)sizeof(integer));
    do_fio(&c__1, (char *)&f, (ftnlen)sizeof(doublereal));
    e_wsfe();
    if (f < *fout) {
	vmove_(np, nrbar, &vorder[1], &d[1], &rbar[1], &thetab[1], &rss[1], &
		jmin, size, &tol[1], ier);
	if (*nbest > 0) {
	    i__1 = *size - 1;
	    for (i = jmin; i <= i__1; ++i) {
/* L40: */
		report_(&i, &rss[i], &bound[1], nvmax, &ress[ress_offset], ir,
			 nbest, &lopt[lopt_offset], il, &vorder[1]);
	    }
	}
	--(*size);
	goto L30;
    }

    goto L20;
} /* efroym_ */

/* Subroutine */ int regcf_(np, nrbar, d, rbar, thetab, tol, beta, nreq, ier)
integer *np, *nrbar;
doublereal *d, *rbar, *thetab, *tol, *beta;
integer *nreq, *ier;
{
    /* Initialized data */

    static doublereal zero = 0.;

    /* System generated locals */
    integer i__1;

    /* Builtin functions */
    double sqrt();

    /* Local variables */
    static integer i, j, nextr;


/*     ALGORITHM AS274  APPL. STATIST. (1992) VOL 41, NO. x */

/*     Modified version of AS75.4 to calculate regression coefficients */
/*     for the first NREQ variables, given an orthogonal reduction from */
/*     AS75.1. */


/*     Local variables */


    /* Parameter adjustments */
    --beta;
    --tol;
    --thetab;
    --d;
    --rbar;

    /* Function Body */

/*     Some checks. */

    *ier = 0;
    if (*np < 1) {
	*ier = 1;
    }
    if (*nrbar < *np * (*np - 1) / 2) {
	*ier += 2;
    }
    if (*nreq < 1 || *nreq > *np) {
	*ier += 4;
    }
    if (*ier != 0) {
	return 0;
    }

    for (i = *nreq; i >= 1; --i) {
	if (sqrt(d[i]) < tol[i]) {
	    beta[i] = zero;
	    d[i] = zero;
	    goto L20;
	}
	beta[i] = thetab[i];
	nextr = (i - 1) * (*np + *np - i) / 2 + 1;
	i__1 = *nreq;
	for (j = i + 1; j <= i__1; ++j) {
	    beta[i] -= rbar[nextr] * beta[j];
	    ++nextr;
/* L10: */
	}
L20:
	;
    }

    return 0;
} /* regcf_ */


/* Subroutine */ int sing_(np, nrbar, d, rbar, thetab, sserr, tol, lindep, 
	work, ier)
integer *np, *nrbar;
doublereal *d, *rbar, *thetab, *sserr, *tol;
logical *lindep;
doublereal *work;
integer *ier;
{
    /* Initialized data */

    static doublereal zero = 0.;

    /* System generated locals */
    integer i__1, i__2;
    doublereal d__1;

    /* Builtin functions */
    double sqrt();

    /* Local variables */
    static doublereal temp;
    extern /* Subroutine */ int includ_();
    static integer nc2, col, pos, row, pos2;


/*     ALGORITHM AS274  APPL. STATIST. (1992) VOL.41, NO. 2 */

/*     Checks for singularities, reports, and adjusts orthogonal */
/*     reductions produced by AS75.1. */


/*     Local variables */


    /* Parameter adjustments */
    --work;
    --lindep;
    --tol;
    --thetab;
    --d;
    --rbar;

    /* Function Body */

/*     Check input parameters */

    *ier = 0;
    if (*np < 1) {
	*ier = 1;
    }
    if (*nrbar < *np * (*np - 1) / 2) {
	*ier += 2;
    }
    if (*ier != 0) {
	return 0;
    }

    i__1 = *np;
    for (col = 1; col <= i__1; ++col) {
/* L10: */
	work[col] = sqrt(d[col]);
    }

    i__1 = *np;
    for (col = 1; col <= i__1; ++col) {

/*     Set elements within RBAR to zero if they are less than TOL(COL)
 in */
/*     absolute value after being scaled by the square root of their r
ow */
/*     multiplier. */

	temp = tol[col];
	pos = col - 1;
	i__2 = col - 1;
	for (row = 1; row <= i__2; ++row) {
	    if ((d__1 = rbar[pos], abs(d__1)) * work[row] < temp) {
		rbar[pos] = zero;
	    }
	    pos = pos + *np - row - 1;
/* L30: */
	}

/*     If diagonal element is near zero, set it to zero, set appropria
te */
/*     element of LINDEP, and use INCLUD to augment the projections in
 */
/*     the lower rows of the orthogonalization. */

	lindep[col] = FALSE_;
	if (work[col] <= temp) {
	    lindep[col] = TRUE_;
	    --(*ier);
	    if (col < *np) {
		nc2 = *np - col;
		pos2 = pos + *np - col + 1;
		i__2 = nc2 * (nc2 - 1) / 2;
		includ_(&nc2, &i__2, &d[col], &rbar[pos + 1], &thetab[col], &
			d[col + 1], &rbar[pos2], &thetab[col + 1], sserr, ier)
			;
	    } else {
/* Computing 2nd power */
		d__1 = thetab[col];
		*sserr += d[col] * (d__1 * d__1);
	    }
	    d[col] = zero;
	    work[col] = zero;
	    thetab[col] = zero;
	}
/* L40: */
    }
    return 0;
} /* sing_ */


/* Subroutine */ int ss_(np, d, thetab, sserr, rss, ier)
integer *np;
doublereal *d, *thetab, *sserr, *rss;
integer *ier;
{
    /* System generated locals */
    doublereal d__1;

    /* Local variables */
    static integer i;
    static doublereal sum;


/*     ALGORITHM AS274  APPL. STATIST. (1992) VOL. 41, NO. 2 */

/*     Calculates partial residual sums of squares from an orthogonal */
/*     reduction from AS75.1. */


/*     Local variables */


/*     Some checks. */

    /* Parameter adjustments */
    --rss;
    --thetab;
    --d;

    /* Function Body */
    *ier = 0;
    if (*np < 1) {
	*ier = 1;
    }
    if (*ier != 0) {
	return 0;
    }

    sum = *sserr;
    rss[*np] = *sserr;
    for (i = *np; i >= 2; --i) {
/* Computing 2nd power */
	d__1 = thetab[i];
	sum += d[i] * (d__1 * d__1);
	rss[i - 1] = sum;
/* L10: */
    }
    return 0;
} /* ss_ */


/* Subroutine */ int tolset_(np, nrbar, d, rbar, tol, work, ier)
integer *np, *nrbar;
doublereal *d, *rbar, *tol, *work;
integer *ier;
{
    /* Initialized data */

    static doublereal eps = 1e-12;
    static doublereal zero = 0.;

    /* System generated locals */
    integer i__1, i__2, i__3;
    doublereal d__1;

    /* Builtin functions */
    double sqrt();

    /* Local variables */
    static integer col, pos;
    static doublereal sum;
    static integer row;


/*     ALGORITHM AS274  APPL. STATIST. (1992) VOL.41, NO. 2 */

/*     Sets up array TOL for testing for zeroes in an orthogonal */
/*     reduction formed using AS75.1. */


/*     Local variables. */


/*     EPS is a machine-dependent constant.   For compilers which use */
/*     the IEEE format for floating-point numbers, recommended values */
/*     are 1.E-06 for single precision and 1.D-12 for double precision. */

    /* Parameter adjustments */
    --work;
    --tol;
    --d;
    --rbar;

    /* Function Body */

/*     Some checks. */

    *ier = 0;
    if (*np < 1) {
	*ier = 1;
    }
    if (*nrbar < *np * (*np - 1) / 2) {
	*ier += 2;
    }
    if (*ier != 0) {
	return 0;
    }

/*     Set TOL(I) = sum of absolute values in column I of RBAR after */
/*     scaling each element by the square root of its row multiplier. */

    i__1 = *np;
    for (row = 1; row <= i__1; ++row) {
/* L10: */
	work[row] = sqrt(d[row]);
    }
    i__1 = *np;
    for (col = 1; col <= i__1; ++col) {
	pos = col - 1;
	if (col <= *np) {
	    sum = work[col];
	} else {
	    sum = zero;
	}
/* Computing MIN */
	i__3 = col - 1;
	i__2 = min(i__3,*np);
	for (row = 1; row <= i__2; ++row) {
	    sum += (d__1 = rbar[pos], abs(d__1)) * work[row];
	    pos = pos + *np - row - 1;
/* L20: */
	}
	tol[col] = eps * sum;
/* L30: */
    }

    return 0;
} /* tolset_ */


/* Subroutine */ int pcorr_(np, nrbar, d, rbar, thetab, sserr, in, work, 
	cormat, dimc, ycorr, ier)
integer *np, *nrbar;
doublereal *d, *rbar, *thetab, *sserr;
integer *in;
doublereal *work, *cormat;
integer *dimc;
doublereal *ycorr;
integer *ier;
{
    /* Initialized data */

    static doublereal zero = 0.;

    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer i, start, in1;
    extern /* Subroutine */ int cor_();


/*     ALGORITHM AS274  APPL. STATIST. (1992) VOL. 41, NO. 2 */

/*     Calculate partial correlations after the first IN variables */
/*     have been forced into the regression. */

/*     Auxiliary routine called: COR */


/*     Local variables. */


    /* Parameter adjustments */
    --ycorr;
    --work;
    --thetab;
    --d;
    --rbar;
    --cormat;

    /* Function Body */

/*     Some checks. */

    *ier = 0;
    if (*np < 1) {
	*ier = 1;
    }
    if (*nrbar < *np * (*np - 1) / 2) {
	*ier += 2;
    }
    if (*in < 0 || *in > *np - 1) {
	*ier += 4;
    }
    if (*dimc < (*np - *in) * (*np - *in - 1) / 2) {
	*ier += 8;
    }
    if (*ier != 0) {
	return 0;
    }

    start = *in * (*np + *np - *in - 1) / 2 + 1;
    in1 = *in + 1;
    i__1 = *np - *in;
    cor_(&i__1, &d[in1], &rbar[start], &thetab[in1], sserr, &work[1], &cormat[
	    1], &ycorr[1]);

/*     Check for zeroes. */

    i__1 = *np - *in;
    for (i = 1; i <= i__1; ++i) {
	if (work[i] <= zero) {
	    *ier = -i;
	}
/* L10: */
    }

    return 0;
} /* pcorr_ */


/* Subroutine */ int cor_(np, d, rbar, thetab, sserr, work, cormat, ycorr)
integer *np;
doublereal *d, *rbar, *thetab, *sserr, *work, *cormat, *ycorr;
{
    /* Initialized data */

    static doublereal zero = 0.;

    /* System generated locals */
    integer i__1, i__2, i__3;
    doublereal d__1;

    /* Builtin functions */
    double sqrt();

    /* Local variables */
    static integer diff;
    static doublereal sumy;
    static integer pos;
    static doublereal sum;
    static integer row, col1, col2, pos1, pos2;


/*     ALGORITHM AS274  APPL. STATIST. (1992) VOL. 41, NO. 2 */

/*     Calculate correlations from an orthogonal reduction.   This */
/*     routine will usually be called from PCORR, which will have */
/*     removed the appropriate number of rows at the start. */


/*     Local variables. */


    /* Parameter adjustments */
    --ycorr;
    --work;
    --thetab;
    --d;
    --rbar;
    --cormat;

    /* Function Body */

/*     Process by columns, including the projections of the dependent */
/*     variable (THETAB). */

    sumy = *sserr;
    i__1 = *np;
    for (row = 1; row <= i__1; ++row) {
/* L10: */
/* Computing 2nd power */
	d__1 = thetab[row];
	sumy += d[row] * (d__1 * d__1);
    }
    sumy = sqrt(sumy);
    pos = *np * (*np - 1) / 2;
    for (col1 = *np; col1 >= 1; --col1) {

/*     Calculate the length of column COL1. */

	sum = d[col1];
	pos1 = col1 - 1;
/* Computing MIN */
	i__2 = col1 - 1;
	i__1 = min(i__2,*np);
	for (row = 1; row <= i__1; ++row) {
/* Computing 2nd power */
	    d__1 = rbar[pos1];
	    sum += d[row] * (d__1 * d__1);
	    pos1 = pos1 + *np - row - 1;
/* L20: */
	}
	work[col1] = sqrt(sum);

/*     If SUM = 0, set all correlations with this variable to zero. */

	if (sum == zero) {
	    ycorr[col1] = zero;
	    i__1 = col1 + 1;
	    for (col2 = *np; col2 >= i__1; --col2) {
		cormat[pos] = zero;
		--pos;
/* L30: */
	    }
	    goto L70;
	}

/*     Form cross-products, then divide by product of column lengths. 
*/

	sum = d[col1] * thetab[col1];
	pos1 = col1 - 1;
/* Computing MIN */
	i__2 = col1 - 1;
	i__1 = min(i__2,*np);
	for (row = 1; row <= i__1; ++row) {
	    sum += d[row] * rbar[pos1] * thetab[row];
	    pos1 = pos1 + *np - row - 1;
/* L40: */
	}
	ycorr[col1] = sum / (sumy * work[col1]);

	i__1 = col1 + 1;
	for (col2 = *np; col2 >= i__1; --col2) {
	    if (work[col2] > zero) {
		pos1 = col1 - 1;
		pos2 = col2 - 1;
		diff = col2 - col1;
		sum = zero;
/* Computing MIN */
		i__3 = col1 - 1;
		i__2 = min(i__3,*np);
		for (row = 1; row <= i__2; ++row) {
		    sum += d[row] * rbar[pos1] * rbar[pos2];
		    pos1 = pos1 + *np - row - 1;
		    pos2 = pos1 + diff;
/* L50: */
		}
		sum += d[col1] * rbar[pos2];
		cormat[pos] = sum / (work[col1] * work[col2]);
	    } else {
		cormat[pos] = zero;
	    }
	    --pos;
/* L60: */
	}
L70:
	;
    }

    return 0;
} /* cor_ */


/* Subroutine */ int vmove_(np, nrbar, vorder, d, rbar, thetab, rss, from, to,
	 tol, ier)
integer *np, *nrbar, *vorder;
doublereal *d, *rbar, *thetab, *rss;
integer *from, *to;
doublereal *tol;
integer *ier;
{
    /* Initialized data */

    static doublereal zero = 0.;
    static doublereal one = 1.;

    /* System generated locals */
    integer i__1, i__2, i__3;
    doublereal d__1;

    /* Builtin functions */
    double sqrt();

    /* Local variables */
    static doublereal cbar, sbar;
    static integer last;
    static doublereal d1new, d2new;
    static integer m;
    static doublereal x, y, d1, d2;
    static integer first, m1, m2, mp1, inc, col, pos, row;


/*     ALGORITHM AS274 APPL. STATIST. (1992) VOL.41, NO. 2 */

/*     Move variable from position FROM to position TO in an */
/*     orthogonal reduction produced by AS75.1. */


/*     Local variables */


    /* Parameter adjustments */
    --tol;
    --rss;
    --thetab;
    --d;
    --vorder;
    --rbar;

    /* Function Body */

/*     Check input parameters */

    *ier = 0;
    if (*np < 1) {
	*ier = 1;
    }
    if (*nrbar < *np * (*np - 1) / 2) {
	*ier += 2;
    }
    if (*from < 1 || *from > *np) {
	*ier += 4;
    }
    if (*to < 1 || *to > *np) {
	*ier += 8;
    }
    if (*ier != 0) {
	return 0;
    }

    if (*from == *to) {
	return 0;
    }

    if (*from < *to) {
	first = *from;
	last = *to - 1;
	inc = 1;
    } else {
	first = *from - 1;
	last = *to;
	inc = -1;
    }
    i__1 = last;
    i__2 = inc;
    for (m = first; i__2 < 0 ? m >= i__1 : m <= i__1; m += i__2) {

/*     Find addresses of first elements of RBAR in rows M and (M+1). 
*/

	m1 = (m - 1) * (*np + *np - m) / 2 + 1;
	m2 = m1 + *np - m;
	mp1 = m + 1;
	if (m <= *np) {
	    d1 = d[m];
	    if (mp1 <= *np) {
		d2 = d[mp1];
	    } else {
		d2 = zero;
	    }
	} else {
	    d1 = zero;
	    d2 = zero;
	}

/*     Special cases. */

	if (d1 == zero && d2 == zero) {
	    goto L40;
	}
	x = rbar[m1];
	if (abs(x) * sqrt(d1) < tol[mp1]) {
	    x = zero;
	}
	if (d1 == zero || x == zero) {
	    d[m] = d2;
	    d[mp1] = d1;
	    rbar[m1] = zero;
	    i__3 = *np;
	    for (col = m + 2; col <= i__3; ++col) {
		++m1;
		x = rbar[m1];
		rbar[m1] = rbar[m2];
		rbar[m2] = x;
		++m2;
/* L10: */
	    }
	    x = thetab[m];
	    thetab[m] = thetab[mp1];
	    thetab[mp1] = x;
	    goto L40;
	} else if (d2 == zero) {
/* Computing 2nd power */
	    d__1 = x;
	    d[m] = d1 * (d__1 * d__1);
	    rbar[m1] = one / x;
	    i__3 = *np;
	    for (col = m + 2; col <= i__3; ++col) {
		++m1;
		rbar[m1] /= x;
/* L20: */
	    }
	    thetab[m] /= x;
	    goto L40;
	}

/*     Planar rotation in regular case. */

/* Computing 2nd power */
	d__1 = x;
	d1new = d2 + d1 * (d__1 * d__1);
	cbar = d2 / d1new;
	sbar = x * d1 / d1new;
	d2new = d1 * cbar;
	d[m] = d1new;
	d[mp1] = d2new;
	rbar[m1] = sbar;
	i__3 = *np;
	for (col = m + 2; col <= i__3; ++col) {
	    ++m1;
	    y = rbar[m1];
	    rbar[m1] = cbar * rbar[m2] + sbar * y;
	    rbar[m2] = y - x * rbar[m2];
	    ++m2;
/* L30: */
	}
	y = thetab[m];
	thetab[m] = cbar * thetab[mp1] + sbar * y;
	thetab[mp1] = y - x * thetab[mp1];

/*     Swap columns M and (M+1) down to row (M-1). */

L40:
	if (m == 1) {
	    goto L60;
	}
	pos = m;
	i__3 = m - 1;
	for (row = 1; row <= i__3; ++row) {
	    x = rbar[pos];
	    rbar[pos] = rbar[pos - 1];
	    rbar[pos - 1] = x;
	    pos = pos + *np - row - 1;
/* L50: */
	}

/*     Adjust variable order (VORDER), the tolerances (TOL) and */
/*     the vector of residual sums of squares (RSS). */

L60:
	m1 = vorder[m];
	vorder[m] = vorder[mp1];
	vorder[mp1] = m1;
	x = tol[m];
	tol[m] = tol[mp1];
	tol[mp1] = x;
/* Computing 2nd power */
	d__1 = thetab[mp1];
	rss[m] = rss[mp1] + d[mp1] * (d__1 * d__1);
/* L70: */
    }

    return 0;
} /* vmove_ */


/* Subroutine */ int reordr_(np, nrbar, vorder, d, rbar, thetab, rss, tol, 
	list, n, pos1, ier)
integer *np, *nrbar, *vorder;
doublereal *d, *rbar, *thetab, *rss, *tol;
integer *list, *n, *pos1, *ier;
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer next, i, j, l;
    extern /* Subroutine */ int vmove_();


/*     ALGORITHM AS274  APPL. STATIST. (1992) VOL.41, NO. 2 */

/*     Re-order the variables in an orthogonal reduction produced by */
/*     AS75.1 so that the N variables in LIST start at position POS1, */
/*     though will not necessarily be in the same order as in LIST. */
/*     Any variables in VORDER before position POS1 are not moved. */

/*     Auxiliary routine called: VMOVE */


/*     Local variables. */


/*     Check N. */

    /* Parameter adjustments */
    --tol;
    --rss;
    --thetab;
    --d;
    --vorder;
    --rbar;
    --list;

    /* Function Body */
    *ier = 0;
    if (*np < 1) {
	*ier = 1;
    }
    if (*nrbar < *np * (*np - 1) / 2) {
	*ier += 2;
    }
    if (*n < 1 || *n > *np + 1 - *pos1) {
	*ier += 4;
    }
    if (*ier != 0) {
	return 0;
    }

/*     Work through VORDER finding variables which are in LIST. */

    next = *pos1;
    i = *pos1;
L10:
    l = vorder[i];
    i__1 = *n;
    for (j = 1; j <= i__1; ++j) {
	if (l == list[j]) {
	    goto L40;
	}
/* L20: */
    }
L30:
    ++i;
    if (i <= *np) {
	goto L10;
    }

/*     If this point is reached, one or more variables in LIST has not */
/*     been found. */

    *ier = next - *n - 1;
    return 0;

/*     Variable L is in LIST; move it up to position NEXT if it is not */
/*     already there. */

L40:
    if (i > next) {
	vmove_(np, nrbar, &vorder[1], &d[1], &rbar[1], &thetab[1], &rss[1], &
		i, &next, &tol[1], ier);
    }
    ++next;
    if (next < *n + *pos1) {
	goto L30;
    }

    return 0;
} /* reordr_ */

/* Subroutine */ int includ_(np, nrbar, weight, xrow, yelem, d, rbar, thetab, 
	sserr, ier)
integer *np, *nrbar;
doublereal *weight, *xrow, *yelem, *d, *rbar, *thetab, *sserr;
integer *ier;
{
    /* Initialized data */

    static doublereal zero = 0.;

    /* System generated locals */
    integer i__1, i__2;

    /* Local variables */
    static doublereal cbar, sbar;
    static integer i, k;
    static doublereal w, y;
    static integer nextr;
    static doublereal di, xi, xk, dpi, wxi;


/*     ALGORITHM AS274  APPL. STATIST. (1992) VOL.41, NO. 2 */
/*     Modified from algorithm AS 75.1 */

/*     Calling this routine updates d, rbar, thetab and sserr by the */
/*     inclusion of xrow, yelem with the specified weight.   The number */
/*     of columns (variables) may exceed the number of rows (cases). */

/* **** WARNING: The elements of XROW are overwritten  **** */


/*     Local variables */


    /* Parameter adjustments */
    --thetab;
    --d;
    --xrow;
    --rbar;

    /* Function Body */

/*     Some checks. */

    *ier = 0;
    if (*np < 1) {
	*ier = 1;
    }
    if (*nrbar < *np * (*np - 1) / 2) {
	*ier += 2;
    }
    if (*ier != 0) {
	return 0;
    }

    w = *weight;
    y = *yelem;
    nextr = 1;
    i__1 = *np;
    for (i = 1; i <= i__1; ++i) {

/*     Skip unnecessary transformations.   Test on exact zeroes must b
e */
/*     used or stability can be destroyed. */

	if (w == zero) {
	    return 0;
	}
	xi = xrow[i];
	if (xi == zero) {
	    nextr = nextr + *np - i;
	    goto L30;
	}
	di = d[i];
	wxi = w * xi;
	dpi = di + wxi * xi;
	cbar = di / dpi;
	sbar = wxi / dpi;
	w = cbar * w;
	d[i] = dpi;
	if (i == *np) {
	    goto L20;
	}
	i__2 = *np;
	for (k = i + 1; k <= i__2; ++k) {
	    xk = xrow[k];
	    xrow[k] = xk - xi * rbar[nextr];
	    rbar[nextr] = cbar * rbar[nextr] + sbar * xk;
	    ++nextr;
/* L10: */
	}
L20:
	xk = y;
	y = xk - xi * thetab[i];
	thetab[i] = cbar * thetab[i] + sbar * xk;
L30:
	;
    }

/*     Y * SQRT(W) is now equal to Brown & Durbin's recursive residual. */

    *sserr += w * y * y;

    return 0;
} /* includ_ */

/* $$$C      PROGRAM SUBSET */
/* $$$C */
/* $$$C     Interactive program to perform regressions on subsets of */
/* $$$C     variables.   Max. no. of variables, excl. constant = 50. */
/* $$$C */
/* $$$C     Subroutines called:- */
/* $$$C     ADD1, BAKWRD, EFROYM, XHAUST, FORWRD, EXADD1, INITR, REGCF, */
/* $$$C     REORDR, LSORT, PCORR, REPORT, SEQREP, SHELL, SS, TOLSET. */
/* $$$C */
/* $$$C     Latest revision - 10 November 1993 */
/* $$$C */
/* $$$C     IMPLICIT NONE */
/* $$$      INTEGER MAXCOL, MAXSUB, MAXBST, MAXL, MAXR */
/* $$$      PARAMETER (MAXCOL=50, MAXSUB=25, MAXBST=20, MAXL=1000, */
/* $$$     +           MAXR=MAXSUB*MAXBST) */
/* $$$      INTEGER UDIM, IIW, IW */
/*$$$      PARAMETER (UDIM=MAXCOL*(MAXCOL+1)/2, IIW=3*MAXCOL, IW=UDIM+IIW)*/
/*$$$      CHARACTER FNAME*30, ANS, OPTION(22), VNAME(0:MAXCOL)*8, YNAME*8*/
/* $$$      LOGICAL LSEL, OK */
/* $$$      INTEGER LIN, LOUT, LPR, LOPT(MAXL), IWK(IIW), K, IRTN, LINE, */
/*$$$     +        ICONST, NCOLS, I1, IPOS, I, NOBS, VORDER(0:MAXCOL), IL,*/
/* $$$     +        NRBAR, NVMAX, NVMX, NBEST, IOPT, IER, NDF, J, NV, NB, */
/* $$$     +        IPRINT, IR, L, IPROC, FIRST, LAST, SIZE, M, ILNB */
/* $$$      DOUBLE PRECISION U(UDIM), EL(0:MAXCOL), RHS(0:MAXCOL), RESSQ, */
/*$$$     +        SSQ(0:MAXCOL), TOL(0:MAXCOL), BOUND(MAXSUB), RESS(MAXR),*/
/* $$$     +        WK(IW), TEMP, FIN, FOUT */
/* $$$      REAL VAR */
/* $$$      DATA OPTION/'C', 'c', 'F', 'f', 'B', 'b', 'R', 'r', 'E', 'e', */
/*$$$     +    'P', 'p', 'I', 'i', 'O', 'o', 'L', 'l', 'X', 'x', 'Q', 'q'/*/
/* $$$C */
/* $$$C     Set unit numbers for I/O in LIN & LOUT below. */
/* $$$C */
/* $$$      DATA LIN/5/, LOUT/6/ */
/* $$$C */
/* $$$C     Ask for name of the data set. */
/* $$$C */
/* $$$   10 WRITE(LOUT,9000) */
/* $$$ 9000 FORMAT(' Enter name of .RED file for data (e.g. B:myfile): ') */
/* $$$      READ(LIN,8000) FNAME */
/* $$$ 8000 FORMAT(A) */
/* $$$C */
/* $$$C     Add the .RED extension if necessary. */
/* $$$C */
/* $$$      IF (INDEX(FNAME, '.RED') .EQ. 0) THEN */
/* $$$	IPOS = INDEX(FNAME, ' ') */
/* $$$	IF (IPOS .EQ. 0 .OR. IPOS .GT. 11) THEN */
/* $$$	  WRITE(LOUT, 9010) FNAME */
/* $$$ 9010     FORMAT(' ** Illegal filename entered - ', A, ' **') */
/* $$$	  GO TO 10 */
/* $$$	END IF */
/* $$$	FNAME(IPOS: IPOS+3) = '.RED' */
/* $$$      END IF */
/* $$$C */
/* $$$C     Check that file exists. */
/* $$$C */
/* $$$      INQUIRE(FILE=FNAME, EXIST=OK) */
/* $$$      IF (.NOT. OK) THEN */
/* $$$	WRITE(LOUT, 9020) FNAME */
/* $$$ 9020   FORMAT(' ** File not found - ', A, ' **') */
/* $$$	GO TO 10 */
/* $$$      END IF */
/* $$$      OPEN(9, FILE=FNAME, STATUS='OLD', ACCESS='SEQUENTIAL', */
/* $$$     +        FORM='UNFORMATTED') */
/* $$$C */
/* $$$C     Read contents of file. */
/* $$$C */
/* $$$      READ(9) K, ICONST, NCOLS, NOBS, NRBAR, LSEL */
/* $$$      IF (ICONST .EQ. 0) THEN */
/* $$$	READ(9) YNAME, (VNAME(I),I=1,K) */
/* $$$	READ(9) (U(I),I=1,NRBAR), (EL(I),I=1,K), (RHS(I),I=1,K), RESSQ */
/* $$$      ELSE */
/* $$$	READ(9) YNAME, (VNAME(I),I=0,K) */
/* $$$	READ(9) (U(I),I=1,NRBAR), (EL(I),I=0,K), (RHS(I),I=0,K), RESSQ */
/* $$$      END IF */
/* $$$      I1 = 1 + ICONST */
/* $$$      WRITE(LOUT, 9030) K, NOBS, YNAME */
/* $$$ 9030 FORMAT(' No. of predictors = ', I3, 5X, 'No. of cases = ', I5/ */
/* $$$     +       ' Dependant variable is ', A) */
/* $$$      WRITE(LOUT, 9930) (I, VNAME(I),I=1,K) */
/* $$$C */
/* $$$C     Initially, all variables except the constant (if there is one) */
/* $$$C     are considered candidates for either inclusion or exclusion */
/* $$$C */
/* $$$      FIRST = I1 */
/* $$$      LAST = NCOLS */
/* $$$C */
/* $$$C     Set up array VORDER. */
/* $$$C */
/* $$$      DO 30 I = 0, K */
/* $$$	VORDER(I) = I */
/* $$$   30 CONTINUE */
/* $$$C */
/* $$$C     Ask for values of NVMAX & NBEST. */
/* $$$C */
/* $$$   50 WRITE(LOUT, 9040) */
/* $$$ 9040 FORMAT(' Enter max. size of subsets (excl. constant): ') */
/* $$$      READ(LIN, 8010) NVMAX */
/* $$$ 8010 FORMAT(I3) */
/* $$$      NVMX = NVMAX + ICONST */
/* $$$      IF(NVMX .LE. MAXBST) GO TO 70 */
/* $$$   60 WRITE(LOUT, 9050) */
/* $$$ 9050 FORMAT(' *** Too many, sorry, try again') */
/* $$$      GO TO 50 */
/* $$$   70 IL = NVMX*(NVMX + 1)/2 */
/* $$$      L = MIN(MAXL/IL, MAXR/NVMX, MAXBST) */
/* $$$      WRITE(LOUT, 9060) L, NVMAX */
/* $$$ 9060 FORMAT('+How many subsets of each size to be recorded ?'/ */
/* $$$     +       ' Max. = ', I4, ' with NVMAX =', I3, ' : ') */
/* $$$      READ(LIN, 8010) NBEST */
/* $$$      IF(NBEST .GT. L) GO TO 60 */
/* $$$C */
/* $$$C     Call TOLSET, SS & INITR to initialize arrays. */
/* $$$C */
/* $$$      IF (ICONST .EQ. 1) THEN */
/* $$$	CALL TOLSET(NCOLS, NRBAR, EL, U, TOL, WK, IER) */
/* $$$	CALL SS(NCOLS, EL, RHS, RESSQ, SSQ, IER) */
/* $$$	CALL INITR(NCOLS, NVMX, NBEST, BOUND, RESS, NVMX, LOPT, IL, */
/* $$$     +             VORDER, SSQ, IER) */
/* $$$      ELSE */
/* $$$	CALL TOLSET(NCOLS, NRBAR, EL(1), U, TOL(1), WK, IER) */
/* $$$	CALL SS(NCOLS, EL(1), RHS(1), RESSQ, SSQ(1), IER) */
/* $$$	CALL INITR(NCOLS, NVMX, NBEST, BOUND, RESS, NVMX, LOPT, IL, */
/* $$$     +             VORDER(1), SSQ(1), IER) */
/* $$$      END IF */
/* $$$      WRITE(LOUT, 9065) NCOLS, RESSQ */
/* $$$ 9065 FORMAT(' Initially NCOLS = ', I4,'  RESSQ = ', G13.5) */
/* $$$      IF (NOBS .GT. NCOLS) THEN */
/* $$$	NDF = NOBS - NCOLS */
/* $$$	VAR = RESSQ / NDF */
/* $$$	WRITE(*, 9068) VAR, NDF */
/* $$$ 9068   FORMAT(' Resid. variance estimate = ', g11.4, ' with ', i4, */
/* $$$     +         ' deg. of freedom'/) */
/* $$$      END IF */
/* $$$      IPROC = 0 */
/* $$$C */
/* $$$C     Display menu & ask for choice. */
/* $$$C */
/* $$$  100 WRITE(LOUT, 9070) */
/* $$$ 9070 FORMAT(' Options:-'/ */
/* $$$     1 ' C Corrlns. & partial corrlns.    F Forward selection'/ */
/* $$$     2 ' B Backward elimination           R Sequential replacement'/ */
/*$$$     3 ' E Efroymson stepwise             P Print summary of subsets'/*/
/* $$$     4 ' I Specify IN variables           O Specify OUT variables'/ */
/* $$$     5 ' L Least-squares regn.coeffs.     X Exhaustive search'/ */
/* $$$     6 ' Q Quit           ENTER YOUR OPTION : ') */
/* $$$      READ(LIN, *) ANS */
/* $$$C */
/* $$$C     Compare ANS with currently available options. */
/* $$$C */
/* $$$      DO 110 IOPT = 1,22 */
/* $$$	IF(ANS .EQ. OPTION(IOPT)) GO TO 120 */
/* $$$  110 CONTINUE */
/* $$$      WRITE(LOUT, 9080) ANS */
/* $$$ 9080 FORMAT(' Option ', A, ' not available') */
/* $$$      GO TO 100 */
/* $$$  120 L = (IOPT + 1)/2 */
/* $$$C */
/* $$$C             C    F    B    R    E    P    I    O    L    X    Q */
/*$$$      GO TO (200, 300, 400, 500, 550, 700, 800, 900, 250, 600, 850), L*/
/* $$$C */
/*$$$C-----------------------------------------------------------------------
*/
/* $$$C */
/* $$$C     Option 1. Correlations. */
/* $$$C */
/* $$$  200 WRITE(LOUT, 9200) */
/* $$$ 9200 FORMAT('+Do you want partial correlations ? (Y or N) ') */
/* $$$      NV = 0 */
/* $$$      READ(LIN, *) ANS */
/* $$$      IF(ANS .EQ. 'N' .OR. ANS .EQ. 'n') GO TO 210 */
/* $$$      IF(ANS .NE. 'Y' .AND. ANS .NE. 'y') GO TO 200 */
/* $$$      ASSIGN 210 TO IRTN */
/* $$$      WRITE(LOUT, 9210) */
/*$$$ 9210 FORMAT(' Partial corrlns. on how many variables (excl.const.) ? ')
*/
/* $$$      READ(LIN, 8010) NV */
/* $$$      IF(NV .GT. 0) GO TO 1000 */
/* $$$  210 WRITE(LOUT, 9220) */
/*$$$ 9220 FORMAT('+Correlations amongst all variables (A) or with Y only',*/
/* $$$     +       1X,'(Y) ? ') */
/* $$$      IOPT = 0 */
/* $$$      READ(LIN, *) ANS */
/* $$$      IF(ANS .EQ. 'A' .OR. ANS .EQ. 'a') IOPT = 1 */
/* $$$      NB = NV + ICONST */
/*$$$      CALL PCORR(NCOLS, NRBAR, EL, U, RHS, RESSQ, NB, WK(UDIM+NCOLS+1),*/
/* $$$     +           WK, IW, WK(UDIM+1), IER) */
/* $$$C */
/* $$$C     Display the (partial) correlations. */
/*$$$C     Correlations amongst the X-variables start at WK(1); correlations*/
/* $$$C     with Y start at WK(UDIM+1). */
/* $$$C */
/* $$$      CALL PRINTC(NCOLS, NB, WK, UDIM, WK(UDIM+1), VORDER, VNAME(1), */
/* $$$     +            YNAME, IOPT, LOUT, IER) */
/* $$$      GO TO 100 */
/* $$$C */
/*$$$C-----------------------------------------------------------------------
*/
/* $$$C */
/* $$$C     Option 9. Least - squares regression coefficients. */
/* $$$C */
/* $$$  250 WRITE(LOUT, 9850) */
/* $$$      READ(LIN, 8010) NV */
/* $$$      ASSIGN 260 TO IRTN */
/* $$$      GO TO 1000 */
/* $$$  260 IF (ICONST .EQ. 1) THEN */
/* $$$	CALL REGCF(NCOLS, NRBAR, EL, U, RHS, TOL, WK, NV, IER) */
/* $$$      ELSE */
/* $$$	CALL REGCF(NCOLS, NRBAR, EL(1), U, RHS(1), TOL(1), WK, NV, IER) */
/* $$$      END IF */
/* $$$      IER = -IER */
/* $$$      IF(IER .NE. 0) WRITE(LOUT, 9250) IER */
/* $$$ 9250 FORMAT(' Variables linearly dependant, rank deficiency =',I4) */
/* $$$      WRITE(LOUT, 9260)(VORDER(I-ICONST),WK(I),I=1,NV) */
/* $$$ 9260 FORMAT(' Least-squares regn.coeffs.', */
/*$$$     +  7(/1X, I5, G13.5, 2X, I5, G13.5, 2X, I5, G13.5, 2X, I5, G13.5))*/
/* $$$      WRITE(LOUT, 9270) SSQ(NV-ICONST) */
/* $$$ 9270 FORMAT(' Resid. sum of sq. =',G13.5/) */
/* $$$      GO TO 100 */
/* $$$C */
/*$$$C-----------------------------------------------------------------------
*/
/* $$$C */
/* $$$C     Option 2. Forward selection. */
/* $$$C */
/* $$$  300 IF (ICONST .EQ. 1) THEN */
/* $$$	CALL FORWRD(NCOLS, NRBAR, EL, U, RHS, FIRST, LAST, VORDER, TOL, */
/*$$$     +      SSQ, BOUND, NVMX, RESS, NVMX, NBEST, LOPT, IL, WK, IW, IER)*/
/* $$$      ELSE */
/* $$$	CALL FORWRD(NCOLS, NRBAR, EL(1), U, RHS(1), FIRST, LAST, */
/*$$$     +      VORDER(1), TOL(1), SSQ(1), BOUND, NVMX, RESS, NVMX, NBEST,*/
/* $$$     +      LOPT, IL, WK, IW, IER) */
/* $$$      END IF */
/* $$$      NV = NVMX */
/* $$$      IF (IPROC .EQ. 2*(IPROC/2)) IPROC = IPROC + 1 */
/* $$$      GO TO 1100 */
/* $$$C */
/*$$$C-----------------------------------------------------------------------
*/
/* $$$C */
/* $$$C     Option 3. Backward elimination. */
/* $$$C */
/* $$$  400 IF (ICONST .EQ. 1) THEN */
/* $$$	CALL BAKWRD(NCOLS, NRBAR, EL, U, RHS, FIRST, LAST, VORDER, TOL, */
/*$$$     +     SSQ, BOUND, NVMX, RESS, NVMX, NBEST, LOPT, IL, WK, IW, IER)*/
/* $$$      ELSE */
/* $$$	CALL BAKWRD(NCOLS, NRBAR, EL(1), U, RHS(1), FIRST, LAST, */
/*$$$     +     VORDER(1), TOL(1), SSQ(1), BOUND, NVMX, RESS, NVMX, NBEST,*/
/* $$$     +     LOPT, IL, WK, IW, IER) */
/* $$$      END IF */
/* $$$      NV = LAST */
/* $$$      I = IPROC/2 */
/* $$$      IF (I .EQ. 2*(I/2)) IPROC = IPROC + 2 */
/* $$$      GO TO 1100 */
/* $$$C */
/*$$$C-----------------------------------------------------------------------
*/
/* $$$C */
/* $$$C     Option 4. Sequential replacement. */
/* $$$C */
/* $$$  500 IF (ICONST .EQ. 1) THEN */
/* $$$	CALL SEQREP(NCOLS, NRBAR, EL, U, RHS, FIRST, LAST, VORDER, TOL, */
/*$$$     +     SSQ, BOUND, NVMX, RESS, NVMX, NBEST, LOPT, IL, WK, IW, IER)*/
/* $$$      ELSE */
/* $$$	CALL SEQREP(NCOLS, NRBAR, EL(1), U, RHS(1), FIRST, LAST, */
/*$$$     +     VORDER(1), TOL(1), SSQ(1), BOUND, NVMX, RESS, NVMX, NBEST,*/
/* $$$     +     LOPT, IL, WK, IW, IER) */
/* $$$      END IF */
/* $$$      I = IPROC/8 */
/* $$$      IF (I .EQ. 2*(I/2)) IPROC = IPROC + 8 */
/* $$$      NV = NVMX */
/* $$$      GO TO 1100 */
/* $$$C */
/*$$$C-----------------------------------------------------------------------
*/
/* $$$C */
/* $$$C     Option 5. Efroymson (stepwise) */
/* $$$C */
/* $$$  550 WRITE(LOUT, 9550) */
/* $$$ 9550 FORMAT(' Enter F-to-enter value : ') */
/* $$$      READ(LIN, 8550) FIN */
/* $$$ 8550 FORMAT(F10.0) */
/* $$$      WRITE(LOUT, 9560) */
/* $$$ 9560 FORMAT(' Enter F-to-remove value : ') */
/* $$$      READ(LIN, 8550) FOUT */
/* $$$      IF (ICONST .EQ. 1) THEN */
/* $$$	CALL EFROYM(NCOLS, NRBAR, EL, U, RHS, FIRST, LAST, FIN, FOUT, */
/*$$$     +    SIZE, NOBS, VORDER, TOL, SSQ, BOUND, NVMX, RESS, NVMX, NBEST,*/
/* $$$     +    LOPT, IL, WK, IW, IER) */
/* $$$      ELSE */
/* $$$	CALL EFROYM(NCOLS, NRBAR, EL(1), U, RHS(1), FIRST, LAST, FIN, */
/*$$$     +   FOUT, SIZE, NOBS, VORDER(1), TOL(1), SSQ(1), BOUND, NVMX, RESS,
*/
/* $$$     +   NVMX, NBEST, LOPT, IL, WK, IW, IER) */
/* $$$      END IF */
/* $$$      IF (IER .NE. 0) THEN */
/* $$$	WRITE(LOUT, 9570) IER */
/* $$$ 9570   FORMAT(' Error code',I4,' returned by EFROYM') */
/* $$$	GO TO 100 */
/* $$$      ELSE */
/* $$$	NV = SIZE */
/* $$$	I = IPROC/4 */
/* $$$	IPROC = IPROC + 4 */
/* $$$	GO TO 1100 */
/* $$$      END IF */
/* $$$C */
/*$$$C-----------------------------------------------------------------------
*/
/* $$$C */
/* $$$C     Option 10. Exhaustive search. */
/* $$$C */
/* $$$  600  IF (ICONST .EQ. 1) THEN */
/* $$$	 CALL XHAUST(NCOLS, NRBAR, EL, U, RHS, FIRST, LAST, VORDER, TOL, */
/*$$$     +      SSQ, BOUND, NVMX, RESS, NVMX, NBEST, LOPT, IL, WK, IW, IWK,*/
/* $$$     +      IIW, IER) */
/* $$$      ELSE */
/* $$$	 CALL XHAUST(NCOLS, NRBAR, EL(1), U, RHS(1), FIRST, LAST, */
/* $$$     +      VORDER(1), TOL(1), +   SSQ(1), BOUND, NVMX, RESS, NVMX, */
/* $$$     +      NBEST, LOPT, IL, WK, IW, IWK, IIW, IER) */
/* $$$      END IF */
/* $$$      IF (IPROC .LT. 16) IPROC = IPROC + 16 */
/* $$$      GO TO 100 */
/* $$$C */
/*$$$C-----------------------------------------------------------------------
*/
/* $$$C */
/* $$$C     Option 6. Print summary of best subsets found so far. */
/* $$$C */
/* $$$  700 CALL LSORT(LOPT, IL, NBEST, NVMX) */
/* $$$      L = FIRST*(FIRST-1)/2 + 1 */
/* $$$      LINE = 1 */
/* $$$      M = FIRST - ICONST */
/* $$$      DO 730 NV = FIRST, NVMX */
/* $$$	WRITE(LOUT,9700) M */
/* $$$ 9700   FORMAT(20X,'Best subsets found of',I3,' variables') */
/* $$$	LINE = LINE + 1 */
/* $$$	DO 720 NB = 1,NBEST */
/* $$$	  J = (NB-1)*NVMX + NV */
/* $$$	  TEMP = RESS(J) */
/* $$$	  IF(TEMP .GT. 1.E+35) GO TO 720 */
/* $$$	  IPOS = L */
/* $$$	  DO 710 I = 1,NV */
/* $$$	    J = (NB-1)*IL + IPOS */
/* $$$	    IWK(I) = LOPT(J) */
/* $$$	    IPOS = IPOS + 1 */
/* $$$  710     CONTINUE */
/* $$$	    WRITE(LOUT,9710) TEMP,(IWK(I),I=FIRST,NV) */
/* $$$	  LINE = LINE + 1 + (NV-1)/10 */
/* $$$ 9710     FORMAT(' RSS =',G14.6,3X,'Variables:',10I4,4(/10X,10I4)) */
/* $$$  720   CONTINUE */
/* $$$	IF (LINE .GE. 25 - NB) THEN */
/* $$$	  PAUSE */
/* $$$	  LINE = 1 */
/* $$$	END IF */
/* $$$	L = L + NV */
/* $$$	M = M + 1 */
/* $$$  730 CONTINUE */
/* $$$      GO TO 100 */
/* $$$C */
/*$$$C----------------------------------------------------------------------*/
/* $$$C */
/* $$$C     Option 7. Force variables into models. */
/* $$$C */
/* $$$  800 WRITE(LOUT, 9800) */
/* $$$ 9800 FORMAT('+How many variables, excl. constant ? ') */
/* $$$      READ(LIN, 8010) NV */
/* $$$      ASSIGN 810 TO IRTN */
/* $$$      GO TO 1000 */
/* $$$  810 GO TO 1100 */
/* $$$C */
/*$$$C----------------------------------------------------------------------*/
/* $$$C */
/* $$$C     Option 11. Exit. */
/* $$$C */
/* $$$  850 IF (IPROC .EQ. 0) STOP */
/* $$$      WRITE(LOUT, 9860) */
/* $$$ 9860 FORMAT(' Do you want to save the best subsets found ? (Y/N) ') */
/* $$$      READ(LIN, *) ANS */
/* $$$      IF (ANS .EQ. 'Y' .OR. ANS .EQ. 'y') THEN */
/* $$$	REWIND(9) */
/* $$$	CALL LSORT(LOPT, IL, NBEST, NVMX) */
/* $$$	READ(9) K, ICONST, NCOLS, NOBS, NRBAR, LSEL */
/* $$$	IF (ICONST .EQ. 0) THEN */
/* $$$	  READ(9) YNAME, (VNAME(I),I=1,K) */
/* $$$	  READ(9) (U(I),I=1,NRBAR), (EL(I),I=1,K), (RHS(I),I=1,K), RESSQ */
/* $$$	ELSE */
/* $$$	  READ(9) YNAME, (VNAME(I),I=0,K) */
/* $$$	  READ(9) (U(I),I=1,NRBAR), (EL(I),I=0,K), (RHS(I),I=0,K), RESSQ */
/* $$$	END IF */
/* $$$	LSEL = .TRUE. */
/* $$$	REWIND(9) */
/* $$$	ILNB = IL*NBEST */
/* $$$	IR = NVMX*NBEST */
/* $$$	WRITE(9) K, ICONST, NCOLS, NOBS, NRBAR, LSEL */
/* $$$	IF (ICONST .EQ. 0) THEN */
/* $$$	  WRITE(9) YNAME, (VNAME(I),I=1,K) */
/* $$$	  WRITE(9) (U(I),I=1,NRBAR), (EL(I),I=1,K), (RHS(I),I=1,K), */
/* $$$     +              RESSQ */
/* $$$	ELSE */
/* $$$	  WRITE(9) YNAME, (VNAME(I),I=0,K) */
/* $$$	  WRITE(9) (U(I),I=1,NRBAR), (EL(I),I=0,K), (RHS(I),I=0,K), */
/* $$$     +              RESSQ */
/* $$$	END IF */
/* $$$	WRITE(9) NVMAX, NBEST, IL, ILNB, IR, IPROC */
/* $$$	WRITE(9) (LOPT(L),L=1,ILNB) */
/* $$$	WRITE(9) (RESS(I),I=1,IR) */
/* $$$      END IF */
/* $$$      STOP */
/* $$$C */
/*$$$C----------------------------------------------------------------------*/
/* $$$C */
/*$$$C     Simulated subroutine to force variables into starting positions.*/
/* $$$C     NV = no. of variables to be forced in. */
/* $$$C */
/* $$$ 1000 WRITE(LOUT, 9930) (I, VNAME(I),I = 1,K) */
/*$$$ 9930 FORMAT('+Variables & their numbers:', 10(/1X, 5(I3, 1X, A8, 3X)))*/
/* $$$      IF(NV .LE. 0) GO TO 100 */
/* $$$      WRITE(LOUT, 9920) */
/* $$$ 9920 FORMAT(' List variable nos. : ') */
/* $$$      READ(LIN, *) (IWK(I),I = 1,NV) */
/* $$$C */
/*$$$C     Find variables in VORDER which are in the input list and move up*/
/* $$$C     to the next available position. */
/* $$$C */
/* $$$      IF (ICONST .EQ. 1) THEN */
/* $$$	CALL REORDR(NCOLS, NRBAR, VORDER, EL, U, RHS, SSQ, TOL, IWK, NV, */
/* $$$     +            2, IER) */
/* $$$      ELSE */
/* $$$	CALL REORDR(NCOLS, NRBAR, VORDER(1), EL(1), U, RHS(1), SSQ(1), */
/* $$$     +              TOL(1), IWK, NV, 1, IER) */
/* $$$      END IF */
/* $$$      NV = NV + ICONST */
/* $$$      FIRST = NV + 1 */
/* $$$      GO TO IRTN,(210, 260, 810) */
/* $$$C */
/*$$$C----------------------------------------------------------------------*/
/* $$$C */
/* $$$C     Option 8. Force variables out of models. */
/* $$$C */
/* $$$  900 WRITE(LOUT, 9850) */
/* $$$ 9850 FORMAT('+How many variables ? ') */
/* $$$      READ(LIN, 8010) NV */
/* $$$      WRITE(LOUT, 9920) */
/* $$$      DO 910 I = 1, NV */
/* $$$  910 READ(LIN, *) IWK(I) */
/* $$$      LAST = NCOLS */
/* $$$      J = LAST */
/* $$$  920 L = VORDER(J) */
/* $$$      DO 930 M = 1, NV */
/* $$$	IF(L .EQ. IWK(M)) GO TO 940 */
/* $$$  930 CONTINUE */
/* $$$      GO TO 960 */
/* $$$  940 IF(J .EQ. LAST) GO TO 950 */
/*$$$      CALL VMOVE(NCOLS, NRBAR, VORDER, EL, U, RHS, SSQ, J, LAST, TOL,*/
/* $$$     +    IER) */
/* $$$  950 LAST = LAST - 1 */
/* $$$      IF(J .LT. FIRST) FIRST = FIRST - 1 */
/* $$$  960 J = J - 1 */
/* $$$      IF(J .GT. 0) GO TO 920 */
/* $$$      GO TO 100 */
/* $$$C */
/*$$$C----------------------------------------------------------------------*/
/* $$$C */
/* $$$C     Print current order of the first NV variables and their RSS's. */
/* $$$C */
/* $$$ 1100 WRITE(LOUT, 9900) */
/* $$$ 9900 FORMAT(' Order  Variable   Resid.sumsq.') */
/* $$$      DO 1110 I = 1-ICONST, NV-ICONST */
/* $$$	J = VORDER(I) */
/* $$$	WRITE(LOUT, 9910) I, VNAME(J), SSQ(I) */
/* $$$ 9910   FORMAT(I5, 3X, A8, 1X, G14.6) */
/* $$$ 1110 CONTINUE */
/* $$$      GO TO 100 */
/* $$$      END */
/* $$$C */
/* $$$C */
/* $$$C */
/* $$$C */
/* Subroutine */ int lsort_(lopt, il, nbest, nvmx)
integer *lopt, *il, *nbest, *nvmx;
{
    /* System generated locals */
    integer lopt_dim1, lopt_offset, i__1, i__2;

    /* Local variables */
    static integer temp, size;
    extern /* Subroutine */ int shell_();
    static integer start, col;


/*      Sort the variable numbers in LOPT into increasing order. */

/*      Latest revision - 12 February 1986 */


    /* Parameter adjustments */
    lopt_dim1 = *il;
    lopt_offset = lopt_dim1 + 1;
    lopt -= lopt_offset;

    /* Function Body */
    if (*nvmx < 2) {
	return 0;
    }
    i__1 = *nbest;
    for (col = 1; col <= i__1; ++col) {
	temp = lopt[col * lopt_dim1 + 2];
	if (temp > lopt[col * lopt_dim1 + 3]) {
	    lopt[col * lopt_dim1 + 2] = lopt[col * lopt_dim1 + 3];
	    lopt[col * lopt_dim1 + 3] = temp;
	}
	if (*il <= 3) {
	    goto L20;
	}
	start = 4;
	i__2 = *nvmx;
	for (size = 3; size <= i__2; ++size) {
	    shell_(&lopt[start + col * lopt_dim1], &size);
	    start += size;
/* L10: */
	}
L20:
	;
    }
    return 0;
} /* lsort_ */





/* Subroutine */ int shell_(l, n)
integer *l, *n;
{
    /* System generated locals */
    integer i__1;

    /* Local variables */
    static integer incr, temp, i1, i2, start, it, end, new__;


/*      Perform a SHELL-sort on integer array L, sorting into */
/*      increasing order. */

/*      Latest revision - 12 February 1986 */


    /* Parameter adjustments */
    --l;

    /* Function Body */
    incr = *n;
L10:
    incr /= 3;
    if (incr == incr / 2 << 1) {
	++incr;
    }
    i__1 = incr;
    for (start = 1; start <= i__1; ++start) {
	end = *n;

/*      TEMP contains the element being compared; IT holds its current
 */
/*      location.   It is compared with the elements in locations */
/*      IT+INCR, IT+2.INCR, ... until a larger element is found.   All
 */
/*      smaller elements move INCR locations towards the start.   Afte
r */
/*      each time through the sequence, the END is decreased by INCR 
*/
/*      until END <= INCR. */

L20:
	i1 = start;
	temp = l[i1];
	it = i1;

/*      I2 = location of element NEW to be compared with TEMP. */
/*      Test I2 <= END. */

L30:
	i2 = i1 + incr;
	if (i2 > end) {
	    if (i1 > it) {
		l[i1] = temp;
	    }
	    end -= incr;
	    goto L40;
	}
	new__ = l[i2];

/*      If TEMP > NEW, move NEW to lower-numbered position. */

	if (temp > new__) {
	    l[i1] = new__;
	    i1 = i2;
	    goto L30;
	}

/*      TEMP <= NEW so do not swap. */
/*      Use NEW as the next TEMP. */

	if (i1 > it) {
	    l[i1] = temp;
	}
	i1 = i2;
	temp = new__;
	it = i1;
	goto L30;

/*      Repeat until END <= INCR. */

L40:
	if (end > incr) {
	    goto L20;
	}
/* L50: */
    }

/*      Repeat until INCR = 1. */

    if (incr > 1) {
	goto L10;
    }
    return 0;
} /* shell_ */

