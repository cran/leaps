#include <R_ext/RS.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Fortran calls */
extern void F77_NAME(bakwrd)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(forwrd)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(initr)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(makeqr)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(regcf)(void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(reordr)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(seqrep)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(sing)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(ssleaps)(void *, void *, void *, void *, void *, void *);
extern void F77_NAME(tolset)(void *, void *, void *, void *, void *, void *, void *);
extern void F77_NAME(xhaust)(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);

static const R_FortranMethodDef FortranEntries[] = {
    {"bakwrd",  (DL_FUNC) &F77_NAME(bakwrd),  20},
    {"forwrd",  (DL_FUNC) &F77_NAME(forwrd),  20},
    {"initr",   (DL_FUNC) &F77_NAME(initr),   11},
    {"makeqr",  (DL_FUNC) &F77_NAME(makeqr),  10},
    {"regcf",   (DL_FUNC) &F77_NAME(regcf),    9},
    {"reordr",  (DL_FUNC) &F77_NAME(reordr),  12},
    {"seqrep",  (DL_FUNC) &F77_NAME(seqrep),  20},
    {"sing",    (DL_FUNC) &F77_NAME(sing),    10},
    {"ssleaps", (DL_FUNC) &F77_NAME(ssleaps),  6},
    {"tolset",  (DL_FUNC) &F77_NAME(tolset),   7},
    {"xhaust",  (DL_FUNC) &F77_NAME(xhaust),  22},
    {NULL, NULL, 0}
};

void R_init_leaps(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, NULL, FortranEntries, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
