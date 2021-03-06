#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
  Check these declarations against the C/Fortran source code.
*/
  
/* .Call calls */
extern SEXP _grattan_pmax3(SEXP, SEXP, SEXP);
extern SEXP _grattan_IncomeTax(SEXP, SEXP, SEXP);
extern SEXP _grattan_pmaxC(SEXP, SEXP);
extern SEXP _grattan_pmaxV(SEXP, SEXP);
extern SEXP _grattan_pminC(SEXP, SEXP);
extern SEXP _grattan_pminV(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
  {"_grattan_pmax3", (DL_FUNC) &_grattan_pmax3, 3},
  {"_grattan_IncomeTax", (DL_FUNC) &_grattan_IncomeTax, 3},
  {"_grattan_pmaxC", (DL_FUNC) &_grattan_pmaxC, 2},
  {"_grattan_pmaxV", (DL_FUNC) &_grattan_pmaxV, 2},
  {"_grattan_pminC", (DL_FUNC) &_grattan_pminC, 2},
  {"_grattan_pminV", (DL_FUNC) &_grattan_pminV, 2},
  {NULL, NULL, 0}
};

void R_init_grattan(DllInfo *dll)
{
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}