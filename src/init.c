#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

extern SEXP snic(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"snic", (DL_FUNC) &snic, 6},
    {NULL, NULL, 0}
};

void R_init_snic(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
