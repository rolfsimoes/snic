#ifdef _FORTIFY_SOURCE
#undef _FORTIFY_SOURCE
#endif

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

extern SEXP _snic(SEXP, SEXP, SEXP, SEXP);
extern SEXP _set_dim(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"snic_snic", (DL_FUNC) &_snic, 4},
    {"snic_set_dim", (DL_FUNC) &_set_dim, 2},
    {NULL, NULL, 0}
};

void R_init_snic(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
