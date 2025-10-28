#ifdef _FORTIFY_SOURCE
#undef _FORTIFY_SOURCE
#endif

#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

extern SEXP _snic(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _seed_grid(SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"C_snic", (DL_FUNC) &_snic, 5},
    {"C_seed_grid", (DL_FUNC) &_seed_grid, 4},
    {NULL, NULL, 0}
};

void R_init_snic(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
