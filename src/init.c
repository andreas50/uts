#include <stdlib.h>
#include <R_ext/Rdynload.h>

#include "utils.h"
#define CALLDEF(name, n) {#name, (DL_FUNC) &name, n}

static const R_CallMethodDef R_CallDef[] = {
   CALLDEF(num_leq_sorted, 5),
   CALLDEF(num_less_sorted, 5),
   CALLDEF(sorted_union, 7),
   {NULL, NULL, 0}
};


void R_init_uts(DllInfo *info)
{
  /*
   * R_useDynamicSymbols(info, FALSE) not working yet. Ggives error message "NULL value passed as symbol address" why trying
   * to call e.g. C_num_leq_sorted
   */
  R_registerRoutines(info, NULL, R_CallDef, NULL, NULL);
  //R_useDynamicSymbols(info, FALSE);
  R_useDynamicSymbols(info, TRUE);
  R_forceSymbols(info, TRUE);
}
