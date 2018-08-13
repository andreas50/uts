// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// C_num_leq_sorted
Rcpp::IntegerVector C_num_leq_sorted(const Rcpp::NumericVector& a, const Rcpp::NumericVector& b, double tolerance);
RcppExport SEXP _uts_C_num_leq_sorted(SEXP aSEXP, SEXP bSEXP, SEXP toleranceSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Rcpp::NumericVector& >::type a(aSEXP);
    Rcpp::traits::input_parameter< const Rcpp::NumericVector& >::type b(bSEXP);
    Rcpp::traits::input_parameter< double >::type tolerance(toleranceSEXP);
    rcpp_result_gen = Rcpp::wrap(C_num_leq_sorted(a, b, tolerance));
    return rcpp_result_gen;
END_RCPP
}
// C_num_less_sorted
Rcpp::IntegerVector C_num_less_sorted(const Rcpp::NumericVector& a, const Rcpp::NumericVector& b, double tolerance);
RcppExport SEXP _uts_C_num_less_sorted(SEXP aSEXP, SEXP bSEXP, SEXP toleranceSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Rcpp::NumericVector& >::type a(aSEXP);
    Rcpp::traits::input_parameter< const Rcpp::NumericVector& >::type b(bSEXP);
    Rcpp::traits::input_parameter< double >::type tolerance(toleranceSEXP);
    rcpp_result_gen = Rcpp::wrap(C_num_less_sorted(a, b, tolerance));
    return rcpp_result_gen;
END_RCPP
}
// C_sorted_union
Rcpp::NumericVector C_sorted_union(const Rcpp::NumericVector& a, const Rcpp::NumericVector& b, double tolerance);
RcppExport SEXP _uts_C_sorted_union(SEXP aSEXP, SEXP bSEXP, SEXP toleranceSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Rcpp::NumericVector& >::type a(aSEXP);
    Rcpp::traits::input_parameter< const Rcpp::NumericVector& >::type b(bSEXP);
    Rcpp::traits::input_parameter< double >::type tolerance(toleranceSEXP);
    rcpp_result_gen = Rcpp::wrap(C_sorted_union(a, b, tolerance));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_uts_C_num_leq_sorted", (DL_FUNC) &_uts_C_num_leq_sorted, 3},
    {"_uts_C_num_less_sorted", (DL_FUNC) &_uts_C_num_less_sorted, 3},
    {"_uts_C_sorted_union", (DL_FUNC) &_uts_C_sorted_union, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_uts(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
