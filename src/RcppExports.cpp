// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// movingRMS
Rcpp::NumericVector movingRMS(Rcpp::NumericVector input_data, int intwind);
RcppExport SEXP _EMGhelpeR_movingRMS(SEXP input_dataSEXP, SEXP intwindSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type input_data(input_dataSEXP);
    Rcpp::traits::input_parameter< int >::type intwind(intwindSEXP);
    rcpp_result_gen = Rcpp::wrap(movingRMS(input_data, intwind));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_EMGhelpeR_movingRMS", (DL_FUNC) &_EMGhelpeR_movingRMS, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_EMGhelpeR(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
