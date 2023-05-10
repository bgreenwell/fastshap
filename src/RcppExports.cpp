// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// genOMat
LogicalMatrix genOMat(int num_rows, int num_cols);
RcppExport SEXP _fastshap_genOMat(SEXP num_rowsSEXP, SEXP num_colsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type num_rows(num_rowsSEXP);
    Rcpp::traits::input_parameter< int >::type num_cols(num_colsSEXP);
    rcpp_result_gen = Rcpp::wrap(genOMat(num_rows, num_cols));
    return rcpp_result_gen;
END_RCPP
}
// genFrankensteinMatrices
List genFrankensteinMatrices(arma::mat X, arma::mat W, arma::umat O, int feature);
RcppExport SEXP _fastshap_genFrankensteinMatrices(SEXP XSEXP, SEXP WSEXP, SEXP OSEXP, SEXP featureSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::mat >::type X(XSEXP);
    Rcpp::traits::input_parameter< arma::mat >::type W(WSEXP);
    Rcpp::traits::input_parameter< arma::umat >::type O(OSEXP);
    Rcpp::traits::input_parameter< int >::type feature(featureSEXP);
    rcpp_result_gen = Rcpp::wrap(genFrankensteinMatrices(X, W, O, feature));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_fastshap_genOMat", (DL_FUNC) &_fastshap_genOMat, 2},
    {"_fastshap_genFrankensteinMatrices", (DL_FUNC) &_fastshap_genFrankensteinMatrices, 4},
    {NULL, NULL, 0}
};

RcppExport void R_init_fastshap(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}