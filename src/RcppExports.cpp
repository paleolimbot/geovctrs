// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include "../inst/include/geovctrs.h"
#include <Rcpp.h>
#include <string>
#include <set>

using namespace Rcpp;

// geovctrs_cpp_convert
SEXP geovctrs_cpp_convert(SEXP data, SEXP ptype);
static SEXP _geovctrs_geovctrs_cpp_convert_try(SEXP dataSEXP, SEXP ptypeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< SEXP >::type data(dataSEXP);
    Rcpp::traits::input_parameter< SEXP >::type ptype(ptypeSEXP);
    rcpp_result_gen = Rcpp::wrap(geovctrs_cpp_convert(data, ptype));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _geovctrs_geovctrs_cpp_convert(SEXP dataSEXP, SEXP ptypeSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_geovctrs_geovctrs_cpp_convert_try(dataSEXP, ptypeSEXP));
    }
    Rboolean rcpp_isInterrupt_gen = Rf_inherits(rcpp_result_gen, "interrupted-error");
    if (rcpp_isInterrupt_gen) {
        UNPROTECT(1);
        Rf_onintr();
    }
    bool rcpp_isLongjump_gen = Rcpp::internal::isLongjumpSentinel(rcpp_result_gen);
    if (rcpp_isLongjump_gen) {
        Rcpp::internal::resumeJump(rcpp_result_gen);
    }
    Rboolean rcpp_isError_gen = Rf_inherits(rcpp_result_gen, "try-error");
    if (rcpp_isError_gen) {
        SEXP rcpp_msgSEXP_gen = Rf_asChar(rcpp_result_gen);
        UNPROTECT(1);
        Rf_error(CHAR(rcpp_msgSEXP_gen));
    }
    UNPROTECT(1);
    return rcpp_result_gen;
}
// geovctrs_cpp_geos_version_runtime
std::string geovctrs_cpp_geos_version_runtime();
RcppExport SEXP _geovctrs_geovctrs_cpp_geos_version_runtime() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(geovctrs_cpp_geos_version_runtime());
    return rcpp_result_gen;
END_RCPP
}
// geovctrs_cpp_geos_version_build
std::string geovctrs_cpp_geos_version_build();
RcppExport SEXP _geovctrs_geovctrs_cpp_geos_version_build() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(geovctrs_cpp_geos_version_build());
    return rcpp_result_gen;
END_RCPP
}

// validate (ensure exported C++ functions exist before calling them)
static int _geovctrs_RcppExport_validate(const char* sig) { 
    static std::set<std::string> signatures;
    if (signatures.empty()) {
        signatures.insert("SEXP(*geovctrs_cpp_convert)(SEXP,SEXP)");
    }
    return signatures.find(sig) != signatures.end();
}

// registerCCallable (register entry points for exported C++ functions)
RcppExport SEXP _geovctrs_RcppExport_registerCCallable() { 
    R_RegisterCCallable("geovctrs", "_geovctrs_geovctrs_cpp_convert", (DL_FUNC)_geovctrs_geovctrs_cpp_convert_try);
    R_RegisterCCallable("geovctrs", "_geovctrs_RcppExport_validate", (DL_FUNC)_geovctrs_RcppExport_validate);
    return R_NilValue;
}

static const R_CallMethodDef CallEntries[] = {
    {"_geovctrs_geovctrs_cpp_convert", (DL_FUNC) &_geovctrs_geovctrs_cpp_convert, 2},
    {"_geovctrs_geovctrs_cpp_geos_version_runtime", (DL_FUNC) &_geovctrs_geovctrs_cpp_geos_version_runtime, 0},
    {"_geovctrs_geovctrs_cpp_geos_version_build", (DL_FUNC) &_geovctrs_geovctrs_cpp_geos_version_build, 0},
    {"_geovctrs_RcppExport_registerCCallable", (DL_FUNC) &_geovctrs_RcppExport_registerCCallable, 0},
    {NULL, NULL, 0}
};

RcppExport void R_init_geovctrs(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
