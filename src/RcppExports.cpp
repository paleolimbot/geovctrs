// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// cpp_convert
SEXP cpp_convert(SEXP data, SEXP ptype);
RcppExport SEXP _geovctrs_cpp_convert(SEXP dataSEXP, SEXP ptypeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type data(dataSEXP);
    Rcpp::traits::input_parameter< SEXP >::type ptype(ptypeSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_convert(data, ptype));
    return rcpp_result_gen;
END_RCPP
}
// pmin2
NumericVector pmin2(NumericVector x1, NumericVector x2);
RcppExport SEXP _geovctrs_pmin2(SEXP x1SEXP, SEXP x2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x1(x1SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type x2(x2SEXP);
    rcpp_result_gen = Rcpp::wrap(pmin2(x1, x2));
    return rcpp_result_gen;
END_RCPP
}
// pmax2
NumericVector pmax2(NumericVector x1, NumericVector x2);
RcppExport SEXP _geovctrs_pmax2(SEXP x1SEXP, SEXP x2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x1(x1SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type x2(x2SEXP);
    rcpp_result_gen = Rcpp::wrap(pmax2(x1, x2));
    return rcpp_result_gen;
END_RCPP
}
// cpp_envelope
SEXP cpp_envelope(SEXP data, bool naRm);
RcppExport SEXP _geovctrs_cpp_envelope(SEXP dataSEXP, SEXP naRmSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type data(dataSEXP);
    Rcpp::traits::input_parameter< bool >::type naRm(naRmSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_envelope(data, naRm));
    return rcpp_result_gen;
END_RCPP
}
// cpp_has_missing
LogicalVector cpp_has_missing(SEXP x);
RcppExport SEXP _geovctrs_cpp_has_missing(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_has_missing(x));
    return rcpp_result_gen;
END_RCPP
}
// cpp_is_finite
LogicalVector cpp_is_finite(SEXP x);
RcppExport SEXP _geovctrs_cpp_is_finite(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_is_finite(x));
    return rcpp_result_gen;
END_RCPP
}
// cpp_version_impl
std::string cpp_version_impl();
RcppExport SEXP _geovctrs_cpp_version_impl() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(cpp_version_impl());
    return rcpp_result_gen;
END_RCPP
}
// cpp_is_empty
LogicalVector cpp_is_empty(SEXP data);
RcppExport SEXP _geovctrs_cpp_is_empty(SEXP dataSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type data(dataSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_is_empty(data));
    return rcpp_result_gen;
END_RCPP
}
// cpp_has_z
LogicalVector cpp_has_z(SEXP data);
RcppExport SEXP _geovctrs_cpp_has_z(SEXP dataSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type data(dataSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_has_z(data));
    return rcpp_result_gen;
END_RCPP
}
// cpp_geom_type_id
IntegerVector cpp_geom_type_id(SEXP x);
RcppExport SEXP _geovctrs_cpp_geom_type_id(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_geom_type_id(x));
    return rcpp_result_gen;
END_RCPP
}
// cpp_get_srid
IntegerVector cpp_get_srid(SEXP x);
RcppExport SEXP _geovctrs_cpp_get_srid(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_get_srid(x));
    return rcpp_result_gen;
END_RCPP
}
// cpp_set_srid
SEXP cpp_set_srid(SEXP x, IntegerVector srid);
RcppExport SEXP _geovctrs_cpp_set_srid(SEXP xSEXP, SEXP sridSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type srid(sridSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_set_srid(x, srid));
    return rcpp_result_gen;
END_RCPP
}
// cpp_n_geometries
IntegerVector cpp_n_geometries(SEXP x);
RcppExport SEXP _geovctrs_cpp_n_geometries(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_n_geometries(x));
    return rcpp_result_gen;
END_RCPP
}
// cpp_n_coordinates
IntegerVector cpp_n_coordinates(SEXP x);
RcppExport SEXP _geovctrs_cpp_n_coordinates(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_n_coordinates(x));
    return rcpp_result_gen;
END_RCPP
}
// cpp_validate_provider
LogicalVector cpp_validate_provider(SEXP data);
RcppExport SEXP _geovctrs_cpp_validate_provider(SEXP dataSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type data(dataSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_validate_provider(data));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_geovctrs_cpp_convert", (DL_FUNC) &_geovctrs_cpp_convert, 2},
    {"_geovctrs_pmin2", (DL_FUNC) &_geovctrs_pmin2, 2},
    {"_geovctrs_pmax2", (DL_FUNC) &_geovctrs_pmax2, 2},
    {"_geovctrs_cpp_envelope", (DL_FUNC) &_geovctrs_cpp_envelope, 2},
    {"_geovctrs_cpp_has_missing", (DL_FUNC) &_geovctrs_cpp_has_missing, 1},
    {"_geovctrs_cpp_is_finite", (DL_FUNC) &_geovctrs_cpp_is_finite, 1},
    {"_geovctrs_cpp_version_impl", (DL_FUNC) &_geovctrs_cpp_version_impl, 0},
    {"_geovctrs_cpp_is_empty", (DL_FUNC) &_geovctrs_cpp_is_empty, 1},
    {"_geovctrs_cpp_has_z", (DL_FUNC) &_geovctrs_cpp_has_z, 1},
    {"_geovctrs_cpp_geom_type_id", (DL_FUNC) &_geovctrs_cpp_geom_type_id, 1},
    {"_geovctrs_cpp_get_srid", (DL_FUNC) &_geovctrs_cpp_get_srid, 1},
    {"_geovctrs_cpp_set_srid", (DL_FUNC) &_geovctrs_cpp_set_srid, 2},
    {"_geovctrs_cpp_n_geometries", (DL_FUNC) &_geovctrs_cpp_n_geometries, 1},
    {"_geovctrs_cpp_n_coordinates", (DL_FUNC) &_geovctrs_cpp_n_coordinates, 1},
    {"_geovctrs_cpp_validate_provider", (DL_FUNC) &_geovctrs_cpp_validate_provider, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_geovctrs(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
