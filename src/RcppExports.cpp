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
// pmin2
NumericVector pmin2(NumericVector x1, NumericVector x2);
static SEXP _geovctrs_pmin2_try(SEXP x1SEXP, SEXP x2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x1(x1SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type x2(x2SEXP);
    rcpp_result_gen = Rcpp::wrap(pmin2(x1, x2));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _geovctrs_pmin2(SEXP x1SEXP, SEXP x2SEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_geovctrs_pmin2_try(x1SEXP, x2SEXP));
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
// pmax2
NumericVector pmax2(NumericVector x1, NumericVector x2);
static SEXP _geovctrs_pmax2_try(SEXP x1SEXP, SEXP x2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x1(x1SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type x2(x2SEXP);
    rcpp_result_gen = Rcpp::wrap(pmax2(x1, x2));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _geovctrs_pmax2(SEXP x1SEXP, SEXP x2SEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_geovctrs_pmax2_try(x1SEXP, x2SEXP));
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
// geovctrs_cpp_bbox
SEXP geovctrs_cpp_bbox(SEXP data, bool naRm);
static SEXP _geovctrs_geovctrs_cpp_bbox_try(SEXP dataSEXP, SEXP naRmSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< SEXP >::type data(dataSEXP);
    Rcpp::traits::input_parameter< bool >::type naRm(naRmSEXP);
    rcpp_result_gen = Rcpp::wrap(geovctrs_cpp_bbox(data, naRm));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _geovctrs_geovctrs_cpp_bbox(SEXP dataSEXP, SEXP naRmSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_geovctrs_geovctrs_cpp_bbox_try(dataSEXP, naRmSEXP));
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
// geovctrs_cpp_envelope
SEXP geovctrs_cpp_envelope(SEXP data, bool naRm);
static SEXP _geovctrs_geovctrs_cpp_envelope_try(SEXP dataSEXP, SEXP naRmSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< SEXP >::type data(dataSEXP);
    Rcpp::traits::input_parameter< bool >::type naRm(naRmSEXP);
    rcpp_result_gen = Rcpp::wrap(geovctrs_cpp_envelope(data, naRm));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _geovctrs_geovctrs_cpp_envelope(SEXP dataSEXP, SEXP naRmSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_geovctrs_geovctrs_cpp_envelope_try(dataSEXP, naRmSEXP));
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
// geovctrs_cpp_has_missing
LogicalVector geovctrs_cpp_has_missing(SEXP x);
static SEXP _geovctrs_geovctrs_cpp_has_missing_try(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(geovctrs_cpp_has_missing(x));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _geovctrs_geovctrs_cpp_has_missing(SEXP xSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_geovctrs_geovctrs_cpp_has_missing_try(xSEXP));
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
// geovctrs_cpp_is_finite
LogicalVector geovctrs_cpp_is_finite(SEXP x);
static SEXP _geovctrs_geovctrs_cpp_is_finite_try(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(geovctrs_cpp_is_finite(x));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _geovctrs_geovctrs_cpp_is_finite(SEXP xSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_geovctrs_geovctrs_cpp_is_finite_try(xSEXP));
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
// geovctrs_cpp_is_empty
LogicalVector geovctrs_cpp_is_empty(SEXP data);
static SEXP _geovctrs_geovctrs_cpp_is_empty_try(SEXP dataSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< SEXP >::type data(dataSEXP);
    rcpp_result_gen = Rcpp::wrap(geovctrs_cpp_is_empty(data));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _geovctrs_geovctrs_cpp_is_empty(SEXP dataSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_geovctrs_geovctrs_cpp_is_empty_try(dataSEXP));
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
// geovctrs_cpp_parse
CharacterVector geovctrs_cpp_parse(SEXP data);
static SEXP _geovctrs_geovctrs_cpp_parse_try(SEXP dataSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< SEXP >::type data(dataSEXP);
    rcpp_result_gen = Rcpp::wrap(geovctrs_cpp_parse(data));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _geovctrs_geovctrs_cpp_parse(SEXP dataSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_geovctrs_geovctrs_cpp_parse_try(dataSEXP));
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
// geovctrs_cpp_get_srid
IntegerVector geovctrs_cpp_get_srid(SEXP x);
static SEXP _geovctrs_geovctrs_cpp_get_srid_try(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(geovctrs_cpp_get_srid(x));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _geovctrs_geovctrs_cpp_get_srid(SEXP xSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_geovctrs_geovctrs_cpp_get_srid_try(xSEXP));
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
// geovctrs_cpp_set_srid
SEXP geovctrs_cpp_set_srid(SEXP x, IntegerVector srid);
static SEXP _geovctrs_geovctrs_cpp_set_srid_try(SEXP xSEXP, SEXP sridSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type srid(sridSEXP);
    rcpp_result_gen = Rcpp::wrap(geovctrs_cpp_set_srid(x, srid));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _geovctrs_geovctrs_cpp_set_srid(SEXP xSEXP, SEXP sridSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_geovctrs_geovctrs_cpp_set_srid_try(xSEXP, sridSEXP));
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
// geovctrs_cpp_geom_type_id
IntegerVector geovctrs_cpp_geom_type_id(SEXP x);
static SEXP _geovctrs_geovctrs_cpp_geom_type_id_try(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(geovctrs_cpp_geom_type_id(x));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _geovctrs_geovctrs_cpp_geom_type_id(SEXP xSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_geovctrs_geovctrs_cpp_geom_type_id_try(xSEXP));
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
// geovctrs_cpp_n_geometries
IntegerVector geovctrs_cpp_n_geometries(SEXP x);
static SEXP _geovctrs_geovctrs_cpp_n_geometries_try(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(geovctrs_cpp_n_geometries(x));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _geovctrs_geovctrs_cpp_n_geometries(SEXP xSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_geovctrs_geovctrs_cpp_n_geometries_try(xSEXP));
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
// geovctrs_cpp_n_coordinates
IntegerVector geovctrs_cpp_n_coordinates(SEXP x);
static SEXP _geovctrs_geovctrs_cpp_n_coordinates_try(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(geovctrs_cpp_n_coordinates(x));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _geovctrs_geovctrs_cpp_n_coordinates(SEXP xSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_geovctrs_geovctrs_cpp_n_coordinates_try(xSEXP));
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
// geovctrs_cpp_coordinate_dimensions
IntegerVector geovctrs_cpp_coordinate_dimensions(SEXP x);
static SEXP _geovctrs_geovctrs_cpp_coordinate_dimensions_try(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(geovctrs_cpp_coordinate_dimensions(x));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _geovctrs_geovctrs_cpp_coordinate_dimensions(SEXP xSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_geovctrs_geovctrs_cpp_coordinate_dimensions_try(xSEXP));
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
// geovctrs_cpp_first_coordinate
SEXP geovctrs_cpp_first_coordinate(SEXP x);
static SEXP _geovctrs_geovctrs_cpp_first_coordinate_try(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(geovctrs_cpp_first_coordinate(x));
    return rcpp_result_gen;
END_RCPP_RETURN_ERROR
}
RcppExport SEXP _geovctrs_geovctrs_cpp_first_coordinate(SEXP xSEXP) {
    SEXP rcpp_result_gen;
    {
        Rcpp::RNGScope rcpp_rngScope_gen;
        rcpp_result_gen = PROTECT(_geovctrs_geovctrs_cpp_first_coordinate_try(xSEXP));
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
// geovctrs_cpp_test_buffer2
SEXP geovctrs_cpp_test_buffer2(SEXP data, SEXP ptype);
RcppExport SEXP _geovctrs_geovctrs_cpp_test_buffer2(SEXP dataSEXP, SEXP ptypeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type data(dataSEXP);
    Rcpp::traits::input_parameter< SEXP >::type ptype(ptypeSEXP);
    rcpp_result_gen = Rcpp::wrap(geovctrs_cpp_test_buffer2(data, ptype));
    return rcpp_result_gen;
END_RCPP
}
// geovctrs_cpp_test_buffer2_bad_provider
SEXP geovctrs_cpp_test_buffer2_bad_provider(SEXP data, SEXP ptype);
RcppExport SEXP _geovctrs_geovctrs_cpp_test_buffer2_bad_provider(SEXP dataSEXP, SEXP ptypeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type data(dataSEXP);
    Rcpp::traits::input_parameter< SEXP >::type ptype(ptypeSEXP);
    rcpp_result_gen = Rcpp::wrap(geovctrs_cpp_test_buffer2_bad_provider(data, ptype));
    return rcpp_result_gen;
END_RCPP
}
// geovctrs_cpp_test_buffer2_bad_exporter
SEXP geovctrs_cpp_test_buffer2_bad_exporter(SEXP data, SEXP ptype);
RcppExport SEXP _geovctrs_geovctrs_cpp_test_buffer2_bad_exporter(SEXP dataSEXP, SEXP ptypeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type data(dataSEXP);
    Rcpp::traits::input_parameter< SEXP >::type ptype(ptypeSEXP);
    rcpp_result_gen = Rcpp::wrap(geovctrs_cpp_test_buffer2_bad_exporter(data, ptype));
    return rcpp_result_gen;
END_RCPP
}

// validate (ensure exported C++ functions exist before calling them)
static int _geovctrs_RcppExport_validate(const char* sig) { 
    static std::set<std::string> signatures;
    if (signatures.empty()) {
        signatures.insert("SEXP(*geovctrs_cpp_convert)(SEXP,SEXP)");
        signatures.insert("NumericVector(*pmin2)(NumericVector,NumericVector)");
        signatures.insert("NumericVector(*pmax2)(NumericVector,NumericVector)");
        signatures.insert("SEXP(*geovctrs_cpp_bbox)(SEXP,bool)");
        signatures.insert("SEXP(*geovctrs_cpp_envelope)(SEXP,bool)");
        signatures.insert("LogicalVector(*geovctrs_cpp_has_missing)(SEXP)");
        signatures.insert("LogicalVector(*geovctrs_cpp_is_finite)(SEXP)");
        signatures.insert("LogicalVector(*geovctrs_cpp_is_empty)(SEXP)");
        signatures.insert("CharacterVector(*geovctrs_cpp_parse)(SEXP)");
        signatures.insert("IntegerVector(*geovctrs_cpp_get_srid)(SEXP)");
        signatures.insert("SEXP(*geovctrs_cpp_set_srid)(SEXP,IntegerVector)");
        signatures.insert("IntegerVector(*geovctrs_cpp_geom_type_id)(SEXP)");
        signatures.insert("IntegerVector(*geovctrs_cpp_n_geometries)(SEXP)");
        signatures.insert("IntegerVector(*geovctrs_cpp_n_coordinates)(SEXP)");
        signatures.insert("IntegerVector(*geovctrs_cpp_coordinate_dimensions)(SEXP)");
        signatures.insert("SEXP(*geovctrs_cpp_first_coordinate)(SEXP)");
    }
    return signatures.find(sig) != signatures.end();
}

// registerCCallable (register entry points for exported C++ functions)
RcppExport SEXP _geovctrs_RcppExport_registerCCallable() { 
    R_RegisterCCallable("geovctrs", "_geovctrs_geovctrs_cpp_convert", (DL_FUNC)_geovctrs_geovctrs_cpp_convert_try);
    R_RegisterCCallable("geovctrs", "_geovctrs_pmin2", (DL_FUNC)_geovctrs_pmin2_try);
    R_RegisterCCallable("geovctrs", "_geovctrs_pmax2", (DL_FUNC)_geovctrs_pmax2_try);
    R_RegisterCCallable("geovctrs", "_geovctrs_geovctrs_cpp_bbox", (DL_FUNC)_geovctrs_geovctrs_cpp_bbox_try);
    R_RegisterCCallable("geovctrs", "_geovctrs_geovctrs_cpp_envelope", (DL_FUNC)_geovctrs_geovctrs_cpp_envelope_try);
    R_RegisterCCallable("geovctrs", "_geovctrs_geovctrs_cpp_has_missing", (DL_FUNC)_geovctrs_geovctrs_cpp_has_missing_try);
    R_RegisterCCallable("geovctrs", "_geovctrs_geovctrs_cpp_is_finite", (DL_FUNC)_geovctrs_geovctrs_cpp_is_finite_try);
    R_RegisterCCallable("geovctrs", "_geovctrs_geovctrs_cpp_is_empty", (DL_FUNC)_geovctrs_geovctrs_cpp_is_empty_try);
    R_RegisterCCallable("geovctrs", "_geovctrs_geovctrs_cpp_parse", (DL_FUNC)_geovctrs_geovctrs_cpp_parse_try);
    R_RegisterCCallable("geovctrs", "_geovctrs_geovctrs_cpp_get_srid", (DL_FUNC)_geovctrs_geovctrs_cpp_get_srid_try);
    R_RegisterCCallable("geovctrs", "_geovctrs_geovctrs_cpp_set_srid", (DL_FUNC)_geovctrs_geovctrs_cpp_set_srid_try);
    R_RegisterCCallable("geovctrs", "_geovctrs_geovctrs_cpp_geom_type_id", (DL_FUNC)_geovctrs_geovctrs_cpp_geom_type_id_try);
    R_RegisterCCallable("geovctrs", "_geovctrs_geovctrs_cpp_n_geometries", (DL_FUNC)_geovctrs_geovctrs_cpp_n_geometries_try);
    R_RegisterCCallable("geovctrs", "_geovctrs_geovctrs_cpp_n_coordinates", (DL_FUNC)_geovctrs_geovctrs_cpp_n_coordinates_try);
    R_RegisterCCallable("geovctrs", "_geovctrs_geovctrs_cpp_coordinate_dimensions", (DL_FUNC)_geovctrs_geovctrs_cpp_coordinate_dimensions_try);
    R_RegisterCCallable("geovctrs", "_geovctrs_geovctrs_cpp_first_coordinate", (DL_FUNC)_geovctrs_geovctrs_cpp_first_coordinate_try);
    R_RegisterCCallable("geovctrs", "_geovctrs_RcppExport_validate", (DL_FUNC)_geovctrs_RcppExport_validate);
    return R_NilValue;
}

static const R_CallMethodDef CallEntries[] = {
    {"_geovctrs_geovctrs_cpp_convert", (DL_FUNC) &_geovctrs_geovctrs_cpp_convert, 2},
    {"_geovctrs_pmin2", (DL_FUNC) &_geovctrs_pmin2, 2},
    {"_geovctrs_pmax2", (DL_FUNC) &_geovctrs_pmax2, 2},
    {"_geovctrs_geovctrs_cpp_bbox", (DL_FUNC) &_geovctrs_geovctrs_cpp_bbox, 2},
    {"_geovctrs_geovctrs_cpp_envelope", (DL_FUNC) &_geovctrs_geovctrs_cpp_envelope, 2},
    {"_geovctrs_geovctrs_cpp_has_missing", (DL_FUNC) &_geovctrs_geovctrs_cpp_has_missing, 1},
    {"_geovctrs_geovctrs_cpp_is_finite", (DL_FUNC) &_geovctrs_geovctrs_cpp_is_finite, 1},
    {"_geovctrs_geovctrs_cpp_is_empty", (DL_FUNC) &_geovctrs_geovctrs_cpp_is_empty, 1},
    {"_geovctrs_geovctrs_cpp_parse", (DL_FUNC) &_geovctrs_geovctrs_cpp_parse, 1},
    {"_geovctrs_geovctrs_cpp_get_srid", (DL_FUNC) &_geovctrs_geovctrs_cpp_get_srid, 1},
    {"_geovctrs_geovctrs_cpp_set_srid", (DL_FUNC) &_geovctrs_geovctrs_cpp_set_srid, 2},
    {"_geovctrs_geovctrs_cpp_geom_type_id", (DL_FUNC) &_geovctrs_geovctrs_cpp_geom_type_id, 1},
    {"_geovctrs_geovctrs_cpp_n_geometries", (DL_FUNC) &_geovctrs_geovctrs_cpp_n_geometries, 1},
    {"_geovctrs_geovctrs_cpp_n_coordinates", (DL_FUNC) &_geovctrs_geovctrs_cpp_n_coordinates, 1},
    {"_geovctrs_geovctrs_cpp_coordinate_dimensions", (DL_FUNC) &_geovctrs_geovctrs_cpp_coordinate_dimensions, 1},
    {"_geovctrs_geovctrs_cpp_first_coordinate", (DL_FUNC) &_geovctrs_geovctrs_cpp_first_coordinate, 1},
    {"_geovctrs_geovctrs_cpp_geos_version_runtime", (DL_FUNC) &_geovctrs_geovctrs_cpp_geos_version_runtime, 0},
    {"_geovctrs_geovctrs_cpp_geos_version_build", (DL_FUNC) &_geovctrs_geovctrs_cpp_geos_version_build, 0},
    {"_geovctrs_geovctrs_cpp_test_buffer2", (DL_FUNC) &_geovctrs_geovctrs_cpp_test_buffer2, 2},
    {"_geovctrs_geovctrs_cpp_test_buffer2_bad_provider", (DL_FUNC) &_geovctrs_geovctrs_cpp_test_buffer2_bad_provider, 2},
    {"_geovctrs_geovctrs_cpp_test_buffer2_bad_exporter", (DL_FUNC) &_geovctrs_geovctrs_cpp_test_buffer2_bad_exporter, 2},
    {"_geovctrs_RcppExport_registerCCallable", (DL_FUNC) &_geovctrs_RcppExport_registerCCallable, 0},
    {NULL, NULL, 0}
};

RcppExport void R_init_geovctrs(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
