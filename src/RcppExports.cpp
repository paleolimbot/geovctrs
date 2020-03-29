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
// cpp_intersection
SEXP cpp_intersection(SEXP dataLeft, SEXP dataRight, SEXP ptype);
RcppExport SEXP _geovctrs_cpp_intersection(SEXP dataLeftSEXP, SEXP dataRightSEXP, SEXP ptypeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type dataLeft(dataLeftSEXP);
    Rcpp::traits::input_parameter< SEXP >::type dataRight(dataRightSEXP);
    Rcpp::traits::input_parameter< SEXP >::type ptype(ptypeSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_intersection(dataLeft, dataRight, ptype));
    return rcpp_result_gen;
END_RCPP
}
// cpp_difference
SEXP cpp_difference(SEXP dataLeft, SEXP dataRight, SEXP ptype);
RcppExport SEXP _geovctrs_cpp_difference(SEXP dataLeftSEXP, SEXP dataRightSEXP, SEXP ptypeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type dataLeft(dataLeftSEXP);
    Rcpp::traits::input_parameter< SEXP >::type dataRight(dataRightSEXP);
    Rcpp::traits::input_parameter< SEXP >::type ptype(ptypeSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_difference(dataLeft, dataRight, ptype));
    return rcpp_result_gen;
END_RCPP
}
// cpp_sym_difference
SEXP cpp_sym_difference(SEXP dataLeft, SEXP dataRight, SEXP ptype);
RcppExport SEXP _geovctrs_cpp_sym_difference(SEXP dataLeftSEXP, SEXP dataRightSEXP, SEXP ptypeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type dataLeft(dataLeftSEXP);
    Rcpp::traits::input_parameter< SEXP >::type dataRight(dataRightSEXP);
    Rcpp::traits::input_parameter< SEXP >::type ptype(ptypeSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_sym_difference(dataLeft, dataRight, ptype));
    return rcpp_result_gen;
END_RCPP
}
// cpp_union
SEXP cpp_union(SEXP dataLeft, SEXP dataRight, SEXP ptype);
RcppExport SEXP _geovctrs_cpp_union(SEXP dataLeftSEXP, SEXP dataRightSEXP, SEXP ptypeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type dataLeft(dataLeftSEXP);
    Rcpp::traits::input_parameter< SEXP >::type dataRight(dataRightSEXP);
    Rcpp::traits::input_parameter< SEXP >::type ptype(ptypeSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_union(dataLeft, dataRight, ptype));
    return rcpp_result_gen;
END_RCPP
}
// cpp_unary_union
SEXP cpp_unary_union(SEXP dataLeft, SEXP ptype);
RcppExport SEXP _geovctrs_cpp_unary_union(SEXP dataLeftSEXP, SEXP ptypeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type dataLeft(dataLeftSEXP);
    Rcpp::traits::input_parameter< SEXP >::type ptype(ptypeSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_unary_union(dataLeft, ptype));
    return rcpp_result_gen;
END_RCPP
}
// cpp_coverage_union
SEXP cpp_coverage_union(SEXP dataLeft, SEXP ptype);
RcppExport SEXP _geovctrs_cpp_coverage_union(SEXP dataLeftSEXP, SEXP ptypeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type dataLeft(dataLeftSEXP);
    Rcpp::traits::input_parameter< SEXP >::type ptype(ptypeSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_coverage_union(dataLeft, ptype));
    return rcpp_result_gen;
END_RCPP
}
// cpp_clip_by_rect
SEXP cpp_clip_by_rect(SEXP dataLeft, NumericVector xmin, NumericVector ymin, NumericVector xmax, NumericVector ymax, SEXP to);
RcppExport SEXP _geovctrs_cpp_clip_by_rect(SEXP dataLeftSEXP, SEXP xminSEXP, SEXP yminSEXP, SEXP xmaxSEXP, SEXP ymaxSEXP, SEXP toSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type dataLeft(dataLeftSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type xmin(xminSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type ymin(yminSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type xmax(xmaxSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type ymax(ymaxSEXP);
    Rcpp::traits::input_parameter< SEXP >::type to(toSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_clip_by_rect(dataLeft, xmin, ymin, xmax, ymax, to));
    return rcpp_result_gen;
END_RCPP
}
// cpp_is_disjoint
LogicalVector cpp_is_disjoint(SEXP dataLeft, SEXP dataRight);
RcppExport SEXP _geovctrs_cpp_is_disjoint(SEXP dataLeftSEXP, SEXP dataRightSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type dataLeft(dataLeftSEXP);
    Rcpp::traits::input_parameter< SEXP >::type dataRight(dataRightSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_is_disjoint(dataLeft, dataRight));
    return rcpp_result_gen;
END_RCPP
}
// cpp_touches
LogicalVector cpp_touches(SEXP dataLeft, SEXP dataRight);
RcppExport SEXP _geovctrs_cpp_touches(SEXP dataLeftSEXP, SEXP dataRightSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type dataLeft(dataLeftSEXP);
    Rcpp::traits::input_parameter< SEXP >::type dataRight(dataRightSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_touches(dataLeft, dataRight));
    return rcpp_result_gen;
END_RCPP
}
// cpp_intersects
LogicalVector cpp_intersects(SEXP dataLeft, SEXP dataRight);
RcppExport SEXP _geovctrs_cpp_intersects(SEXP dataLeftSEXP, SEXP dataRightSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type dataLeft(dataLeftSEXP);
    Rcpp::traits::input_parameter< SEXP >::type dataRight(dataRightSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_intersects(dataLeft, dataRight));
    return rcpp_result_gen;
END_RCPP
}
// cpp_crosses
LogicalVector cpp_crosses(SEXP dataLeft, SEXP dataRight);
RcppExport SEXP _geovctrs_cpp_crosses(SEXP dataLeftSEXP, SEXP dataRightSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type dataLeft(dataLeftSEXP);
    Rcpp::traits::input_parameter< SEXP >::type dataRight(dataRightSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_crosses(dataLeft, dataRight));
    return rcpp_result_gen;
END_RCPP
}
// cpp_is_within
LogicalVector cpp_is_within(SEXP dataLeft, SEXP dataRight);
RcppExport SEXP _geovctrs_cpp_is_within(SEXP dataLeftSEXP, SEXP dataRightSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type dataLeft(dataLeftSEXP);
    Rcpp::traits::input_parameter< SEXP >::type dataRight(dataRightSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_is_within(dataLeft, dataRight));
    return rcpp_result_gen;
END_RCPP
}
// cpp_contains
LogicalVector cpp_contains(SEXP dataLeft, SEXP dataRight);
RcppExport SEXP _geovctrs_cpp_contains(SEXP dataLeftSEXP, SEXP dataRightSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type dataLeft(dataLeftSEXP);
    Rcpp::traits::input_parameter< SEXP >::type dataRight(dataRightSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_contains(dataLeft, dataRight));
    return rcpp_result_gen;
END_RCPP
}
// cpp_overlaps
LogicalVector cpp_overlaps(SEXP dataLeft, SEXP dataRight);
RcppExport SEXP _geovctrs_cpp_overlaps(SEXP dataLeftSEXP, SEXP dataRightSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type dataLeft(dataLeftSEXP);
    Rcpp::traits::input_parameter< SEXP >::type dataRight(dataRightSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_overlaps(dataLeft, dataRight));
    return rcpp_result_gen;
END_RCPP
}
// cpp_equals
LogicalVector cpp_equals(SEXP dataLeft, SEXP dataRight);
RcppExport SEXP _geovctrs_cpp_equals(SEXP dataLeftSEXP, SEXP dataRightSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type dataLeft(dataLeftSEXP);
    Rcpp::traits::input_parameter< SEXP >::type dataRight(dataRightSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_equals(dataLeft, dataRight));
    return rcpp_result_gen;
END_RCPP
}
// cpp_covers
LogicalVector cpp_covers(SEXP dataLeft, SEXP dataRight);
RcppExport SEXP _geovctrs_cpp_covers(SEXP dataLeftSEXP, SEXP dataRightSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type dataLeft(dataLeftSEXP);
    Rcpp::traits::input_parameter< SEXP >::type dataRight(dataRightSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_covers(dataLeft, dataRight));
    return rcpp_result_gen;
END_RCPP
}
// cpp_is_covered_by
LogicalVector cpp_is_covered_by(SEXP dataLeft, SEXP dataRight);
RcppExport SEXP _geovctrs_cpp_is_covered_by(SEXP dataLeftSEXP, SEXP dataRightSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type dataLeft(dataLeftSEXP);
    Rcpp::traits::input_parameter< SEXP >::type dataRight(dataRightSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_is_covered_by(dataLeft, dataRight));
    return rcpp_result_gen;
END_RCPP
}
// cpp_offset_curve
SEXP cpp_offset_curve(SEXP x, NumericVector width, int quadSegs, int endCapStyle, int joinStyle, double mitreLimit, SEXP to);
RcppExport SEXP _geovctrs_cpp_offset_curve(SEXP xSEXP, SEXP widthSEXP, SEXP quadSegsSEXP, SEXP endCapStyleSEXP, SEXP joinStyleSEXP, SEXP mitreLimitSEXP, SEXP toSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type width(widthSEXP);
    Rcpp::traits::input_parameter< int >::type quadSegs(quadSegsSEXP);
    Rcpp::traits::input_parameter< int >::type endCapStyle(endCapStyleSEXP);
    Rcpp::traits::input_parameter< int >::type joinStyle(joinStyleSEXP);
    Rcpp::traits::input_parameter< double >::type mitreLimit(mitreLimitSEXP);
    Rcpp::traits::input_parameter< SEXP >::type to(toSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_offset_curve(x, width, quadSegs, endCapStyle, joinStyle, mitreLimit, to));
    return rcpp_result_gen;
END_RCPP
}
// cpp_buffer
SEXP cpp_buffer(SEXP x, NumericVector width, int quadSegs, int endCapStyle, int joinStyle, double mitreLimit, int singleSided, SEXP to);
RcppExport SEXP _geovctrs_cpp_buffer(SEXP xSEXP, SEXP widthSEXP, SEXP quadSegsSEXP, SEXP endCapStyleSEXP, SEXP joinStyleSEXP, SEXP mitreLimitSEXP, SEXP singleSidedSEXP, SEXP toSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type width(widthSEXP);
    Rcpp::traits::input_parameter< int >::type quadSegs(quadSegsSEXP);
    Rcpp::traits::input_parameter< int >::type endCapStyle(endCapStyleSEXP);
    Rcpp::traits::input_parameter< int >::type joinStyle(joinStyleSEXP);
    Rcpp::traits::input_parameter< double >::type mitreLimit(mitreLimitSEXP);
    Rcpp::traits::input_parameter< int >::type singleSided(singleSidedSEXP);
    Rcpp::traits::input_parameter< SEXP >::type to(toSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_buffer(x, width, quadSegs, endCapStyle, joinStyle, mitreLimit, singleSided, to));
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
// cpp_is_simple
LogicalVector cpp_is_simple(SEXP data);
RcppExport SEXP _geovctrs_cpp_is_simple(SEXP dataSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type data(dataSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_is_simple(data));
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
// cpp_is_closed
LogicalVector cpp_is_closed(SEXP data);
RcppExport SEXP _geovctrs_cpp_is_closed(SEXP dataSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type data(dataSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_is_closed(data));
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
// cpp_n_points
IntegerVector cpp_n_points(SEXP x);
RcppExport SEXP _geovctrs_cpp_n_points(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_n_points(x));
    return rcpp_result_gen;
END_RCPP
}
// cpp_n_interior_rings
IntegerVector cpp_n_interior_rings(SEXP x);
RcppExport SEXP _geovctrs_cpp_n_interior_rings(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_n_interior_rings(x));
    return rcpp_result_gen;
END_RCPP
}
// cpp_n_dimensions
IntegerVector cpp_n_dimensions(SEXP x);
RcppExport SEXP _geovctrs_cpp_n_dimensions(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_n_dimensions(x));
    return rcpp_result_gen;
END_RCPP
}
// cpp_n_coordinate_dimensions
IntegerVector cpp_n_coordinate_dimensions(SEXP x);
RcppExport SEXP _geovctrs_cpp_n_coordinate_dimensions(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_n_coordinate_dimensions(x));
    return rcpp_result_gen;
END_RCPP
}
// cpp_project
NumericVector cpp_project(SEXP dataLeft, SEXP dataRight);
RcppExport SEXP _geovctrs_cpp_project(SEXP dataLeftSEXP, SEXP dataRightSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type dataLeft(dataLeftSEXP);
    Rcpp::traits::input_parameter< SEXP >::type dataRight(dataRightSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_project(dataLeft, dataRight));
    return rcpp_result_gen;
END_RCPP
}
// cpp_project_normalized
NumericVector cpp_project_normalized(SEXP dataLeft, SEXP dataRight);
RcppExport SEXP _geovctrs_cpp_project_normalized(SEXP dataLeftSEXP, SEXP dataRightSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type dataLeft(dataLeftSEXP);
    Rcpp::traits::input_parameter< SEXP >::type dataRight(dataRightSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_project_normalized(dataLeft, dataRight));
    return rcpp_result_gen;
END_RCPP
}
// cpp_interpolate
SEXP cpp_interpolate(SEXP data, SEXP ptype, NumericVector distance);
RcppExport SEXP _geovctrs_cpp_interpolate(SEXP dataSEXP, SEXP ptypeSEXP, SEXP distanceSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type data(dataSEXP);
    Rcpp::traits::input_parameter< SEXP >::type ptype(ptypeSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type distance(distanceSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_interpolate(data, ptype, distance));
    return rcpp_result_gen;
END_RCPP
}
// cpp_interpolate_normalized
SEXP cpp_interpolate_normalized(SEXP data, SEXP ptype, NumericVector distance);
RcppExport SEXP _geovctrs_cpp_interpolate_normalized(SEXP dataSEXP, SEXP ptypeSEXP, SEXP distanceSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type data(dataSEXP);
    Rcpp::traits::input_parameter< SEXP >::type ptype(ptypeSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type distance(distanceSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_interpolate_normalized(data, ptype, distance));
    return rcpp_result_gen;
END_RCPP
}
// cpp_area
NumericVector cpp_area(SEXP x);
RcppExport SEXP _geovctrs_cpp_area(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_area(x));
    return rcpp_result_gen;
END_RCPP
}
// cpp_length
NumericVector cpp_length(SEXP x);
RcppExport SEXP _geovctrs_cpp_length(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_length(x));
    return rcpp_result_gen;
END_RCPP
}
// cpp_distance
NumericVector cpp_distance(SEXP x, SEXP y);
RcppExport SEXP _geovctrs_cpp_distance(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< SEXP >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_distance(x, y));
    return rcpp_result_gen;
END_RCPP
}
// cpp_point_on_surface
SEXP cpp_point_on_surface(SEXP dataLeft, SEXP ptype);
RcppExport SEXP _geovctrs_cpp_point_on_surface(SEXP dataLeftSEXP, SEXP ptypeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type dataLeft(dataLeftSEXP);
    Rcpp::traits::input_parameter< SEXP >::type ptype(ptypeSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_point_on_surface(dataLeft, ptype));
    return rcpp_result_gen;
END_RCPP
}
// cpp_centroid
SEXP cpp_centroid(SEXP dataLeft, SEXP ptype);
RcppExport SEXP _geovctrs_cpp_centroid(SEXP dataLeftSEXP, SEXP ptypeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type dataLeft(dataLeftSEXP);
    Rcpp::traits::input_parameter< SEXP >::type ptype(ptypeSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_centroid(dataLeft, ptype));
    return rcpp_result_gen;
END_RCPP
}
// cpp_node
SEXP cpp_node(SEXP dataLeft, SEXP ptype);
RcppExport SEXP _geovctrs_cpp_node(SEXP dataLeftSEXP, SEXP ptypeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type dataLeft(dataLeftSEXP);
    Rcpp::traits::input_parameter< SEXP >::type ptype(ptypeSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_node(dataLeft, ptype));
    return rcpp_result_gen;
END_RCPP
}
// cpp_boundary
SEXP cpp_boundary(SEXP dataLeft, SEXP ptype);
RcppExport SEXP _geovctrs_cpp_boundary(SEXP dataLeftSEXP, SEXP ptypeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type dataLeft(dataLeftSEXP);
    Rcpp::traits::input_parameter< SEXP >::type ptype(ptypeSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_boundary(dataLeft, ptype));
    return rcpp_result_gen;
END_RCPP
}
// cpp_envelope
SEXP cpp_envelope(SEXP dataLeft, SEXP ptype);
RcppExport SEXP _geovctrs_cpp_envelope(SEXP dataLeftSEXP, SEXP ptypeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type dataLeft(dataLeftSEXP);
    Rcpp::traits::input_parameter< SEXP >::type ptype(ptypeSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_envelope(dataLeft, ptype));
    return rcpp_result_gen;
END_RCPP
}
// cpp_convex_hull
SEXP cpp_convex_hull(SEXP dataLeft, SEXP ptype);
RcppExport SEXP _geovctrs_cpp_convex_hull(SEXP dataLeftSEXP, SEXP ptypeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type dataLeft(dataLeftSEXP);
    Rcpp::traits::input_parameter< SEXP >::type ptype(ptypeSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_convex_hull(dataLeft, ptype));
    return rcpp_result_gen;
END_RCPP
}
// cpp_minimum_rotated_rectangle
SEXP cpp_minimum_rotated_rectangle(SEXP dataLeft, SEXP ptype);
RcppExport SEXP _geovctrs_cpp_minimum_rotated_rectangle(SEXP dataLeftSEXP, SEXP ptypeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type dataLeft(dataLeftSEXP);
    Rcpp::traits::input_parameter< SEXP >::type ptype(ptypeSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_minimum_rotated_rectangle(dataLeft, ptype));
    return rcpp_result_gen;
END_RCPP
}
// cpp_minimum_bounding_circle
SEXP cpp_minimum_bounding_circle(SEXP dataLeft, SEXP ptype);
RcppExport SEXP _geovctrs_cpp_minimum_bounding_circle(SEXP dataLeftSEXP, SEXP ptypeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type dataLeft(dataLeftSEXP);
    Rcpp::traits::input_parameter< SEXP >::type ptype(ptypeSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_minimum_bounding_circle(dataLeft, ptype));
    return rcpp_result_gen;
END_RCPP
}
// cpp_minimum_bounding_circle_center
SEXP cpp_minimum_bounding_circle_center(SEXP dataLeft, SEXP ptype);
RcppExport SEXP _geovctrs_cpp_minimum_bounding_circle_center(SEXP dataLeftSEXP, SEXP ptypeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type dataLeft(dataLeftSEXP);
    Rcpp::traits::input_parameter< SEXP >::type ptype(ptypeSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_minimum_bounding_circle_center(dataLeft, ptype));
    return rcpp_result_gen;
END_RCPP
}
// cpp_minimum_bounding_circle_radius
NumericVector cpp_minimum_bounding_circle_radius(SEXP dataLeft);
RcppExport SEXP _geovctrs_cpp_minimum_bounding_circle_radius(SEXP dataLeftSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type dataLeft(dataLeftSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_minimum_bounding_circle_radius(dataLeft));
    return rcpp_result_gen;
END_RCPP
}
// cpp_minimum_width
SEXP cpp_minimum_width(SEXP dataLeft, SEXP ptype);
RcppExport SEXP _geovctrs_cpp_minimum_width(SEXP dataLeftSEXP, SEXP ptypeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type dataLeft(dataLeftSEXP);
    Rcpp::traits::input_parameter< SEXP >::type ptype(ptypeSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_minimum_width(dataLeft, ptype));
    return rcpp_result_gen;
END_RCPP
}
// cpp_minimum_clearance_line
SEXP cpp_minimum_clearance_line(SEXP dataLeft, SEXP ptype);
RcppExport SEXP _geovctrs_cpp_minimum_clearance_line(SEXP dataLeftSEXP, SEXP ptypeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type dataLeft(dataLeftSEXP);
    Rcpp::traits::input_parameter< SEXP >::type ptype(ptypeSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_minimum_clearance_line(dataLeft, ptype));
    return rcpp_result_gen;
END_RCPP
}
// cpp_minimum_clearance
NumericVector cpp_minimum_clearance(SEXP dataLeft);
RcppExport SEXP _geovctrs_cpp_minimum_clearance(SEXP dataLeftSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type dataLeft(dataLeftSEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_minimum_clearance(dataLeft));
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
// cpp_segment_intersection
List cpp_segment_intersection(NumericVector ax0, NumericVector ay0, NumericVector ax1, NumericVector ay1, NumericVector bx0, NumericVector by0, NumericVector bx1, NumericVector by1);
RcppExport SEXP _geovctrs_cpp_segment_intersection(SEXP ax0SEXP, SEXP ay0SEXP, SEXP ax1SEXP, SEXP ay1SEXP, SEXP bx0SEXP, SEXP by0SEXP, SEXP bx1SEXP, SEXP by1SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type ax0(ax0SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type ay0(ay0SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type ax1(ax1SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type ay1(ay1SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type bx0(bx0SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type by0(by0SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type bx1(bx1SEXP);
    Rcpp::traits::input_parameter< NumericVector >::type by1(by1SEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_segment_intersection(ax0, ay0, ax1, ay1, bx0, by0, bx1, by1));
    return rcpp_result_gen;
END_RCPP
}
// cpp_orientation_index
IntegerVector cpp_orientation_index(NumericVector ax, NumericVector ay, NumericVector bx, NumericVector by, NumericVector px, NumericVector py);
RcppExport SEXP _geovctrs_cpp_orientation_index(SEXP axSEXP, SEXP aySEXP, SEXP bxSEXP, SEXP bySEXP, SEXP pxSEXP, SEXP pySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type ax(axSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type ay(aySEXP);
    Rcpp::traits::input_parameter< NumericVector >::type bx(bxSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type by(bySEXP);
    Rcpp::traits::input_parameter< NumericVector >::type px(pxSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type py(pySEXP);
    rcpp_result_gen = Rcpp::wrap(cpp_orientation_index(ax, ay, bx, by, px, py));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_geovctrs_cpp_convert", (DL_FUNC) &_geovctrs_cpp_convert, 2},
    {"_geovctrs_cpp_version_impl", (DL_FUNC) &_geovctrs_cpp_version_impl, 0},
    {"_geovctrs_cpp_intersection", (DL_FUNC) &_geovctrs_cpp_intersection, 3},
    {"_geovctrs_cpp_difference", (DL_FUNC) &_geovctrs_cpp_difference, 3},
    {"_geovctrs_cpp_sym_difference", (DL_FUNC) &_geovctrs_cpp_sym_difference, 3},
    {"_geovctrs_cpp_union", (DL_FUNC) &_geovctrs_cpp_union, 3},
    {"_geovctrs_cpp_unary_union", (DL_FUNC) &_geovctrs_cpp_unary_union, 2},
    {"_geovctrs_cpp_coverage_union", (DL_FUNC) &_geovctrs_cpp_coverage_union, 2},
    {"_geovctrs_cpp_clip_by_rect", (DL_FUNC) &_geovctrs_cpp_clip_by_rect, 6},
    {"_geovctrs_cpp_is_disjoint", (DL_FUNC) &_geovctrs_cpp_is_disjoint, 2},
    {"_geovctrs_cpp_touches", (DL_FUNC) &_geovctrs_cpp_touches, 2},
    {"_geovctrs_cpp_intersects", (DL_FUNC) &_geovctrs_cpp_intersects, 2},
    {"_geovctrs_cpp_crosses", (DL_FUNC) &_geovctrs_cpp_crosses, 2},
    {"_geovctrs_cpp_is_within", (DL_FUNC) &_geovctrs_cpp_is_within, 2},
    {"_geovctrs_cpp_contains", (DL_FUNC) &_geovctrs_cpp_contains, 2},
    {"_geovctrs_cpp_overlaps", (DL_FUNC) &_geovctrs_cpp_overlaps, 2},
    {"_geovctrs_cpp_equals", (DL_FUNC) &_geovctrs_cpp_equals, 2},
    {"_geovctrs_cpp_covers", (DL_FUNC) &_geovctrs_cpp_covers, 2},
    {"_geovctrs_cpp_is_covered_by", (DL_FUNC) &_geovctrs_cpp_is_covered_by, 2},
    {"_geovctrs_cpp_offset_curve", (DL_FUNC) &_geovctrs_cpp_offset_curve, 7},
    {"_geovctrs_cpp_buffer", (DL_FUNC) &_geovctrs_cpp_buffer, 8},
    {"_geovctrs_cpp_is_empty", (DL_FUNC) &_geovctrs_cpp_is_empty, 1},
    {"_geovctrs_cpp_is_simple", (DL_FUNC) &_geovctrs_cpp_is_simple, 1},
    {"_geovctrs_cpp_has_z", (DL_FUNC) &_geovctrs_cpp_has_z, 1},
    {"_geovctrs_cpp_is_closed", (DL_FUNC) &_geovctrs_cpp_is_closed, 1},
    {"_geovctrs_cpp_geom_type_id", (DL_FUNC) &_geovctrs_cpp_geom_type_id, 1},
    {"_geovctrs_cpp_get_srid", (DL_FUNC) &_geovctrs_cpp_get_srid, 1},
    {"_geovctrs_cpp_n_geometries", (DL_FUNC) &_geovctrs_cpp_n_geometries, 1},
    {"_geovctrs_cpp_n_coordinates", (DL_FUNC) &_geovctrs_cpp_n_coordinates, 1},
    {"_geovctrs_cpp_n_points", (DL_FUNC) &_geovctrs_cpp_n_points, 1},
    {"_geovctrs_cpp_n_interior_rings", (DL_FUNC) &_geovctrs_cpp_n_interior_rings, 1},
    {"_geovctrs_cpp_n_dimensions", (DL_FUNC) &_geovctrs_cpp_n_dimensions, 1},
    {"_geovctrs_cpp_n_coordinate_dimensions", (DL_FUNC) &_geovctrs_cpp_n_coordinate_dimensions, 1},
    {"_geovctrs_cpp_project", (DL_FUNC) &_geovctrs_cpp_project, 2},
    {"_geovctrs_cpp_project_normalized", (DL_FUNC) &_geovctrs_cpp_project_normalized, 2},
    {"_geovctrs_cpp_interpolate", (DL_FUNC) &_geovctrs_cpp_interpolate, 3},
    {"_geovctrs_cpp_interpolate_normalized", (DL_FUNC) &_geovctrs_cpp_interpolate_normalized, 3},
    {"_geovctrs_cpp_area", (DL_FUNC) &_geovctrs_cpp_area, 1},
    {"_geovctrs_cpp_length", (DL_FUNC) &_geovctrs_cpp_length, 1},
    {"_geovctrs_cpp_distance", (DL_FUNC) &_geovctrs_cpp_distance, 2},
    {"_geovctrs_cpp_point_on_surface", (DL_FUNC) &_geovctrs_cpp_point_on_surface, 2},
    {"_geovctrs_cpp_centroid", (DL_FUNC) &_geovctrs_cpp_centroid, 2},
    {"_geovctrs_cpp_node", (DL_FUNC) &_geovctrs_cpp_node, 2},
    {"_geovctrs_cpp_boundary", (DL_FUNC) &_geovctrs_cpp_boundary, 2},
    {"_geovctrs_cpp_envelope", (DL_FUNC) &_geovctrs_cpp_envelope, 2},
    {"_geovctrs_cpp_convex_hull", (DL_FUNC) &_geovctrs_cpp_convex_hull, 2},
    {"_geovctrs_cpp_minimum_rotated_rectangle", (DL_FUNC) &_geovctrs_cpp_minimum_rotated_rectangle, 2},
    {"_geovctrs_cpp_minimum_bounding_circle", (DL_FUNC) &_geovctrs_cpp_minimum_bounding_circle, 2},
    {"_geovctrs_cpp_minimum_bounding_circle_center", (DL_FUNC) &_geovctrs_cpp_minimum_bounding_circle_center, 2},
    {"_geovctrs_cpp_minimum_bounding_circle_radius", (DL_FUNC) &_geovctrs_cpp_minimum_bounding_circle_radius, 1},
    {"_geovctrs_cpp_minimum_width", (DL_FUNC) &_geovctrs_cpp_minimum_width, 2},
    {"_geovctrs_cpp_minimum_clearance_line", (DL_FUNC) &_geovctrs_cpp_minimum_clearance_line, 2},
    {"_geovctrs_cpp_minimum_clearance", (DL_FUNC) &_geovctrs_cpp_minimum_clearance, 1},
    {"_geovctrs_cpp_validate_provider", (DL_FUNC) &_geovctrs_cpp_validate_provider, 1},
    {"_geovctrs_cpp_segment_intersection", (DL_FUNC) &_geovctrs_cpp_segment_intersection, 8},
    {"_geovctrs_cpp_orientation_index", (DL_FUNC) &_geovctrs_cpp_orientation_index, 6},
    {NULL, NULL, 0}
};

RcppExport void R_init_geovctrs(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
