
#ifndef GEOVCTRS_FACTORY_HPP
#define GEOVCTRS_FACTORY_HPP

#include <geos_c.h>
#include <Rcpp.h>
using namespace Rcpp;

class GeovctrsFactory {
public:

  static CharacterVector newWKT(CharacterVector data) {
    data.attr("class") = CharacterVector::create("geovctrs_wkt", "geovctr", "vctrs_vctr");

    // always give the default values (prevents unnecessary conversion)
    data.attr("trim") = LogicalVector::create(true);
    data.attr("precision") =  IntegerVector::create(16);
    data.attr("dimensions") = IntegerVector::create(3);

    return data;
  }

  static List newWKB(List data) {
    // always give the default values (prevents unnecessary conversion)
    data.attr("class") = CharacterVector::create("geovctrs_wkb", "geovctr", "vctrs_vctr");
    data.attr("include_srid") = LogicalVector::create(LogicalVector::get_na());
    data.attr("dimensions") = IntegerVector::create(3);
    data.attr("endian") = IntegerVector::create(LogicalVector::get_na());

    return data;
  }

  static List newCollection(List data, IntegerVector srid) {
    List collection = List::create(_["feature"] = data, _["srid"] = srid);
    collection.attr("class") = CharacterVector::create(
      "geovctrs_collection", "geovctr", "vctrs_rcrd", "vctrs_vctr"
    );
    return collection;
  }

  static List newXY(NumericVector x, NumericVector y) {
    List xy = List::create(_["x"] = x, _["y"] = y);
    xy.attr("class") = CharacterVector::create("geovctrs_xy", "geovctr", "vctrs_rcrd", "vctrs_vctr");
    return xy;
  }

  static List newSegment(NumericVector x0, NumericVector y0, NumericVector x1, NumericVector y1,
                         IntegerVector srid) {
    List start = newXY(x0, y0);
    List end = newXY(x1, y1);
    List segment = List::create(_["start"] = start, _["end"] = end, _["srid"] = srid);
    segment.attr("class") = CharacterVector::create("geovctrs_segment", "geovctr", "vctrs_rcrd", "vctrs_vctr");
    return segment;
  }

  static List newRect(NumericVector xmin, NumericVector ymin, NumericVector xmax, NumericVector ymax,
                      IntegerVector srid) {
    List result = List::create(
      _["xmin"] = xmin,
      _["ymin"] = ymin,
      _["xmax"] = xmax,
      _["ymax"] = ymax,
      _["srid"] = srid
    );
    result.attr("class") = CharacterVector::create("geovctrs_rect", "geovctr", "vctrs_rcrd", "vctrs_vctr");
    return result;
  }

  static List newPoint(NumericVector x, NumericVector y) {
    List point = List::create(_["xy"] = newXY(x, y));
    point.attr("class") = CharacterVector::create("geovctrs_point");
    return point;
  }

  static List newLinestring(NumericVector x, NumericVector y) {
    List point = List::create(_["xy"] = newXY(x, y));
    point.attr("class") = CharacterVector::create("geovctrs_linestring");
    return point;
  }

  static List newPolygon(NumericVector x, NumericVector y, IntegerVector ring) {
    List point = List::create(_["xy"] = newXY(x, y), _["ring"] = ring);
    point.attr("class") = CharacterVector::create("geovctrs_polygon");
    return point;
  }

  static List newMultipoint(NumericVector x, NumericVector y) {
    List point = List::create(_["xy"] = newXY(x, y));
    point.attr("class") = CharacterVector::create("geovctrs_multipoint");
    return point;
  }

  static List newMultilinestring(NumericVector x, NumericVector y, IntegerVector part) {
    List point = List::create(_["xy"] = newXY(x, y), _["part"] = part);
    point.attr("class") = CharacterVector::create("geovctrs_multilinestring");
    return point;
  }

  static List newMultipolygon(NumericVector x, NumericVector y, IntegerVector part, IntegerVector ring) {
    List point = List::create(_["xy"] = newXY(x, y),  _["part"] = part, _["ring"] = ring);
    point.attr("class") = CharacterVector::create("geovctrs_multipolygon");
    return point;
  }

};

#endif
