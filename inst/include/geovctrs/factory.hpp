
#ifndef GEOVCTRS_FACTORY_HPP
#define GEOVCTRS_FACTORY_HPP

#include <Rcpp.h>
using namespace Rcpp;

class GeovctrsFactory {
public:

  static CharacterVector newWKT(CharacterVector data) {
    data.attr("class") = CharacterVector::create("wk_wkt", "wk_vctr", "geovctr");
    return data;
  }

  static List newWKB(List data) {
    data.attr("class") = CharacterVector::create("wk_wkb", "wk_vctr", "geovctr");
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

  static List newXYZ(NumericVector x, NumericVector y, NumericVector z) {
    List xyz = List::create(_["x"] = x, _["y"] = y, _["z"] = z);
    xyz.attr("class") = CharacterVector::create(
      "geovctrs_xyz", "geovctrs_xy",
      "geovctr", "vctrs_rcrd", "vctrs_vctr"
    );
    return xyz;
  }

  static List newSegment(NumericVector x0, NumericVector y0,
                         NumericVector x1, NumericVector y1,
                         IntegerVector srid) {
    List segment = List::create(
      _["x0"] = x0,
      _["y0"] = y0,
      _["x1"] = x1,
      _["y1"] = y1,
      _["srid"] = srid
    );
    segment.attr("class") = CharacterVector::create(
      "geovctrs_segment", "geovctr",
      "vctrs_rcrd", "vctrs_vctr"
    );
    return segment;
  }

  static List newRect(NumericVector xmin, NumericVector ymin,
                      NumericVector xmax, NumericVector ymax,
                      IntegerVector srid) {
    List result = List::create(
      _["xmin"] = xmin,
      _["ymin"] = ymin,
      _["xmax"] = xmax,
      _["ymax"] = ymax,
      _["srid"] = srid
    );
    result.attr("class") = CharacterVector::create(
      "geovctrs_rect", "geovctr",
      "vctrs_rcrd", "vctrs_vctr"
    );
    return result;
  }

  static List newLim(NumericVector lower, NumericVector upper) {
    List result = List::create(
      _["lower"] = lower,
      _["upper"] = upper
    );
    result.attr("class") = CharacterVector::create(
      "geovctrs_lim", "vctrs_rcrd", "vctrs_vctr"
    );
    return result;
  }

  static List newPoint(List xy) {
    List point = List::create(_["xy"] = xy);
    point.attr("class") = CharacterVector::create("geovctrs_point");
    return point;
  }

  static List newLinestring(List xy) {
    List point = List::create(_["xy"] = xy);
    point.attr("class") = CharacterVector::create("geovctrs_linestring");
    return point;
  }

  static List newPolygon(List xy, IntegerVector ring) {
    List point = List::create(_["xy"] = xy, _["ring"] = ring);
    point.attr("class") = CharacterVector::create("geovctrs_polygon");
    return point;
  }

  static List newMultipoint(List xy) {
    List point = List::create(_["xy"] = xy);
    point.attr("class") = CharacterVector::create("geovctrs_multipoint");
    return point;
  }

  static List newMultilinestring(List xy, IntegerVector part) {
    List point = List::create(_["xy"] = xy, _["part"] = part);
    point.attr("class") = CharacterVector::create("geovctrs_multilinestring");
    return point;
  }

  static List newMultipolygon(List xy, IntegerVector part,
                              IntegerVector ring) {
    List point = List::create(_["xy"] = xy,  _["part"] = part, _["ring"] = ring);
    point.attr("class") = CharacterVector::create("geovctrs_multipolygon");
    return point;
  }

};

#endif
