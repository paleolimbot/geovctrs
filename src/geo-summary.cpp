
#include "geos-operator.h"
#include "geos-coords.h"
using namespace Rcpp;

class GeomTypeIdOperator: public UnaryVectorOperator<IntegerVector, int> {
  int operateNext(GEOSGeometry* geometry) {
    return GEOSGeomTypeId_r(this->context, geometry);
  }
};

// [[Rcpp::export]]
IntegerVector cpp_geom_type_id(SEXP x) {
  GeomTypeIdOperator op;
  op.initProvider(x);
  return op.operate();
}

class GetNumGeometriesOperator: public UnaryVectorOperator<IntegerVector, int> {
  int operateNext(GEOSGeometry* geometry) {
    return GEOSGetNumGeometries_r(this->context, geometry);
  }
};

// [[Rcpp::export]]
IntegerVector cpp_n_geometries(SEXP x) {
  GetNumGeometriesOperator op;
  op.initProvider(x);
  return op.operate();
}

class GetNumCoordinatesOperator: public UnaryVectorOperator<IntegerVector, int> {
  int operateNext(GEOSGeometry* geometry) {
    return GEOSGetNumCoordinates_r(this->context, geometry);
  }
};

// [[Rcpp::export]]
IntegerVector cpp_n_coordinates(SEXP x) {
  GetNumCoordinatesOperator op;
  op.initProvider(x);
  return op.operate();
}

class CoordinateDimensionsOperator: public UnaryVectorOperator<IntegerVector, int> {
  int operateNext(GEOSGeometry* geometry) {
    // the behaviour of this changed between GEOS 3.5 and 3.7, but
    // currently empty geometries have 3 dimensions
    if (GEOSisEmpty_r(this->context, geometry)) {
      return 3;
    } else {
      return GEOSGeom_getCoordinateDimension_r(this->context, geometry);
    }
  }
};

// [[Rcpp::export]]
IntegerVector cpp_coordinate_dimensions(SEXP x) {
  CoordinateDimensionsOperator op;
  op.initProvider(x);
  return op.operate();
}

class FirstCoordinateOperator: public UnaryOperator {
public:
  NumericVector x;
  NumericVector y;

  void init() {
    NumericVector x(this->size());
    NumericVector y(this->size());
    this->x = x;
    this->y = y;
  }

  void operateNext(GEOSGeometry* geometry) {
    if (geometry == NULL) {
      this->x[this->counter] = NA_REAL;
      this->y[this->counter] = NA_REAL;
    } else if (GEOSisEmpty_r(this->context, geometry)) {
      this->x[this->counter] = NA_REAL;
      this->y[this->counter] = NA_REAL;
    } else if (GEOSGeomTypeId_r(this->context, geometry) == GEOSGeomTypes::GEOS_GEOMETRYCOLLECTION) {
      this->x[this->counter] = NA_REAL;
      this->y[this->counter] = NA_REAL;
    } else {
      List coords = geometry_to_geo_coord(this->context, geometry);
      List xy = coords["xy"];
      NumericVector x = as<NumericVector>(xy["x"]);
      NumericVector y = as<NumericVector>(xy["y"]);
      this->x[this->counter] = x[0];
      this->y[this->counter] = y[0];
    }
  }

  SEXP assemble() {
    List xy = List::create(
      _["x"] = this->x,
      _["y"] = this->y
    );
    xy.attr("class") = CharacterVector::create("geovctrs_xy", "geovctr", "vctrs_rcrd", "vctrs_vctr");
    return xy;
  }
};

// [[Rcpp::export]]
SEXP cpp_first_coordinate(SEXP x) {
  FirstCoordinateOperator op;
  op.initProvider(x);
  return op.operate();
}
