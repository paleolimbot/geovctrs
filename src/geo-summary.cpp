
#include "geos-operator.h"
#include "geos-coords.h"
using namespace Rcpp;

class GeomTypeIdOperator: public UnaryVectorOperator<IntegerVector, int> {
  int operateNext(GEOSContextHandle_t context, GEOSGeometry* geometry, size_t i) {
    return GEOSGeomTypeId_r(context, geometry);
  }
};

// [[Rcpp::export]]
IntegerVector cpp_geom_type_id(SEXP x) {
  GeomTypeIdOperator op;
  op.initProvider(x);
  return op.operate();
}

class GetNumGeometriesOperator: public UnaryVectorOperator<IntegerVector, int> {
  int operateNext(GEOSContextHandle_t context, GEOSGeometry* geometry, size_t i) {
    return GEOSGetNumGeometries_r(context, geometry);
  }
};

// [[Rcpp::export]]
IntegerVector cpp_n_geometries(SEXP x) {
  GetNumGeometriesOperator op;
  op.initProvider(x);
  return op.operate();
}

class GetNumCoordinatesOperator: public UnaryVectorOperator<IntegerVector, int> {
  int operateNext(GEOSContextHandle_t context, GEOSGeometry* geometry, size_t i) {
    return GEOSGetNumCoordinates_r(context, geometry);
  }
};

// [[Rcpp::export]]
IntegerVector cpp_n_coordinates(SEXP x) {
  GetNumCoordinatesOperator op;
  op.initProvider(x);
  return op.operate();
}

class CoordinateDimensionsOperator: public UnaryVectorOperator<IntegerVector, int> {
  int operateNext(GEOSContextHandle_t context, GEOSGeometry* geometry, size_t i) {
    // the behaviour of this changed between GEOS 3.5 and 3.7, but
    // currently empty geometries have 3 dimensions
    if (GEOSisEmpty_r(context, geometry)) {
      return 3;
    } else {
      return GEOSGeom_getCoordinateDimension_r(context, geometry);
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

  void init(GEOSContextHandle_t context, size_t size) {
    NumericVector x(size);
    NumericVector y(size);
    this->x = x;
    this->y = y;
  }

  void operateNext(GEOSContextHandle_t context, GEOSGeometry* geometry, size_t i) {
    if (geometry == NULL) {
      this->x[i] = NA_REAL;
      this->y[i] = NA_REAL;
    } else if (GEOSisEmpty_r(context, geometry)) {
      this->x[i] = NA_REAL;
      this->y[i] = NA_REAL;
    } else if (GEOSGeomTypeId_r(context, geometry) == GEOSGeomTypes::GEOS_GEOMETRYCOLLECTION) {
      this->x[i] = NA_REAL;
      this->y[i] = NA_REAL;
    } else {
      List coords = geometry_to_geo_coord(context, geometry);
      List xy = coords["xy"];
      NumericVector x = as<NumericVector>(xy["x"]);
      NumericVector y = as<NumericVector>(xy["y"]);
      this->x[i] = x[0];
      this->y[i] = y[0];
    }
  }

  SEXP assemble(GEOSContextHandle_t context) {
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
