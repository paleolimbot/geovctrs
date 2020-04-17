
#include "geovctrs/geos/operator.hpp"
#include "geovctrs/factory.hpp"
using namespace Rcpp;

// [[Rcpp::interfaces(r, cpp)]]

class SummaryOperator: public GeovctrsRecursiveOperator {
public:
  LogicalVector isEmpty;
  IntegerVector geometryTypeId;
  IntegerVector nGeometries;
  IntegerVector nCoordinates;
  IntegerVector srid;
  IntegerVector coordinateDimensions;
  NumericVector firstX;
  NumericVector firstY;
  NumericVector firstZ;
  CharacterVector problems;
  LogicalVector isMissing;

  void init(GEOSContextHandle_t context, size_t size) {
    this->isEmpty = LogicalVector(size);
    this->geometryTypeId = IntegerVector(size);
    this->nGeometries = IntegerVector(size);
    this->nCoordinates = IntegerVector(size);
    this->srid = IntegerVector(size);
    this->coordinateDimensions = IntegerVector(size);
    this->firstX = NumericVector(size);
    this->firstY = NumericVector(size);
    this->firstZ = NumericVector(size);
    this->problems = CharacterVector(size);
    this->isMissing = LogicalVector(size);
  }

  void nextFeature(GEOSContextHandle_t context, GEOSGeometry* geometry, size_t i) {
    this->problems[i] = NA_STRING;

    if (geometry == NULL) {
      this->setRowNULL(i);
    } else {
      this->isMissing[i] = false;
      this->isEmpty[i] = GEOSisEmpty_r(context, geometry);
      this->geometryTypeId[i] = GEOSGeomTypeId_r(context, geometry);
      this->nGeometries[i] = GEOSGetNumGeometries_r(context, geometry);
      this->nCoordinates[i] = GEOSGetNumCoordinates_r(context, geometry);
      this->srid[i] = GEOSGetSRID_r(context, geometry);
      this->coordinateDimensions[i] = GEOSGeom_getCoordinateDimension_r(context, geometry);
      this->firstX[i] = NA_REAL;
      this->firstY[i] = NA_REAL;
      this->firstZ[i] = NA_REAL;
      try {
        this->nextGeometry(context, geometry);
      } catch(NumericVector firstCoords) {
        this->firstX[i] = firstCoords[0];
        this->firstY[i] = firstCoords[1];
        this->firstZ[i] = firstCoords[2];
      }
    }
  }

  void nextError(GEOSContextHandle_t context, const char* message, size_t i) {
    this->problems[i] = message;
    this->setRowNULL(i);
  }

  void nextCoordinate(GEOSContextHandle_t context, double x, double y, double z) {
    throw NumericVector::create(x, y, z);
  }

  void setRowNULL(size_t i) {
    this->isMissing[i] = true;
    this->isEmpty[i] = true;
    this->geometryTypeId[i] = NA_INTEGER;
    this->nGeometries[i] = NA_INTEGER;
    this->nCoordinates[i] = NA_INTEGER;
    this->srid[i] = NA_INTEGER;
    this->coordinateDimensions[i] = NA_INTEGER;
    this->firstX[i] = NA_REAL;
    this->firstY[i] = NA_REAL;
    this->firstZ[i] = NA_REAL;
  }

  SEXP assemble(GEOSContextHandle_t context) {
    return List::create(
      _["is_empty"] = this->isEmpty,
      _["geometry_type"] = this->geometryTypeId,
      _["n_geometries"] = this->nGeometries,
      _["n_coordinates"] = this->nCoordinates,
      _["srid"] = this->srid,
      _["coordinate_dimensions"] = this->coordinateDimensions,
      _["first_coordinate"] = GeovctrsFactory::newXY(this->firstX, this->firstY),
      _["problems"] = this->problems,
      _["is_missing"] = this->isMissing
    );
  }
};

// [[Rcpp::export]]
List geovctrs_cpp_summary(SEXP x) {
  SummaryOperator op;
  op.initProvider(x);
  return op.operate();
}
