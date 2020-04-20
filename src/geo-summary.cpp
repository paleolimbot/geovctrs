
#include "geovctrs/geos/operator.hpp"
#include "geovctrs/factory.hpp"
using namespace Rcpp;

// [[Rcpp::interfaces(r, cpp)]]

class SummaryOperator: public GeovctrsGEOSRecursiveOperator {
public:
  LogicalVector isEmpty;
  IntegerVector geometryTypeId;
  IntegerVector nGeometries;
  IntegerVector nCoordinates;
  IntegerVector srid;
  IntegerVector coordinateDimensions;
  LogicalVector hasZ;
  NumericVector firstX;
  NumericVector firstY;
  NumericVector firstZ;
  CharacterVector problems;
  LogicalVector isMissing;

  bool anyHasZ;

  void init(GEOSContextHandle_t context, R_xlen_t size) {
    this->isEmpty = LogicalVector(size);
    this->geometryTypeId = IntegerVector(size);
    this->nGeometries = IntegerVector(size);
    this->nCoordinates = IntegerVector(size);
    this->srid = IntegerVector(size);
    this->coordinateDimensions = IntegerVector(size);
    this->hasZ = LogicalVector(size);
    this->firstX = NumericVector(size);
    this->firstY = NumericVector(size);
    this->firstZ = NumericVector(size);
    this->problems = CharacterVector(size);
    this->isMissing = LogicalVector(size);

    anyHasZ = false;
  }

  void nextFeature(GEOSContextHandle_t context, GEOSGeometry* geometry, R_xlen_t i) {
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
      this->hasZ[i] = GEOSHasZ_r(context, geometry);
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

  void nextError(GEOSContextHandle_t context, const char* message, R_xlen_t i) {
    this->problems[i] = message;
    this->setRowNULL(i);
  }

  void nextXY(GEOSContextHandle_t context, double x, double y) {
    throw NumericVector::create(x, y, NA_REAL);
  }

  void nextXYZ(GEOSContextHandle_t context, double x, double y, double z) {
    this->anyHasZ = true;
    throw NumericVector::create(x, y, z);
  }

  void setRowNULL(R_xlen_t i) {
    this->isMissing[i] = true;
    this->isEmpty[i] = true;
    this->geometryTypeId[i] = NA_INTEGER;
    this->nGeometries[i] = NA_INTEGER;
    this->nCoordinates[i] = NA_INTEGER;
    this->srid[i] = NA_INTEGER;
    this->coordinateDimensions[i] = NA_INTEGER;
    this->hasZ[i] = NA_LOGICAL;
    this->firstX[i] = NA_REAL;
    this->firstY[i] = NA_REAL;
    this->firstZ[i] = NA_REAL;
  }

  SEXP assemble(GEOSContextHandle_t context) {
    List firstCoord;
    if (this->anyHasZ) {
      firstCoord = GeovctrsFactory::newXYZ(this->firstX, this->firstY, this->firstZ);
    } else {
      firstCoord = GeovctrsFactory::newXY(this->firstX, this->firstY);
    }

    return List::create(
      _["is_empty"] = this->isEmpty,
      _["geometry_type"] = this->geometryTypeId,
      _["n_geometries"] = this->nGeometries,
      _["n_coordinates"] = this->nCoordinates,
      _["srid"] = this->srid,
      _["coordinate_dimensions"] = this->coordinateDimensions,
      _["has_z"] = this->hasZ,
      _["first_coordinate"] = firstCoord,
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
