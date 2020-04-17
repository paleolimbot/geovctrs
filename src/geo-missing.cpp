
#include "geovctrs/geos/operator.hpp"
using namespace Rcpp;

// [[Rcpp::interfaces(r, cpp)]]

class BadCoordinateOperator: public GeovctrsRecursiveOperator {
public:
  LogicalVector badCoordinate;

  void init(GEOSContextHandle_t context, size_t size) {
    badCoordinate = LogicalVector(size);
  }

  void nextFeature(GEOSContextHandle_t context, GEOSGeometry* geometry, size_t i) {
    if (geometry == NULL) {
      this->badCoordinate[i] = NA_LOGICAL;
    } else {
      try {
        GeovctrsRecursiveOperator::nextFeature(context, geometry, i);
        this->badCoordinate[i] = false;
      } catch(NumericVector badCoordinates) {
        this->badCoordinate[i] = true;
      }
    }
  }

  SEXP assemble(GEOSContextHandle_t context) {
    return this->badCoordinate;
  }
};

class HasMissingOperator: public BadCoordinateOperator {
  void nextCoordinate(GEOSContextHandle_t context, double x, double y) {
    if (NumericVector::is_na(x) || NumericVector::is_na(y)) {
      throw NumericVector::create(x, y);
    }
  }

  void nextCoordinate(GEOSContextHandle_t context, double x, double y, double z) {
    if (NumericVector::is_na(x) || NumericVector::is_na(y) || NumericVector::is_na(z)) {
      throw NumericVector::create(x, y, z);
    }
  }
};

// [[Rcpp::export]]
LogicalVector geovctrs_cpp_has_missing(SEXP x) {
  HasMissingOperator op;
  op.initProvider(x);
  return op.operate();
}

class HasMissingOrInfiniteOperator: public BadCoordinateOperator {
  void nextCoordinate(GEOSContextHandle_t context, double x, double y) {
    if (NumericVector::is_na(x) ||
        NumericVector::is_na(y) ||
        x == R_PosInf || x == R_NegInf ||
        y == R_PosInf || y == R_NegInf) {
      throw NumericVector::create(x, y);
    }
  }

  void nextCoordinate(GEOSContextHandle_t context, double x, double y, double z) {
    if (NumericVector::is_na(x) ||
        NumericVector::is_na(y) ||
        x == R_PosInf || x == R_NegInf ||
        y == R_PosInf || y == R_NegInf ||
        z == R_PosInf || z == R_NegInf) {
      throw NumericVector::create(x, y, z);
    }
  }
};

// [[Rcpp::export]]
LogicalVector geovctrs_cpp_has_missing_or_infinite(SEXP x) {
  HasMissingOrInfiniteOperator op;
  op.initProvider(x);
  return op.operate();
}
