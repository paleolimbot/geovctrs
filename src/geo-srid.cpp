
#include "geos-operator.h"
using namespace Rcpp;

class GetSRIDOperator: public UnaryVectorOperator<IntegerVector, int> {
  int operateNext(GEOSGeometry* geometry) {
    if (geometry == NULL) {
      return NA_INTEGER;
    } else {
      return GEOSGetSRID_r(this->context, geometry);
    }
  }
};

// [[Rcpp::export]]
IntegerVector cpp_get_srid(SEXP x) {
  GetSRIDOperator op;
  op.initProvider(x);
  return op.operate();
}

class SetSRIDOperator: public UnaryGeometryOperator {
public:
  IntegerVector srid;

  SetSRIDOperator(IntegerVector srid) {
    this->srid = srid;
  }

  size_t getMaxParameterLength() {
    return srid.size();
  }

  GEOSGeometry* operateNext(GEOSGeometry* geometry) {
    if (geometry != NULL) {
      GEOSSetSRID_r(this->context, geometry, this->srid[this->counter]);
    }

    return geometry;
  }
};

// [[Rcpp::export]]
SEXP cpp_set_srid(SEXP x, IntegerVector srid) {
  SetSRIDOperator op(srid);
  // returning the same type as input
  op.initProvider(x, x);
  return op.operate();
}