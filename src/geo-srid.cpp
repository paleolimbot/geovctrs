
#include "geos-operator.h"
using namespace Rcpp;

class GetSRIDOperator: public UnaryVectorOperator<IntegerVector, int> {
  int operateNext(GEOSContextHandle_t context, GEOSGeometry* geometry, size_t i) {
    if (geometry == NULL) {
      return NA_INTEGER;
    } else {
      return GEOSGetSRID_r(context, geometry);
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

  GEOSGeometry* operateNext(GEOSContextHandle_t context, GEOSGeometry* geometry, size_t i) {
    if (geometry != NULL) {
      GEOSSetSRID_r(context, geometry, this->srid[i]);
    }

    return geometry;
  }
};

// [[Rcpp::export]]
SEXP cpp_set_srid(SEXP x, IntegerVector srid) {
  SetSRIDOperator op(srid);
  // returning the same type as input
  op.initProvider(x);
  op.initExporter(x);
  return op.operate();
}
