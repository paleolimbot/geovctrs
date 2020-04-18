
#include "geovctrs/geos/operator.hpp"
using namespace Rcpp;

class SetZOperator: public GeovctrsGEOSRecursiveGeometryOperator {
public:
  NumericVector z;

  SetZOperator(NumericVector z) {
    this->z = z;
  }

  size_t getMaxParameterLength() {
    return z.size();
  }

  void nextCoordinate(GEOSContextHandle_t context, double* x, double* y, double* z) {
    *z = this->z[this->featureId];
  }
};

// [[Rcpp::export]]
SEXP geovctrs_cpp_set_z(SEXP x, NumericVector z) {
  SetZOperator op(z);
  // returning the same type as input
  op.initProvider(x);
  op.initExporter(x);
  return op.operate();
}
