
#include "geovctrs/geos/operator.hpp"
#include <Rcpp.h>
using namespace Rcpp;


class TestBufferOp: public GeovctrsGEOSGeometryOperator {
  GEOSGeometry* operateNext(GEOSContextHandle_t context, GEOSGeometry* geometry, R_xlen_t i) {
    // width, segments per quandrant
    return GEOSBuffer_r(context, geometry, 2, 30);
  }
};

// [[Rcpp::export]]
SEXP geovctrs_cpp_test_buffer2(SEXP data, SEXP ptype) {
  TestBufferOp op;
  op.initProvider(data);
  op.initExporter(ptype);
  return op.operate();
}

// [[Rcpp::export]]
SEXP geovctrs_cpp_test_buffer2_bad_provider(SEXP data, SEXP ptype) {
  TestBufferOp op;
  op.initExporter(ptype);
  return op.operate();
}

// [[Rcpp::export]]
SEXP geovctrs_cpp_test_buffer2_bad_exporter(SEXP data, SEXP ptype) {
  TestBufferOp op;
  op.initProvider(data);
  return op.operate();
}

class TestRecursiveIdentityOp: public GeovctrsGEOSRecursiveGeometryOperator {

};

// [[Rcpp::export]]
SEXP geovctrs_cpp_test_recursive_identity(SEXP data, SEXP ptype) {
  TestRecursiveIdentityOp op;
  op.initProvider(data);
  op.initExporter(ptype);
  return op.operate();
}
