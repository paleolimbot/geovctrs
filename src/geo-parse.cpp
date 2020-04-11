
#include "geovctrs/operator.hpp"
using namespace Rcpp;

// [[Rcpp::interfaces(r, cpp)]]

class ParseOperator: public GeovctrsRecursiveOperator {
public:
  CharacterVector problems;

  void init(GEOSContextHandle_t context, size_t size) {
    CharacterVector problems(size);
    this->problems = problems;
  }

  void nextFeature(GEOSContextHandle_t context, GEOSGeometry* geometry, size_t i) {
    this->problems[i] = NA_STRING;
  }

  void nextError(GEOSContextHandle_t context, const char* message, size_t i) {
    this->problems[i] = message;
  }

  SEXP assemble(GEOSContextHandle_t context) {
    return this->problems;
  }
};

// [[Rcpp::export]]
CharacterVector geovctrs_cpp_parse(SEXP data) {
  ParseOperator op;
  op.initProvider(data);
  return op.operate();
}
