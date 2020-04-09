
#include "geos-operator.h"
using namespace Rcpp;


class ParseOperator: public Operator {
public:
  CharacterVector problems;

  void init(GEOSContextHandle_t context, size_t size) {
    CharacterVector problems(size);
    this->problems = problems;
  }

  void loopNext(GEOSContextHandle_t context, size_t i) {
    try {
      // assign geometry so that Operator destroys it
      this->geometry = this->provider->getNext(context, i);
      this->problems[i] = NA_STRING;
    } catch(Rcpp::exception e) {
      this->problems[i] = e.what();
    } catch(std::exception e) {
      provider->finish(context);
      throw e;
    }
  }

  SEXP assemble(GEOSContextHandle_t context) {
    return this->problems;
  }
};

// [[Rcpp::export]]
CharacterVector cpp_parse(SEXP data) {
  ParseOperator op;
  op.initProvider(data);
  return op.operate();
}
