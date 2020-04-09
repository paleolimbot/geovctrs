
#include "geos-operator.h"
using namespace Rcpp;

// [[Rcpp::export]]
CharacterVector cpp_validate_provider(SEXP data) {
  std::unique_ptr<GeometryProvider> provider = GeometryProviderFactory::get(data);
  CharacterVector problems(provider->size());

  GEOSContextHandle_t context = geos_init();
  provider->init(context);

  for (size_t i=0; i < provider->size(); i++) {
    try {
      provider->getNext(context, i);
      problems[i] = NA_STRING;
    } catch(Rcpp::exception e) {
      problems[i] = e.what();
    } catch(std::exception e) {
      provider->finish(context);
      geos_finish(context);
      throw e;
    }
  }

  provider->finish(context);
  geos_finish(context);
  return problems;
}
