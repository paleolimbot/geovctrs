
#include "geos-provider.h"
using namespace Rcpp;

// [[Rcpp::export]]
CharacterVector cpp_validate_provider(SEXP data) {
  std::unique_ptr<GeometryProvider> provider = resolve_provider(data);
  CharacterVector problems(provider->size());

  GEOSContextHandle_t context = geos_init();
  provider->init(context);

  for (size_t i=0; i < provider->size(); i++) {
    try {
      provider->getNext();
      problems[i] = NA_STRING;
    } catch(Rcpp::exception e) {
      problems[i] = e.what();
    } catch(std::exception e) {
      provider->finish();
      geos_finish(context);
      throw e;
    }
  }

  provider->finish();
  geos_finish(context);
  return problems;
}
