
#include "wk/wkt-reader.hpp"
#include "wk/wkb-reader.hpp"
#include "wk/rcpp-sexp-reader.hpp"

#include <Rcpp.h>
using namespace Rcpp;

class WKHasSomethingException: public WKParseException {
public:
  const static int CODE_HAS_SOMETHING = 2948379;
  WKHasSomethingException(): WKParseException(CODE_HAS_SOMETHING) {}
};

class WKHasSomethingHandler: public WKGeometryHandler {
public:
  LogicalVector output;
  WKHasSomethingHandler(size_t size): output(size) {}

  void nextFeatureStart(size_t featureId) {
    this->featureIsNull = false;
  }

  void nextNull(size_t featureId) {
    this->featureIsNull = true;
  }

  void nextFeatureEnd(size_t featureId) {
    if (this->featureIsNull) {
      this->output[featureId] = NA_LOGICAL;
    } else {
      this->output[featureId] = false;
    }
  }

  bool nextError(WKParseException& error, size_t featureId) {
    if (error.code() == WKHasSomethingException::CODE_HAS_SOMETHING) {
      this->output[featureId] = true;
      return true;
    } else {
      return false;
    }
  }

private:
  bool featureIsNull;
};

class WKHasNonFiniteHandler: public WKHasSomethingHandler {
public:
  WKHasNonFiniteHandler(size_t size): WKHasSomethingHandler(size) {}
  void nextCoordinate(const WKGeometryMeta& meta, const WKCoord& coord, uint32_t coordId) {
    for (size_t i = 0; i < coord.size(); i++) {
      double value = coord[i];
      if (!std::isfinite(value)) {
        throw WKHasSomethingException();
      }
    }
  }
};

class WKHasMissingHandler: public WKHasSomethingHandler {
public:
  WKHasMissingHandler(size_t size): WKHasSomethingHandler(size) {}
  void nextCoordinate(const WKGeometryMeta& meta, const WKCoord& coord, uint32_t coordId) {
    for (size_t i = 0; i < coord.size(); i++) {
      double value = coord[i];
      if (std::isnan(value)) {
        throw WKHasSomethingException();
      }
    }
  }
};

LogicalVector has_non_finite_base(WKReader& reader) {
  WKHasNonFiniteHandler handler(reader.nFeatures());
  reader.setHandler(&handler);
  while (reader.hasNextFeature()) {
    checkUserInterrupt();
    reader.iterateFeature();
  }

  return handler.output;
}

// [[Rcpp::export]]
LogicalVector cpp_wkt_has_non_finite(CharacterVector wkt) {
  WKCharacterVectorProvider provider(wkt);
  WKTReader reader(provider);
  return has_non_finite_base(reader);
}

// [[Rcpp::export]]
LogicalVector cpp_wkb_has_non_finite(List wkb) {
  WKRawVectorListProvider provider(wkb);
  WKBReader reader(provider);
  return has_non_finite_base(reader);
}

// [[Rcpp::export]]
LogicalVector cpp_wksxp_has_non_finite(List wksxp) {
  WKRcppSEXPProvider provider(wksxp);
  WKRcppSEXPReader reader(provider);
  return has_non_finite_base(reader);
}

LogicalVector has_missing_base(WKReader& reader) {
  WKHasMissingHandler handler(reader.nFeatures());
  reader.setHandler(&handler);
  while (reader.hasNextFeature()) {
    checkUserInterrupt();
    reader.iterateFeature();
  }

  return handler.output;
}

// [[Rcpp::export]]
LogicalVector cpp_wkt_has_missing(CharacterVector wkt) {
  WKCharacterVectorProvider provider(wkt);
  WKTReader reader(provider);
  return has_missing_base(reader);
}

// [[Rcpp::export]]
LogicalVector cpp_wkb_has_missing(List wkb) {
  WKRawVectorListProvider provider(wkb);
  WKBReader reader(provider);
  return has_missing_base(reader);
}

// [[Rcpp::export]]
LogicalVector cpp_wksxp_has_missing(List wksxp) {
  WKRcppSEXPProvider provider(wksxp);
  WKRcppSEXPReader reader(provider);
  return has_missing_base(reader);
}
