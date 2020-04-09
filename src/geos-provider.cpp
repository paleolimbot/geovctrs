
#include "geos-provider.h"
using namespace Rcpp;

std::unique_ptr<GeometryProvider> resolve_provider(SEXP data) {
  std::unique_ptr<GeometryProvider> provider;

  if (Rf_inherits(data, "geovctrs_wkt")) {
    provider = std::unique_ptr<GeometryProvider> { new WKTGeometryProvider(data) };
  } else if(Rf_inherits(data, "geovctrs_wkb")) {
    provider = std::unique_ptr<GeometryProvider> { new WKBGeometryProvider(data) };
  } else if(Rf_inherits(data, "geovctrs_xy")) {
    provider = std::unique_ptr<GeometryProvider> { new XYProvider(data) };
  } else if(Rf_inherits(data, "geovctrs_segment")) {
    provider = std::unique_ptr<GeometryProvider> { new SegmentProvider(data) };
  } else if(Rf_inherits(data, "geovctrs_rect")) {
    provider = std::unique_ptr<GeometryProvider> { new GeoRectProvider(data) };
  } else if(Rf_inherits(data, "geovctrs_collection")) {
    provider = std::unique_ptr<GeometryProvider> { new GeoCollectionProvider(data) };
  } else {
    stop("Can't resolve GeometryProvider");
  }

  if (provider->size() == 1) {
    return std::unique_ptr<GeometryProvider> { new ConstantGeometryProvider(provider.release()) };
  } else {
    return provider;
  }
}

std::unique_ptr<GeometryExporter> resolve_exporter(SEXP ptype) {
  if (Rf_inherits(ptype, "geovctrs_wkt")) {
    return std::unique_ptr<GeometryExporter> { new WKTGeometryExporter(ptype) };
  } else if(Rf_inherits(ptype, "geovctrs_wkb")) {
    return std::unique_ptr<GeometryExporter> { new WKBGeometryExporter(ptype) };
  } else if(Rf_inherits(ptype, "geovctrs_collection")) {
    return std::unique_ptr<GeometryExporter> { new GeoCollectionExporter() };
  } else if(Rf_inherits(ptype, "geovctrs_xy")) {
    return std::unique_ptr<GeometryExporter> { new XYExporter() };
  } else if(Rf_inherits(ptype, "geovctrs_segment")) {
    return std::unique_ptr<GeometryExporter> { new SegmentExporter() };
  } else {
    stop("Can't resolve GeometryExporter");
  }
}
