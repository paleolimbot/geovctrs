
#include "geos-provider.h"
using namespace Rcpp;

std::unique_ptr<GeometryProvider> resolve_provider(SEXP data) {
  if (Rf_inherits(data, "geovctrs_wkt")) {
    CharacterVector dataChar = (CharacterVector) data;

    if (dataChar.size() ==  1) {
      return std::unique_ptr<GeometryProvider> { new ConstantGeometryProvider(new WKTGeometryProvider(dataChar)) };
    } else {
      return std::unique_ptr<GeometryProvider> { new WKTGeometryProvider(dataChar) };
    }

  } else if(Rf_inherits(data, "geovctrs_wkb")) {
    List dataList = (List) data;

    if (dataList.size() ==  1) {
      return std::unique_ptr<GeometryProvider> { new ConstantGeometryProvider(new WKBGeometryProvider(dataList)) };
    } else {
      return std::unique_ptr<GeometryProvider> { new WKBGeometryProvider(dataList) };
    }

  } else if(Rf_inherits(data, "geovctrs_xy")) {
    List xy = (List)data;
    if (Rf_length(xy[0]) ==  1) {
      return std::unique_ptr<GeometryProvider> { new ConstantGeometryProvider(new XYProvider(data)) };
    } else {
      return std::unique_ptr<GeometryProvider> { new XYProvider(data) };
    }
  } else if(Rf_inherits(data, "geovctrs_segment")) {
    List segment = (List) data;

    if (Rf_length(segment[0]) == 1) {
      return std::unique_ptr<GeometryProvider> {
        new ConstantGeometryProvider(new SegmentProvider(segment))
      };
    } else {
      return std::unique_ptr<GeometryProvider> {
        new SegmentProvider(segment)
      };
    }
  } else if(Rf_inherits(data, "geovctrs_rect")) {
    List rect = (List) data;

    if (Rf_length(rect[0]) ==  1) {
      return std::unique_ptr<GeometryProvider> {
        new ConstantGeometryProvider(new GeoRectProvider(rect))
      };
    } else {
      return std::unique_ptr<GeometryProvider> {
        new GeoRectProvider(rect)
      };
    }
  } else if(Rf_inherits(data, "geovctrs_collection")) {
    List col = (List) data;

    if (Rf_length(col[0]) ==  1) {
      return std::unique_ptr<GeometryProvider> { new ConstantGeometryProvider(new GeoCollectionProvider(col)) };
    } else {
      return std::unique_ptr<GeometryProvider> { new GeoCollectionProvider(col) };
    }
  }

  stop("Can't resolve GeometryProvider");
}

std::unique_ptr<GeometryExporter> resolve_exporter(SEXP ptype) {
  if (Rf_inherits(ptype, "geovctrs_wkt")) {
    return std::unique_ptr<GeometryExporter> {
      new WKTGeometryExporter(ptype)
    };
  } else if(Rf_inherits(ptype, "geovctrs_wkb")) {
    return std::unique_ptr<GeometryExporter> {
      new WKBGeometryExporter(ptype)
    };
  } else if(Rf_inherits(ptype, "geovctrs_collection")) {
    return std::unique_ptr<GeometryExporter> { new GeoCollectionExporter() };
  } else if(Rf_inherits(ptype, "geovctrs_xy")) {
    return std::unique_ptr<GeometryExporter> { new XYExporter() };
  } else if(Rf_inherits(ptype, "geovctrs_segment")) {
    return std::unique_ptr<GeometryExporter> { new SegmentExporter() };
  }

  stop("Can't resolve GeometryExporter");
}
