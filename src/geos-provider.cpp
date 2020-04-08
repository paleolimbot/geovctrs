
#include "geos-provider.h"
#include <Rcpp.h>
using namespace Rcpp;

// ---------- geometry provider implementations -------------

// ---------- geometry provider resolvers -------------

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
    List xy = (List) data;
    NumericVector x = xy["x"];
    NumericVector y = xy["y"];

    if (x.size() ==  1) {
      return std::unique_ptr<GeometryProvider> { new ConstantGeometryProvider(new XYProvider(x, y)) };
    } else {
      return std::unique_ptr<GeometryProvider> { new XYProvider(x, y) };
    }
  } else if(Rf_inherits(data, "geovctrs_segment")) {
    List segment = (List) data;
    List start = segment["start"];
    List end = segment["end"];
    NumericVector x0 = start["x"];
    NumericVector y0 = start["y"];
    NumericVector x1 = end["x"];
    NumericVector y1 = end["y"];
    IntegerVector srid = segment["srid"];

    if (x0.size() ==  1) {
      return std::unique_ptr<GeometryProvider> {
        new ConstantGeometryProvider(new SegmentProvider(x0, y0, x1, y1, srid))
      };
    } else {
      return std::unique_ptr<GeometryProvider> {
        new SegmentProvider(x0, y0, x1, y1, srid)
      };
    }
  } else if(Rf_inherits(data, "geovctrs_rect")) {
    List rect = (List) data;
    NumericVector xmin = rect["xmin"];
    NumericVector ymin = rect["ymin"];
    NumericVector xmax = rect["xmax"];
    NumericVector ymax = rect["ymax"];
    IntegerVector srid = rect["srid"];

    if (xmin.size() ==  1) {
      return std::unique_ptr<GeometryProvider> {
        new ConstantGeometryProvider(new GeoRectProvider(xmin, ymin, xmax, ymax, srid))
      };
    } else {
      return std::unique_ptr<GeometryProvider> {
        new GeoRectProvider(xmin, ymin, xmax, ymax, srid)
      };
    }
  } else if(Rf_inherits(data, "geovctrs_collection")) {
    List col = (List) data;
    List features = col["feature"];

    if (features.size() ==  1) {
      return std::unique_ptr<GeometryProvider> { new ConstantGeometryProvider(new GeoCollectionProvider(col)) };
    } else {
      return std::unique_ptr<GeometryProvider> { new GeoCollectionProvider(col) };
    }
  }

  stop("Can't resolve GeometryProvider");
}

std::unique_ptr<GeometryExporter> resolve_exporter(SEXP ptype) {
  if (Rf_inherits(ptype, "geovctrs_wkt")) {
    CharacterVector data = (CharacterVector)ptype;
    bool trim = data.attr("trim");
    int precision = data.attr("precision");
    int dimensions = data.attr("dimensions");

    return std::unique_ptr<GeometryExporter> {
      new WKTGeometryExporter(trim, precision, dimensions)
    };

  } else if(Rf_inherits(ptype, "geovctrs_wkb")) {
    List data = (List)ptype;
    int includeSRID = data.attr("include_srid");
    int dimensions = data.attr("dimensions");
    int endian = data.attr("endian");

    return std::unique_ptr<GeometryExporter> {
      new WKBGeometryExporter(includeSRID, dimensions, endian)
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
