
#include "geos-coords-write.h"
using namespace Rcpp;

GEOSCoordSequence* seq_from_xy(GEOSContextHandle_t context, List xy, int offset, size_t size) {
  NumericVector x = xy["x"];
  NumericVector y = xy["y"];

  GEOSCoordSequence* seq = GEOSCoordSeq_create_r(context, size, 2);
  for (size_t i=0; i<size; i++) {
    GEOSCoordSeq_setX_r(context, seq, i, offset + x[i]);
    GEOSCoordSeq_setY_r(context, seq, i, offset + y[i]);
  }

  return seq;
}

GEOSGeometry* feature_from_geo_coord(GEOSContextHandle_t context, List feature) {
  if (Rf_inherits(feature, "geo_point")) {
    return point_from_geo_coord(context, feature);

  } else if(Rf_inherits(feature, "geo_linestring")) {
    return linestring_from_geo_coord(context, feature);

  } else if(Rf_inherits(feature, "geo_polygon")) {
    return polygon_from_geo_coord(context, feature);

  } else if(Rf_inherits(feature, "geo_multipoint")) {
    return multipoint_from_geo_coord(context, feature);

  } else if(Rf_inherits(feature, "geo_multilinestring")) {
    return multilinestring_from_geo_coord(context, feature);

  } else if(Rf_inherits(feature, "geo_multipolygon")) {
    return multipolygon_from_geo_coord(context, feature);

  } else if(Rf_inherits(feature, "geo_collection")) {
    return geometrycollection_from_geo_coord(context, feature);

  } else {
    stop("Can only convert point, linestring, polygon, and multi- variants from a geo_coord");
  }
}

GEOSGeometry* point_from_geo_coord(GEOSContextHandle_t context, List feature) {
  List xy = feature["xy"];
  NumericVector x = xy["x"];
  GEOSCoordSequence* seq = seq_from_xy(context, xy, 0, x.size());
  return GEOSGeom_createPoint_r(context, seq);
}

GEOSGeometry* linestring_from_geo_coord(GEOSContextHandle_t context, List feature) {
  List xy = feature["xy"];
  NumericVector x = xy["x"];
  GEOSCoordSequence* seq = seq_from_xy(context, xy, 0, x.size());
  return GEOSGeom_createLineString_r(context, seq);
}

GEOSGeometry* polygon_from_geo_coord(GEOSContextHandle_t context, List feature) {
  stop("Can only convert point");
}

GEOSGeometry* multipoint_from_geo_coord(GEOSContextHandle_t context, List feature) {
  stop("Can only convert point");
}

GEOSGeometry* multilinestring_from_geo_coord(GEOSContextHandle_t context, List feature) {
  stop("Can only convert point");
}

GEOSGeometry* multipolygon_from_geo_coord(GEOSContextHandle_t context, List feature) {
  stop("Can only convert point");
}

GEOSGeometry* geometrycollection_from_geo_coord(GEOSContextHandle_t context, List feature) {
  stop("Can only convert point");
}
