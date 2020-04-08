
#include "geos-coords-write.h"
using namespace Rcpp;

IntegerVector groups_to_lengths(IntegerVector groups) {
  if (groups.size() == 0) {
    return IntegerVector::create();
  }

  // one pass through to find the number of rings
  int nGroups = 1;
  for (size_t i=1; i < groups.size(); i++) {
    if (groups[i] != groups[i - 1]) {
      nGroups++;
    }
  }

  // one pass through to find their lengths
  IntegerVector groupLengths(nGroups);
  int iGroup = 0;
  size_t lastGroup = 0;
  for (size_t i=1; i < groups.size(); i++) {
    if (groups[i] != groups[i - 1]) {
      groupLengths[iGroup] = i - lastGroup;
      lastGroup = i;
      iGroup++;
    }
  }

  groupLengths[iGroup] = groups.size() - lastGroup;
  return groupLengths;
}

GEOSCoordSequence* seq_from_xy(GEOSContextHandle_t context, List xy, int offset, size_t size) {
  NumericVector x = xy["x"];
  NumericVector y = xy["y"];

  GEOSCoordSequence* seq = GEOSCoordSeq_create_r(context, size, 2);
  for (size_t i=0; i<size; i++) {
    GEOSCoordSeq_setX_r(context, seq, i, x[offset + i]);
    GEOSCoordSeq_setY_r(context, seq, i, y[offset + i]);
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

  } else if(Rf_inherits(feature, "geovctrs_collection")) {
    return geometrycollection_from_geo_coord(context, feature);

  } else {
    stop("Can only convert point, linestring, polygon, and multi- variants from a geo_coord");
  }
}

GEOSGeometry* point_from_geo_coord(GEOSContextHandle_t context, List feature) {
  List xy = feature["xy"];
  NumericVector x = xy["x"];
  if (x.size() == 0) {
    return GEOSGeom_createEmptyPoint_r(context);
  }

  GEOSCoordSequence* seq = seq_from_xy(context, xy, 0, x.size());
  return GEOSGeom_createPoint_r(context, seq);
}

GEOSGeometry* linestring_from_geo_coord(GEOSContextHandle_t context, List feature) {
  List xy = feature["xy"];
  NumericVector x = xy["x"];
  if (x.size() == 0) {
    return GEOSGeom_createEmptyLineString_r(context);
  }

  GEOSCoordSequence* seq = seq_from_xy(context, xy, 0, x.size());
  return GEOSGeom_createLineString_r(context, seq);
}

GEOSGeometry* polygon_from_geo_coord(GEOSContextHandle_t context, List feature) {
  List xy = feature["xy"];
  IntegerVector ring = feature["ring"];
  if (ring.size() == 0) {
    return GEOSGeom_createEmptyPolygon_r(context);
  }

  IntegerVector ringLengths = groups_to_lengths(ring);

  // generate outer shell
  GEOSCoordSequence* shellSeq = seq_from_xy(context, xy, 0, ringLengths[0]);
  GEOSGeometry* shell = GEOSGeom_createLinearRing_r(context, shellSeq);

  // generate holes
  GEOSGeometry* holes[ringLengths.size() - 1];
  size_t offset = ringLengths[0];
  for (int i=1; i < ringLengths.size(); i++) {
    GEOSCoordSequence* holeSeq = seq_from_xy(context, xy, offset, ringLengths[i]);
    holes[i - 1] = GEOSGeom_createLinearRing_r(context, holeSeq);
    offset += ringLengths[i];
  }

  // generate polygon
  GEOSGeometry* output = GEOSGeom_createPolygon_r(context, shell, holes, ringLengths.size() - 1);
  return output;
}

GEOSGeometry* multipoint_from_geo_coord(GEOSContextHandle_t context, List feature) {
  List xy = feature["xy"];
  NumericVector x = xy["x"];
  if (x.size() == 0) {
    return GEOSGeom_createEmptyCollection_r(context, GEOSGeomTypes::GEOS_MULTIPOINT);
  }

  GEOSGeometry* parts[x.size()];
  for (size_t i=0; i<x.size(); i++) {
    GEOSCoordSequence* seq = seq_from_xy(context, xy, i, 1);
    parts[i] = GEOSGeom_createPoint_r(context, seq);
  }

  GEOSGeometry* output = GEOSGeom_createCollection_r(context, GEOSGeomTypes::GEOS_MULTIPOINT, parts, x.size());
  return output;
}

GEOSGeometry* multilinestring_from_geo_coord(GEOSContextHandle_t context, List feature) {
  IntegerVector part = feature["part"];
  if (part.size() == 0) {
    return GEOSGeom_createEmptyCollection_r(context, GEOSGeomTypes::GEOS_MULTILINESTRING);
  }

  List xy = feature["xy"];
  IntegerVector partLengths = groups_to_lengths(part);

  GEOSGeometry* parts[partLengths.size()];
  size_t offset = 0;
  for (int i=0; i < partLengths.size(); i++) {
    GEOSCoordSequence* lineSeq = seq_from_xy(context, xy, offset, partLengths[i]);
    parts[i] = GEOSGeom_createLineString_r(context, lineSeq);
    offset += partLengths[i];
  }

  GEOSGeometry* output = GEOSGeom_createCollection_r(
    context,
    GEOSGeomTypes::GEOS_MULTILINESTRING,
    parts,
    partLengths.size()
  );

  return output;
}

GEOSGeometry* multipolygon_from_geo_coord(GEOSContextHandle_t context, List feature) {
  IntegerVector part = feature["part"];
  if (part.size() == 0) {
    return GEOSGeom_createEmptyCollection_r(context, GEOSGeomTypes::GEOS_MULTIPOLYGON);
  }

  List xy = feature["xy"];
  IntegerVector ring = feature["ring"];
  IntegerVector partLengths = groups_to_lengths(part);

  GEOSGeometry* parts[partLengths.size()];
  size_t offset = 0;
  for (int i=0; i < partLengths.size(); i++) {
    IntegerVector ringPart = ring[Range(offset, offset + partLengths[i] - 1)];
    IntegerVector ringLengths = groups_to_lengths(ringPart);

    // generate outer shell
    GEOSCoordSequence* shellSeq = seq_from_xy(context, xy, offset, ringLengths[0]);
    GEOSGeometry* shell = GEOSGeom_createLinearRing_r(context, shellSeq);
    offset += ringLengths[0];

    // generate holes
    GEOSGeometry* holes[ringLengths.size() - 1];
    for (int j=1; j < ringLengths.size(); j++) {
      GEOSCoordSequence* holeSeq = seq_from_xy(context, xy, offset, ringLengths[j]);
      holes[j - 1] = GEOSGeom_createLinearRing_r(context, holeSeq);
      offset += ringLengths[j];
    }

    // generate polygon
    parts[i] = GEOSGeom_createPolygon_r(context, shell, holes, ringLengths.size() - 1);
  }


  GEOSGeometry* output = GEOSGeom_createCollection_r(
    context,
    GEOSGeomTypes::GEOS_MULTIPOLYGON,
    parts,
    partLengths.size()
  );

  return output;
}

GEOSGeometry* geometrycollection_from_geo_coord(GEOSContextHandle_t context, List data) {
  IntegerVector srid = data["srid"];
  List feature = data["feature"];
  GEOSGeometry* parts[feature.size()];

  for (size_t i=0; i < feature.size(); i++) {
    GEOSGeometry* geometry = feature_from_geo_coord(context, feature[i]);
    GEOSSetSRID_r(context, geometry, srid[i]);

    parts[i] = geometry;
  }

  GEOSGeometry* output = GEOSGeom_createCollection_r(
    context,
    GEOSGeomTypes::GEOS_GEOMETRYCOLLECTION,
    parts,
    feature.size()
  );

  return output;
}
