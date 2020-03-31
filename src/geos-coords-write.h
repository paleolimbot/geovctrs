
#ifndef GEOS_COORDS_WRITE_H
#define GEOS_COORDS_WRITE_H

#include <geos_c.h>
#include <Rcpp.h>
using namespace Rcpp;

GEOSGeometry* feature_from_geo_coord(GEOSContextHandle_t context, List feature);

GEOSGeometry* point_from_geo_coord(GEOSContextHandle_t context, List feature);
GEOSGeometry* linestring_from_geo_coord(GEOSContextHandle_t context, List feature);
GEOSGeometry* polygon_from_geo_coord(GEOSContextHandle_t context, List feature);

GEOSGeometry* multipoint_from_geo_coord(GEOSContextHandle_t context, List feature);
GEOSGeometry* multilinestring_from_geo_coord(GEOSContextHandle_t context, List feature);
GEOSGeometry* multipolygon_from_geo_coord(GEOSContextHandle_t context, List feature);

GEOSGeometry* geometrycollection_from_geo_coord(GEOSContextHandle_t context, List feature);

#endif
