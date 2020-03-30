
#ifndef GEOS_COORDS_H
#define GEOS_COORDS_H

#include <geos_c.h>
#include <Rcpp.h>
using namespace Rcpp;

List geo_coord_reclass(List input, const char* cls);

List geometry_to_geo_coord(GEOSContextHandle_t context, GEOSGeometry* geometry);

List point_to_geo_coord(GEOSContextHandle_t context, GEOSGeometry* geometry);
List linestring_to_geo_coord(GEOSContextHandle_t context, GEOSGeometry* geometry);
List polygon_to_geo_coord(GEOSContextHandle_t context, GEOSGeometry* geometry);

List multipoint_to_geo_coord(GEOSContextHandle_t context, GEOSGeometry* geometry);
List multilinestring_to_geo_coord(GEOSContextHandle_t context, GEOSGeometry* geometry);
List multipolygon_to_geo_coord(GEOSContextHandle_t context, GEOSGeometry* geometry);

List geometrycollection_to_geo_coord(GEOSContextHandle_t context, GEOSGeometry* geometry);

#endif
