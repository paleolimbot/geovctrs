
#ifndef GEOVCTRS_GEOS_FEATURE_FACTORY_HPP
#define GEOVCTRS_GEOS_FEATURE_FACTORY_HPP

#include "../factory.hpp"
#include <geos_c.h>
#include <Rcpp.h>
using namespace Rcpp;

class GeovctrsGEOSFeatureFactory {
public:

  static List getFeature(GEOSContextHandle_t context, GEOSGeometry* geometry) {
    int type = GEOSGeomTypeId_r(context, geometry);

    if (type == GEOSGeomTypes::GEOS_POINT) {
      return getPoint(context, geometry);

    } else if(type == GEOSGeomTypes::GEOS_LINESTRING) {
      return getLinestring(context, geometry);

    } else if(type == GEOSGeomTypes::GEOS_POLYGON) {
      return getPolygon(context, geometry);

    } else if(type == GEOSGeomTypes::GEOS_MULTIPOINT) {
      return getMultipoint(context, geometry);

    } else if(type == GEOSGeomTypes::GEOS_MULTILINESTRING) {
      return getMultilinestring(context, geometry);

    } else if(type == GEOSGeomTypes::GEOS_MULTIPOLYGON) {
      return getMultipolygon(context, geometry);

    } else if(type == GEOSGeomTypes::GEOS_GEOMETRYCOLLECTION) {
      return getGeometrycollection(context, geometry);

    } else {
      stop("GeovctrsGEOSFeatureFactory::getFeature(): Unrecognized geometry type");
    }
  }

private:
  static List getPoint(GEOSContextHandle_t context, GEOSGeometry* geometry) {
    int nCoordinates = GEOSGetNumCoordinates_r(context, geometry);
    NumericVector x(nCoordinates);
    NumericVector y(nCoordinates);
    writeCoordinates(context, geometry, x, y, 0);

    return GeovctrsFactory::newPoint(x, y);
  }

  static List getLinestring(GEOSContextHandle_t context, GEOSGeometry* geometry) {
    int nCoordinates = GEOSGetNumCoordinates_r(context, geometry);
    NumericVector x(nCoordinates);
    NumericVector y(nCoordinates);
    writeCoordinates(context, geometry, x, y, 0);

    return GeovctrsFactory::newLinestring(x, y);
  }

  static List getPolygon(GEOSContextHandle_t context, GEOSGeometry* geometry) {
    int nInteriorRings = GEOSGetNumInteriorRings_r(context, geometry);
    int nCoordinates = GEOSGetNumCoordinates_r(context, geometry);

    NumericVector x(nCoordinates);
    NumericVector y(nCoordinates);
    IntegerVector ringId(nCoordinates);

    unsigned int offset = 0;
    const GEOSGeometry* exterior = GEOSGetExteriorRing_r(context, geometry);
    offset += writeCoordinates(context, exterior, x, y, ringId, 1, offset);

    for(int i=0; i < nInteriorRings; i++) {
      const GEOSGeometry* ring = GEOSGetInteriorRingN_r(context, geometry, i);
      offset += writeCoordinates(context, ring, x, y, ringId, 2 + i, offset);
    }

    return GeovctrsFactory::newPolygon(x, y, ringId);
  }

  static List getMultipoint(GEOSContextHandle_t context, GEOSGeometry* geometry) {
    int nParts = GEOSGetNumGeometries_r(context, geometry);
    int nCoordinates = GEOSGetNumCoordinates_r(context, geometry);

    NumericVector x(nCoordinates);
    NumericVector y(nCoordinates);

    unsigned int offset = 0;

    for(int i=0; i < nParts; i++) {
      const GEOSGeometry* part = GEOSGetGeometryN_r(context, geometry, i);
      offset += writeCoordinates(context, part, x, y, offset);
    }

    return GeovctrsFactory::newMultipoint(x, y);
  }

  static List getMultilinestring(GEOSContextHandle_t context, GEOSGeometry* geometry) {
    int nParts = GEOSGetNumGeometries_r(context, geometry);
    int nCoordinates = GEOSGetNumCoordinates_r(context, geometry);

    NumericVector x(nCoordinates);
    NumericVector y(nCoordinates);
    IntegerVector partId(nCoordinates);

    unsigned int offset = 0;

    for(int i=0; i < nParts; i++) {
      const GEOSGeometry* part = GEOSGetGeometryN_r(context, geometry, i);
      offset += writeCoordinates(context, part, x, y, partId, i+1, offset);
    }

    return GeovctrsFactory::newMultilinestring(x, y, partId);
  }

  static List getMultipolygon(GEOSContextHandle_t context, GEOSGeometry* geometry) {
    int nParts = GEOSGetNumGeometries_r(context, geometry);
    int nCoordinates = GEOSGetNumCoordinates_r(context, geometry);

    NumericVector x(nCoordinates);
    NumericVector y(nCoordinates);
    IntegerVector partId(nCoordinates);
    IntegerVector ringId(nCoordinates);

    unsigned int offset = 0;

    for(int i=0; i < nParts; i++) {
      const GEOSGeometry* part = GEOSGetGeometryN_r(context, geometry, i);
      int nInteriorRings = GEOSGetNumInteriorRings_r(context, part);

      const GEOSGeometry* exterior = GEOSGetExteriorRing_r(context, part);
      offset += writeCoordinates(
        context, exterior,
        x, y,
        partId, i + 1,
        ringId, 1,
        offset
      );

      for(int j=0; j < nInteriorRings; j++) {
        const GEOSGeometry* ring = GEOSGetInteriorRingN_r(context, part, j);
        offset += writeCoordinates(
          context, ring,
          x, y,
          partId, i + 1,
          ringId, 2 + j,
          offset
        );
      }
    }

    return GeovctrsFactory::newMultipolygon(x, y, partId, ringId);
  }

  static List getGeometrycollection(GEOSContextHandle_t context, GEOSGeometry* geometry) {

    int nGeometries = GEOSGetNumGeometries_r(context, geometry);
    List features(nGeometries);
    IntegerVector srid(nGeometries);

    // it's worth doing this here, but these will all be the same as the
    // parent collection (behaviour changed in GEOS 3.8, but we enforce
    // this here for all GEOS versions)
    int geomSRID = GEOSGetSRID_r(context, geometry);

    for (int i=0; i < nGeometries; i++) {
      const GEOSGeometry* feature = GEOSGetGeometryN_r(context, geometry, i);
      features[i] = getFeature(context, (GEOSGeometry*)feature);
      srid[i] = geomSRID;
    }

    return GeovctrsFactory::newCollection(features, srid);
  }

  static unsigned int writeCoordinates(GEOSContextHandle_t context, const GEOSGeometry* geometry,
                                       NumericVector x, NumericVector y, int offset) {
    const GEOSCoordSequence* seq = GEOSGeom_getCoordSeq_r(context, geometry);
    unsigned int size;
    GEOSCoordSeq_getSize_r(context, seq, &size);

    double xi;
    double yi;

    for (unsigned int i=0; i < size; i++) {
      GEOSCoordSeq_getX_r(context, seq, i, &xi);
      GEOSCoordSeq_getY_r(context, seq, i, &yi);
      x[offset + i] = xi;
      y[offset + i] = yi;
    }

    return size;
  }

  static unsigned int writeCoordinates(GEOSContextHandle_t context, const GEOSGeometry* geometry,
                                       NumericVector x, NumericVector y, IntegerVector part,
                                       int partId, int offset) {
    unsigned int size = writeCoordinates(context, geometry, x, y, offset);
    for(unsigned int i=0; i < size; i++) {
      part[offset + i] = partId;
    }
    return size;
  }

  static unsigned int writeCoordinates(GEOSContextHandle_t context, const GEOSGeometry* geometry,
                                       NumericVector x, NumericVector y, IntegerVector part,
                                       int partId, IntegerVector ring, int ringId, int offset) {
    unsigned int size = writeCoordinates(context, geometry, x, y, offset);
    for(unsigned int i=0; i < size; i++) {
      part[offset + i] = partId;
      ring[offset + i] = ringId;
    }
    return size;
  }
};

#endif
