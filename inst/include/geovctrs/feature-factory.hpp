
#ifndef GEOVCTRS_FEATURE_FACTORY_HPP
#define GEOVCTRS_FEATURE_FACTORY_HPP

#include <geos_c.h>
#include <Rcpp.h>
using namespace Rcpp;

class GeovctrsFeatureFactory {
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
      stop("GeovctrsFeatureFactory::getFeature(): Unrecognized geometry type");
    }
  }

private:
  static List getPoint(GEOSContextHandle_t context, GEOSGeometry* geometry) {
    int nCoordinates = GEOSGetNumCoordinates_r(context, geometry);
    NumericVector xVec(nCoordinates);
    NumericVector yVec(nCoordinates);
    write_simple_geometry(context, geometry, xVec, yVec, 0);

    return geo_reclass(new_geo_coord(xVec, yVec), "geovctrs_point");
  }

  static List getLinestring(GEOSContextHandle_t context, GEOSGeometry* geometry) {
    int nCoordinates = GEOSGetNumCoordinates_r(context, geometry);
    NumericVector xVec(nCoordinates);
    NumericVector yVec(nCoordinates);
    write_simple_geometry(context, geometry, xVec, yVec, 0);

    return geo_reclass(new_geo_coord(xVec, yVec), "geovctrs_linestring");
  }

  static List getPolygon(GEOSContextHandle_t context, GEOSGeometry* geometry) {
    int nInteriorRings = GEOSGetNumInteriorRings_r(context, geometry);
    int nCoordinates = GEOSGetNumCoordinates_r(context, geometry);

    NumericVector xVec(nCoordinates);
    NumericVector yVec(nCoordinates);
    IntegerVector ringVec(nCoordinates);

    unsigned int offset = 0;
    const GEOSGeometry* exterior = GEOSGetExteriorRing_r(context, geometry);
    offset += write_simple_geometry(context, exterior, xVec, yVec, ringVec, 1, offset);

    for(int i=0; i<nInteriorRings; i++) {
      const GEOSGeometry* ring = GEOSGetInteriorRingN_r(context, geometry, i);
      offset += write_simple_geometry(context, ring, xVec, yVec, ringVec, 2 + i, offset);
    }

    return geo_reclass(new_geo_coord(xVec, yVec, ringVec), "geovctrs_polygon");
  }

  static List getMultipoint(GEOSContextHandle_t context, GEOSGeometry* geometry) {
    int nParts = GEOSGetNumGeometries_r(context, geometry);
    int nCoordinates = GEOSGetNumCoordinates_r(context, geometry);

    NumericVector xVec(nCoordinates);
    NumericVector yVec(nCoordinates);

    unsigned int offset = 0;

    for(int i=0; i<nParts; i++) {
      const GEOSGeometry* part = GEOSGetGeometryN_r(context, geometry, i);
      offset += write_simple_geometry(context, part, xVec, yVec, offset);
    }

    return geo_reclass(new_geo_coord(xVec, yVec), "geovctrs_multipoint");
  }

  static List getMultilinestring(GEOSContextHandle_t context, GEOSGeometry* geometry) {
    int nParts = GEOSGetNumGeometries_r(context, geometry);
    int nCoordinates = GEOSGetNumCoordinates_r(context, geometry);

    NumericVector xVec(nCoordinates);
    NumericVector yVec(nCoordinates);
    IntegerVector partVec(nCoordinates);

    unsigned int offset = 0;

    for(int i=0; i<nParts; i++) {
      const GEOSGeometry* part = GEOSGetGeometryN_r(context, geometry, i);
      offset += write_simple_geometry(context, part, xVec, yVec, partVec, i+1, offset);
    }

    List output = new_geo_coord(xVec, yVec);
    output.push_back(partVec, "part");
    return geo_reclass(output, "geovctrs_multilinestring");
  }

  static List getMultipolygon(GEOSContextHandle_t context, GEOSGeometry* geometry) {
    int nParts = GEOSGetNumGeometries_r(context, geometry);
    int nCoordinates = GEOSGetNumCoordinates_r(context, geometry);

    NumericVector xVec(nCoordinates);
    NumericVector yVec(nCoordinates);
    IntegerVector partVec(nCoordinates);
    IntegerVector ringVec(nCoordinates);

    unsigned int offset = 0;

    for(int i=0; i<nParts; i++) {
      const GEOSGeometry* part = GEOSGetGeometryN_r(context, geometry, i);
      int nInteriorRings = GEOSGetNumInteriorRings_r(context, part);

      const GEOSGeometry* exterior = GEOSGetExteriorRing_r(context, part);
      offset += write_simple_geometry(
        context, exterior,
        xVec, yVec,
        partVec, i + 1,
        ringVec, 1,
        offset
      );

      for(int j=0; j<nInteriorRings; j++) {
        const GEOSGeometry* ring = GEOSGetInteriorRingN_r(context, part, j);
        offset += write_simple_geometry(
          context, ring,
          xVec, yVec,
          partVec, i + 1,
          ringVec, 2 + j,
          offset
        );
      }
    }

    return geo_reclass(new_geo_coord(xVec, yVec, partVec, ringVec), "geovctrs_multipolygon");
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

    List out = List::create(_["feature"] = features, _["srid"] = srid);
    out.attr("class") = CharacterVector::create("geovctrs_collection", "geovctr", "vctrs_rcrd", "vctrs_vctr");
    return out;
  }

  static List geo_reclass(List input, const char* cls) {
    input.attr("class") = CharacterVector::create(cls);
    return input;
  }

  static List new_geo_coord(NumericVector x, NumericVector y) {
    List xy = List::create(_["x"] = x, _["y"] = y);
    xy.attr("class") = CharacterVector::create("geovctrs_xy", "geovctr", "vctrs_rcrd", "vctrs_vctr");

    List output = List::create(_["xy"] = xy);
    return output;
  }

  static List new_geo_coord(NumericVector x, NumericVector y, IntegerVector ring) {
    List output = new_geo_coord(x, y);
    output.push_back(ring, "ring");
    return output;
  }

  static List new_geo_coord(NumericVector x, NumericVector y, IntegerVector part, IntegerVector ring) {
    List output = new_geo_coord(x, y);
    output.push_back(part, "part");
    output.push_back(ring, "ring");
    return output;
  }

  static unsigned int write_simple_geometry(GEOSContextHandle_t context, const GEOSGeometry* geometry,
                                            NumericVector xVec, NumericVector yVec, int offset) {
    const GEOSCoordSequence* seq = GEOSGeom_getCoordSeq_r(context, geometry);
    unsigned int size;
    GEOSCoordSeq_getSize_r(context, seq, &size);

    double x;
    double y;

    for (unsigned int i=0; i<size; i++) {
      GEOSCoordSeq_getX_r(context, seq, i, &x);
      GEOSCoordSeq_getY_r(context, seq, i, &y);
      xVec[offset + i] = x;
      yVec[offset + i] = y;
    }

    return size;
  }

  static unsigned int write_simple_geometry(GEOSContextHandle_t context, const GEOSGeometry* geometry,
                                            NumericVector xVec, NumericVector yVec, IntegerVector part,
                                            int partId, int offset) {
    unsigned int size = write_simple_geometry(context, geometry, xVec, yVec, offset);
    for(unsigned int i=0; i<size; i++) {
      part[offset + i] = partId;
    }
    return size;
  }

  static unsigned int write_simple_geometry(GEOSContextHandle_t context, const GEOSGeometry* geometry,
                                            NumericVector xVec, NumericVector yVec, IntegerVector part,
                                            int partId, IntegerVector ring, int ringId, int offset) {
    unsigned int size = write_simple_geometry(context, geometry, xVec, yVec, offset);
    for(unsigned int i=0; i<size; i++) {
      part[offset + i] = partId;
      ring[offset + i] = ringId;
    }
    return size;
  }
};

#endif
