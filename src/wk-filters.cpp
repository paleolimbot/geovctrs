
#include <unordered_map>

#include "wk/wkt-writer.hpp"
#include "wk/wkt-reader.hpp"
#include "wk/wkb-writer.hpp"
#include "wk/wkb-reader.hpp"
#include "wk/rcpp-sexp-writer.hpp"
#include "wk/rcpp-sexp-reader.hpp"
#include "geovctrs/wk-filter.hpp"

#include <Rcpp.h>
#include "wk/rcpp-io.hpp"
using namespace Rcpp;

class  WKMetaFilter: public WKFilter {
public:
  WKMetaFilter(WKGeometryHandler& handler): WKFilter(handler) {}

  virtual WKGeometryMeta newGeometryMeta(const WKGeometryMeta& meta, uint32_t partId) = 0;

  virtual void nextFeatureStart(size_t featureId) {
    this->metaReplacement.clear();
    this->handler.nextFeatureStart(featureId);
  }

  virtual void nextGeometryStart(const WKGeometryMeta& meta, uint32_t partId) {
    this->metaReplacement[&meta] = this->newGeometryMeta(meta, partId);
    this->handler.nextGeometryStart(this->metaReplacement[&meta], partId);
  }

  virtual void nextGeometryEnd(const WKGeometryMeta& meta, uint32_t partId) {
    this->handler.nextGeometryEnd(this->metaReplacement[&meta], partId);
  }

  virtual void nextLinearRingStart(const WKGeometryMeta& meta, uint32_t size, uint32_t ringId) {
    this->handler.nextLinearRingStart(this->metaReplacement[&meta], size, ringId);
  }

  virtual void nextLinearRingEnd(const WKGeometryMeta& meta, uint32_t size, uint32_t ringId) {
    this->handler.nextLinearRingEnd(this->metaReplacement[&meta], size, ringId);
  }

  virtual void nextCoordinate(const WKGeometryMeta& meta, const WKCoord& coord, uint32_t coordId) {
    this->handler.nextCoordinate(this->metaReplacement[&meta], coord, coordId);
  }

private:
  // using a hash map to keep track of meta, because it's important to make sure that
  // identical meta objects (at the same address) are used for identical geometry
  // objects (used in s2 and elsewhere to help handle nested collections)
  std::unordered_map<const WKGeometryMeta*, WKGeometryMeta> metaReplacement;
};

class  WKSetSridFilter: public WKMetaFilter {
public:
  WKSetSridFilter(WKGeometryHandler& handler, IntegerVector srid):
    WKMetaFilter(handler), srid(srid), featureSrid(NA_REAL) {}

  virtual void nextFeatureStart(size_t featureId) {
    this->featureSrid = this->srid[featureId];
    WKMetaFilter::nextFeatureStart(featureId);
  }

  WKGeometryMeta newGeometryMeta(const WKGeometryMeta& meta, uint32_t partId) {
    WKGeometryMeta newMeta(meta);
    if (IntegerVector::is_na(this->featureSrid)) {
      newMeta.hasSRID = false;
    } else {
      newMeta.hasSRID = true;
      newMeta.srid = this->featureSrid;
    }

    return newMeta;
  }

private:
  IntegerVector srid;
  int featureSrid;
};


class WKSetZFilter: public WKMetaFilter {
public:
  WKSetZFilter(WKGeometryHandler& handler, NumericVector z):
    WKMetaFilter(handler), z(z), featureZ(NA_REAL) {}

  virtual void nextFeatureStart(size_t featureId) {
    this->featureZ = this->z[featureId];
    WKMetaFilter::nextFeatureStart(featureId);
  }

  WKGeometryMeta newGeometryMeta(const WKGeometryMeta& meta, uint32_t partId) {
    WKGeometryMeta newMeta(meta);
    newMeta.hasZ = !NumericVector::is_na(this->featureZ);
    return newMeta;
  }

  virtual void nextCoordinate(const WKGeometryMeta& meta, const WKCoord& coord, uint32_t coordId) {
    WKCoord newCoord(coord);
    newCoord.z = this->featureZ;
    newCoord.hasZ = !NumericVector::is_na(this->featureZ);
    WKMetaFilter::nextCoordinate(meta, newCoord, coordId);
  }

private:
  NumericVector z;
  double featureZ;
};


void set_srid_base(WKReader& reader, WKWriter& writer, IntegerVector srid) {
  WKSetSridFilter filter(writer, srid);
  reader.setHandler(&filter);

  while (reader.hasNextFeature()) {
    checkUserInterrupt();
    reader.iterateFeature();
  }
}

// [[Rcpp::export]]
CharacterVector cpp_wkt_set_srid(CharacterVector wkt, IntegerVector srid,
                                 int precision = 16, bool trim = true) {
  WKCharacterVectorProvider provider(wkt);
  WKTReader reader(provider);

  WKCharacterVectorExporter exporter(wkt.size());
  WKTWriter writer(exporter);
  exporter.setRoundingPrecision(precision);
  exporter.setTrim(trim);
  set_srid_base(reader, writer, srid);
  return exporter.output;
}

// [[Rcpp::export]]
List cpp_wkb_set_srid(List wkb, IntegerVector srid, int endian) {
  WKRawVectorListProvider provider(wkb);
  WKBReader reader(provider);

  WKRawVectorListExporter exporter(wkb.size());
  WKBWriter writer(exporter);
  writer.setEndian(endian);
  set_srid_base(reader, writer, srid);
  return exporter.output;
}

// [[Rcpp::export]]
List cpp_wksxp_set_srid(List wksxp, IntegerVector srid) {
  WKRcppSEXPProvider provider(wksxp);
  WKRcppSEXPReader reader(provider);

  WKSEXPExporter exporter(wksxp.size());
  WKRcppSEXPWriter writer(exporter);
  set_srid_base(reader, writer, srid);
  return exporter.output;
}


void set_z_base(WKReader& reader, WKWriter& writer, NumericVector z) {
  WKSetZFilter filter(writer, z);
  reader.setHandler(&filter);

  while (reader.hasNextFeature()) {
    checkUserInterrupt();
    reader.iterateFeature();
  }
}

// [[Rcpp::export]]
CharacterVector cpp_wkt_set_z(CharacterVector wkt, NumericVector z,
                              int precision = 16, bool trim = true) {
  WKCharacterVectorProvider provider(wkt);
  WKTReader reader(provider);

  WKCharacterVectorExporter exporter(wkt.size());
  WKTWriter writer(exporter);
  exporter.setRoundingPrecision(precision);
  exporter.setTrim(trim);
  set_z_base(reader, writer, z);
  return exporter.output;
}

// [[Rcpp::export]]
List cpp_wkb_set_z(List wkb, NumericVector z, int endian) {
  WKRawVectorListProvider provider(wkb);
  WKBReader reader(provider);

  WKRawVectorListExporter exporter(wkb.size());
  WKBWriter writer(exporter);
  writer.setEndian(endian);
  set_z_base(reader, writer, z);
  return exporter.output;
}

// [[Rcpp::export]]
List cpp_wksxp_set_z(List wksxp, NumericVector z) {
  WKRcppSEXPProvider provider(wksxp);
  WKRcppSEXPReader reader(provider);

  WKSEXPExporter exporter(wksxp.size());
  WKRcppSEXPWriter writer(exporter);
  set_z_base(reader, writer, z);
  return exporter.output;
}
