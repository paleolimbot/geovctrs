
#include <unordered_map>

#include "wk/wkt-writer.hpp"
#include "wk/wkt-reader.hpp"
#include "wk/wkb-writer.hpp"
#include "wk/wkb-reader.hpp"
#include "wk/rcpp-sexp-writer.hpp"
#include "wk/rcpp-sexp-reader.hpp"
#include "wk/filter.hpp"

#include <Rcpp.h>
#include "wk/rcpp-io.hpp"
using namespace Rcpp;

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

  WKRcppSEXPExporter exporter(wksxp.size());
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

  WKRcppSEXPExporter exporter(wksxp.size());
  WKRcppSEXPWriter writer(exporter);
  set_z_base(reader, writer, z);
  return exporter.output;
}
