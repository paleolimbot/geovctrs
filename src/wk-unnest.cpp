
#include <unordered_set>
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

class WKUnnestedCounter: public WKGeometryHandler {
public:
  size_t nNewFeatures;

  WKUnnestedCounter(bool keepEmpty): nNewFeatures(0), keepEmpty(keepEmpty) {}

  virtual bool shouldUnnest(const WKGeometryMeta& meta) {
    if (this->keepEmpty && meta.size == 0) {
      return false;
    } else {
      return meta.geometryType >= WKGeometryType::MultiPoint;
    }
  }

  void nextNull(size_t featureId) {
    this->nNewFeatures++;
  }

  void nextGeometryStart(const WKGeometryMeta& meta, uint32_t partId) {
    if (!this->shouldUnnest(meta)) {
      this->nNewFeatures++;
    }
  }

private:
  bool keepEmpty;
};

class WKUnnester: public WKMetaFilter {
public:
  WKUnnester(WKGeometryHandler& handler, bool keepEmpty):
    WKMetaFilter(handler), newFeatureId(0), topLevelMetaId(0), keepEmpty(keepEmpty) {}

  bool shouldUnnest(const WKGeometryMeta& meta) {
    if (this->keepEmpty && meta.size == 0) {
      return false;
    } else {
      return meta.geometryType >= WKGeometryType::MultiPoint;
    }
  }

  WKGeometryMeta newGeometryMeta(const WKGeometryMeta& meta, uint32_t partId) {
    if (this->unnestDepth > 0) {
      WKGeometryMeta newMeta(meta);
      newMeta.hasSRID = this->newHasSrid;
      newMeta.srid = this->newSrid;
      return newMeta;
    } else {
      return meta;
    }
  }

  void nextFeatureStart(size_t featureId) {
    this->topLevelMetaId = 0;
    this->unnestDepth = 0;
  }

  void nextFeatureEnd(size_t featureId) {
    // specifically do nothing
  }

  void nextNull(size_t featureId) {
    this->handler.nextFeatureStart(this->newFeatureId);
    this->handler.nextNull(this->newFeatureId);
    this->handler.nextFeatureEnd(this->newFeatureId);
    this->newFeatureId++;
  }

  void nextGeometryStart(const WKGeometryMeta& meta, uint32_t partId) {
    if (this->shouldUnnest(meta)) {
      if (this->unnestDepth == 0) {
        this->newHasSrid = meta.hasSRID;
        this->newSrid = meta.srid;
      }
      this->unnestDepth++;
      return;
    }

    if (this->topLevelMetaId == 0) {
      this->topLevelMetaId = meta.id();
      partId = WKReader::PART_ID_NONE;
      this->handler.nextFeatureStart(this->newFeatureId);
    }

    // takes care of meta updating
    WKMetaFilter::nextGeometryStart(meta, partId);
  }

  void nextGeometryEnd(const WKGeometryMeta& meta, uint32_t partId) {
    if (this->shouldUnnest(meta)) {
      this->unnestDepth--;
      return;
    }

    if (meta.id() == this->topLevelMetaId) {
      this->handler.nextGeometryEnd(this->metaReplacement[meta.id()], partId);
      this->handler.nextFeatureEnd(this->newFeatureId);
      this->newFeatureId++;
      this->topLevelMetaId = 0;
    } else {
      this->handler.nextGeometryEnd(this->metaReplacement[meta.id()], WKReader::PART_ID_NONE);
    }

    // manually handled metaReplacement (no need for WKMetaFilter::nextGeometryEnd())
  }

private:
  size_t newFeatureId;
  uintptr_t topLevelMetaId;
  bool keepEmpty;
  int unnestDepth;
  bool newHasSrid;
  uint32_t newSrid;
};

size_t unnest_all_count(WKReader& reader, bool keepEmpty) {
  WKUnnestedCounter counter(keepEmpty);
  reader.setHandler(&counter);

  while (reader.hasNextFeature()) {
    checkUserInterrupt();
    reader.iterateFeature();
  }

  return counter.nNewFeatures;
}

void unnest_all_do(WKReader& reader, WKWriter& writer, bool keepEmpty) {
  WKUnnester unnester(writer, keepEmpty);
  reader.setHandler(&unnester);

  reader.reset();
  while (reader.hasNextFeature()) {
    checkUserInterrupt();
    reader.iterateFeature();
  }
}

//  [[Rcpp::export]]
CharacterVector cpp_wkt_unnest_all(CharacterVector wkt, bool keepEmpty) {
  WKCharacterVectorProvider provider(wkt);
  WKTReader reader(provider);

  size_t nGeometries = unnest_all_count(reader, keepEmpty);
  WKCharacterVectorExporter exporter(nGeometries);
  exporter.setRoundingPrecision(16);
  exporter.setTrim(true);
  WKTWriter writer(exporter);

  unnest_all_do(reader, writer, keepEmpty);
  return exporter.output;
}

//  [[Rcpp::export]]
List cpp_wkb_unnest_all(List wkb, bool keepEmpty, int endian) {
  WKRawVectorListProvider provider(wkb);
  WKBReader reader(provider);

  size_t nGeometries = unnest_all_count(reader, keepEmpty);
  WKRawVectorListExporter exporter(nGeometries);
  WKBWriter writer(exporter);
  writer.setEndian(endian);

  unnest_all_do(reader, writer, keepEmpty);
  return exporter.output;
}

//  [[Rcpp::export]]
List cpp_wksxp_unnest_all(List wkb, bool keepEmpty) {
  WKRcppSEXPProvider provider(wkb);
  WKRcppSEXPReader reader(provider);

  size_t nGeometries = unnest_all_count(reader, keepEmpty);
  WKRcppSEXPExporter exporter(nGeometries);
  WKRcppSEXPWriter writer(exporter);

  unnest_all_do(reader, writer, keepEmpty);
  return exporter.output;
}
