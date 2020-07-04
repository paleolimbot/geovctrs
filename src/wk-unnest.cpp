
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

class WKUnnester: public WKMetaFilter {
public:
  WKUnnester(WKGeometryHandler& handler, bool keepEmpty, bool keepMulti, int maxUnnestDepth = INT_MAX):
    WKMetaFilter(handler), newFeatureId(0), topLevelMetaId(0), keepEmpty(keepEmpty),
    maxUnnestDepth(maxUnnestDepth), unnestDepth(0) {

    if (keepMulti) {
      this->minUnnestType = WKGeometryType::GeometryCollection;
    } else {
      this->minUnnestType = WKGeometryType::MultiPoint;
    }
  }

  bool shouldUnnest(const WKGeometryMeta& meta, int unnestDepth) {
    if (unnestDepth >= this->maxUnnestDepth) {
      return false;
    } else if (this->keepEmpty && (meta.size == 0)) {
      return false;
    } else if (meta.geometryType >= this->minUnnestType) {
      return true;
    } else {
      return false;
    }
  }

  // at the start, the unnestDepth is correct
  bool shouldUnnestStart(const WKGeometryMeta& meta) {
    if (this->shouldUnnest(meta, this->unnestDepth)) {
      this->isUnnested.insert(meta.id());
      return true;
    } else {
      return false;
    }
  }

  // at the end, just check if this meta was unnested
  bool shouldUnnestEnd(const WKGeometryMeta& meta) {
    return this->isUnnested.count(meta.id()) == 1;
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
    this->isUnnested.clear();
    this->topLevelMetaId = 0;
    this->unnestDepth = 0;
  }

  void nextFeatureEnd(size_t featureId) {

  }

  void nextNull(size_t featureId) {
    this->handler.nextFeatureStart(this->newFeatureId);
    this->handler.nextNull(this->newFeatureId);
    this->handler.nextFeatureEnd(this->newFeatureId);
    this->newFeatureId++;
  }

  void nextGeometryStart(const WKGeometryMeta& meta, uint32_t partId) {
    if (this->shouldUnnestStart(meta)) {
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

  void nextCoordinate(const WKGeometryMeta& meta, const WKCoord& coord, uint32_t coordId) {
    this->handler.nextCoordinate(this->metaReplacement[meta.id()], coord, coordId);
  }

  void nextGeometryEnd(const WKGeometryMeta& meta, uint32_t partId) {
    if (this->shouldUnnestEnd(meta)) {
      this->isUnnested.erase(meta.id());
      this->unnestDepth--;
      return;
    }

    if (meta.id() == this->topLevelMetaId) {
      this->handler.nextGeometryEnd(this->metaReplacement[meta.id()], WKReader::PART_ID_NONE);
      this->handler.nextFeatureEnd(this->newFeatureId);
      this->newFeatureId++;
      this->topLevelMetaId = 0;
    } else {
      this->handler.nextGeometryEnd(this->metaReplacement[meta.id()], partId);
    }

    // manually handled metaReplacement (no need for WKMetaFilter::nextGeometryEnd())
  }

private:
  size_t newFeatureId;
  uintptr_t topLevelMetaId;
  bool keepEmpty;
  int minUnnestType;
  int maxUnnestDepth;
  std::unordered_set<uintptr_t> isUnnested;
  int unnestDepth;
  bool newHasSrid;
  uint32_t newSrid;
};

class WKUnnestedFeatureCounter: public WKGeometryHandler {
public:
  size_t nNewFeatures;
  WKUnnestedFeatureCounter(): nNewFeatures(0) {}

  size_t reset() {
    size_t out = this->nNewFeatures;
    this->nNewFeatures = 0;
    return out;
  }

  void nextFeatureStart(size_t featureId) {
    this->nNewFeatures++;
  }
};

IntegerVector unnest_count(WKReader& reader, bool keepEmpty, bool keepMulti, int maxUnnestDepth) {
  WKUnnestedFeatureCounter counter;
  WKUnnester unnester(counter, keepEmpty, keepMulti, maxUnnestDepth);
  reader.setHandler(&unnester);

  R_xlen_t featureId = 0;
  IntegerVector lengths(reader.nFeatures());

  while (reader.hasNextFeature()) {
    checkUserInterrupt();
    reader.iterateFeature();
    lengths[featureId] = counter.reset();
    featureId++;
  }

  return lengths;
}

void unnest_do(WKReader& reader, WKWriter& writer, bool keepEmpty, bool keepMulti, int maxUnnestDepth) {
  WKUnnester unnester(writer, keepEmpty, keepMulti, maxUnnestDepth);
  reader.setHandler(&unnester);

  reader.reset();
  while (reader.hasNextFeature()) {
    checkUserInterrupt();
    reader.iterateFeature();
  }
}


// [[Rcpp::export]]
CharacterVector cpp_wkt_unnest(CharacterVector wkt, bool keepEmpty, bool keepMulti, int maxUnnestDepth) {
  WKCharacterVectorProvider provider(wkt);
  WKTReader reader(provider);

  IntegerVector lengths = unnest_count(reader, keepEmpty, keepMulti, maxUnnestDepth);
  size_t nGeometries = sum(lengths);
  WKCharacterVectorExporter exporter(nGeometries);
  exporter.setRoundingPrecision(16);
  exporter.setTrim(true);
  WKTWriter writer(exporter);

  unnest_do(reader, writer, keepEmpty, keepMulti, maxUnnestDepth);
  exporter.output.attr("lengths") = lengths;
  return exporter.output;
}

//  [[Rcpp::export]]
List cpp_wkb_unnest(List wkb, bool keepEmpty, bool keepMulti, int maxUnnestDepth, int endian) {
  WKRawVectorListProvider provider(wkb);
  WKBReader reader(provider);

  IntegerVector lengths = unnest_count(reader, keepEmpty, keepMulti, maxUnnestDepth);
  size_t nGeometries = sum(lengths);
  WKRawVectorListExporter exporter(nGeometries);
  WKBWriter writer(exporter);
  writer.setEndian(endian);

  unnest_do(reader, writer, keepEmpty, keepMulti, maxUnnestDepth);
  exporter.output.attr("lengths") = lengths;
  return exporter.output;
}

//  [[Rcpp::export]]
List cpp_wksxp_unnest(List wksxp, bool keepEmpty, bool keepMulti, int maxUnnestDepth) {
  WKRcppSEXPProvider provider(wksxp);
  WKRcppSEXPReader reader(provider);

  IntegerVector lengths = unnest_count(reader, keepEmpty, keepMulti, maxUnnestDepth);
  size_t nGeometries = sum(lengths);
  WKRcppSEXPExporter exporter(nGeometries);
  WKRcppSEXPWriter writer(exporter);

  unnest_do(reader, writer, keepEmpty, keepMulti, maxUnnestDepth);
  exporter.output.attr("lengths") = lengths;
  return exporter.output;
}
