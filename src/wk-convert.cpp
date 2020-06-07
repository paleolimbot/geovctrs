
#include <Rcpp.h>
#include "wk/wkt-writer.h"
#include "wk/wkt-reader.h"
#include "wk/wkb-writer.h"
#include "wk/wkb-reader.h"
#include "wk/sexp-writer.h"
#include "wk/sexp-reader.h"
#include "wk/rcpp-io.h"
#include "wk/geometry-debug-handler.h"
#include "geovctrs/wk-xy.h"

using namespace Rcpp;

class RcppFieldsProvider: public GeovctrsFieldsProvider<List> {
public:
  RcppFieldsProvider(List container):
    GeovctrsFieldsProvider<List>(container, Rf_xlength(container[0])) {}
};

class RcppXYReader: public GeovctrsWKXYReader<List, NumericVector> {
public:
  RcppXYReader(RcppFieldsProvider& provider): GeovctrsWKXYReader<List, NumericVector>(provider) {}
};


void cpp_translate_base(WKReader& reader, WKWriter& writer,
                        int includeZ, int includeM, int includeSRID) {
  writer.setIncludeZ(includeZ);
  writer.setIncludeM(includeM);
  writer.setIncludeSRID(includeSRID);

  reader.setHandler(&writer);

  while (reader.hasNextFeature()) {
    checkUserInterrupt();
    reader.iterateFeature();
  }
}

CharacterVector cpp_translate_base_wkt(WKReader& reader,
                                       int includeZ, int includeM, int includeSRID,
                                       int precision, bool trim) {
  WKCharacterVectorExporter exporter(reader.nFeatures());
  exporter.setRoundingPrecision(precision);
  exporter.setTrim(trim);
  WKTWriter writer(exporter);

  cpp_translate_base(reader, writer, includeZ, includeM, includeSRID);

  return exporter.output;
}

List cpp_translate_base_wkb(WKReader& reader,
                            int includeZ, int includeM, int includeSRID,
                            int endian, int bufferSize) {
  WKRawVectorListExporter exporter(reader.nFeatures());
  exporter.setBufferSize(bufferSize);
  WKBWriter writer(exporter);
  writer.setEndian(endian);

  cpp_translate_base(reader, writer, includeZ, includeM, includeSRID);

  return exporter.output;
}

List cpp_translate_base_wksxp(WKReader& reader,
                              int includeZ, int includeM, int includeSRID) {
  WKSEXPExporter exporter(reader.nFeatures());
  WKSEXPWriter writer(exporter);

  cpp_translate_base(reader, writer, includeZ, includeM, includeSRID);

  return exporter.output;
}



// [[Rcpp::export]]
CharacterVector cpp_translate_xy_wkt(List xy, int precision, int trim) {
  RcppFieldsProvider provider(xy);
  RcppXYReader reader(provider);
  return cpp_translate_base_wkt(reader, 0, 0, 0, precision, trim);
}

// [[Rcpp::export]]
List cpp_translate_xy_wkb(List xy, int endian, int bufferSize) {
  RcppFieldsProvider provider(xy);
  RcppXYReader reader(provider);
  return cpp_translate_base_wkb(reader, 0, 0, 0, endian, bufferSize);
}

// [[Rcpp::export]]
List cpp_translate_xy_wksxp(List xy) {
  RcppFieldsProvider provider(xy);
  RcppXYReader reader(provider);
  return cpp_translate_base_wksxp(reader, 0, 0, 0);
}
