
#include "wk/wkt-writer.h"
#include "wk/wkt-reader.h"
#include "wk/wkb-writer.h"
#include "wk/wkb-reader.h"
#include "wk/sexp-writer.h"
#include "wk/sexp-reader.h"
#include "geovctrs/wk-xy.h"
#include "geovctrs/wk-xyz.h"
#include "geovctrs/wk-segment.h"
#include "wk/geometry-debug-handler.h"

#include <Rcpp.h>
#include "wk/rcpp-io.h"
using namespace Rcpp;

class RcppFieldsProvider: public GeovctrsFieldsProvider<List> {
public:
  RcppFieldsProvider(List container):
    GeovctrsFieldsProvider<List>(container, Rf_xlength(container[0])) {}
};

class RcppFieldsExporter: public GeovctrsFieldsExporter<List> {
public:
  RcppFieldsExporter(List container):
    GeovctrsFieldsExporter<List>(container, Rf_xlength(container[0])) {}
};

class RcppXYReader: public GeovctrsWKXYReader<List, NumericVector> {
public:
  RcppXYReader(RcppFieldsProvider& provider):
    GeovctrsWKXYReader<List, NumericVector>(provider) {}
};

class RcppXYWriter: public GeovctrsWKXYWriter<List, NumericVector> {
public:
  RcppXYWriter(RcppFieldsExporter& exporter):
    GeovctrsWKXYWriter<List, NumericVector>(exporter) {}
};

class RcppXYZReader: public GeovctrsWKXYZReader<List, NumericVector> {
public:
  RcppXYZReader(RcppFieldsProvider& provider):
    GeovctrsWKXYZReader<List, NumericVector>(provider) {}
};

class RcppXYZWriter: public GeovctrsWKXYZWriter<List, NumericVector> {
public:
  RcppXYZWriter(RcppFieldsExporter& exporter):
    GeovctrsWKXYZWriter<List, NumericVector>(exporter) {}
};

class RcppSegmentReader: public GeovctrsWKSegmentReader<List, NumericVector, IntegerVector> {
public:
  RcppSegmentReader(RcppFieldsProvider& provider):
    GeovctrsWKSegmentReader<List, NumericVector, IntegerVector>(provider) {}
};

class RcppSegmentWriter: public GeovctrsWKSegmentWriter<List, NumericVector, IntegerVector> {
public:
  RcppSegmentWriter(RcppFieldsExporter& exporter):
    GeovctrsWKSegmentWriter<List, NumericVector, IntegerVector>(exporter) {}
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

List cpp_translate_base_xy(WKReader& reader) {
  List xy = List::create(
    _["x"] = NumericVector(reader.nFeatures()),
    _["y"] = NumericVector(reader.nFeatures())
  );

  RcppFieldsExporter exporter(xy);
  RcppXYWriter writer(exporter);
  cpp_translate_base(reader, writer, false, false, false);
  return xy;
}

List cpp_translate_base_xyz(WKReader& reader) {
  List xyz = List::create(
    _["x"] = NumericVector(reader.nFeatures()),
    _["y"] = NumericVector(reader.nFeatures()),
    _["z"] = NumericVector(reader.nFeatures())
  );

  RcppFieldsExporter exporter(xyz);
  RcppXYZWriter writer(exporter);
  cpp_translate_base(reader, writer, 2, false, false);
  return xyz;
}

List cpp_translate_base_segment(WKReader& reader, int includeSRID) {
  List segment = List::create(
    _["x0"] = NumericVector(reader.nFeatures()),
    _["y0"] = NumericVector(reader.nFeatures()),
    _["x1"] = NumericVector(reader.nFeatures()),
    _["y1"] = NumericVector(reader.nFeatures())
  );

  RcppFieldsExporter exporter(segment);
  RcppSegmentWriter writer(exporter);
  cpp_translate_base(reader, writer, false, false, includeSRID);
  return segment;
}

// -------- XY -----------

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

// [[Rcpp::export]]
List cpp_translate_wkt_xy(CharacterVector wkt) {
  WKCharacterVectorProvider provider(wkt);
  WKTReader reader(provider);
  return cpp_translate_base_xy(reader);
}

// [[Rcpp::export]]
List cpp_translate_wkb_xy(List wkb) {
  WKRawVectorListProvider provider(wkb);
  WKBReader reader(provider);
  return cpp_translate_base_xy(reader);
}

// [[Rcpp::export]]
List cpp_translate_wksxp_xy(List wksxp) {
  WKSEXPProvider provider(wksxp);
  WKSEXPReader reader(provider);
  return cpp_translate_base_xy(reader);
}

// -------- XYZ -----------

// [[Rcpp::export]]
CharacterVector cpp_translate_xyz_wkt(List xyz, int precision, int trim) {
  RcppFieldsProvider provider(xyz);
  RcppXYZReader reader(provider);
  return cpp_translate_base_wkt(reader, true, 0, 0, precision, trim);
}

// [[Rcpp::export]]
List cpp_translate_xyz_wkb(List xyz, int endian, int bufferSize) {
  RcppFieldsProvider provider(xyz);
  RcppXYZReader reader(provider);
  return cpp_translate_base_wkb(reader, true, 0, 0, endian, bufferSize);
}

// [[Rcpp::export]]
List cpp_translate_xyz_wksxp(List xyz) {
  RcppFieldsProvider provider(xyz);
  RcppXYZReader reader(provider);
  return cpp_translate_base_wksxp(reader, true, 0, 0);
}

// [[Rcpp::export]]
List cpp_translate_wkt_xyz(CharacterVector wkt) {
  WKCharacterVectorProvider provider(wkt);
  WKTReader reader(provider);
  return cpp_translate_base_xyz(reader);
}

// [[Rcpp::export]]
List cpp_translate_wkb_xyz(List wkb) {
  WKRawVectorListProvider provider(wkb);
  WKBReader reader(provider);
  return cpp_translate_base_xyz(reader);
}

// [[Rcpp::export]]
List cpp_translate_wksxp_xyz(List wksxp) {
  WKSEXPProvider provider(wksxp);
  WKSEXPReader reader(provider);
  return cpp_translate_base_xyz(reader);
}

// -------- segment -----------

// [[Rcpp::export]]
CharacterVector cpp_translate_segment_wkt(List segment, int precision, int trim) {
  RcppFieldsProvider provider(segment);
  RcppSegmentReader reader(provider);
  return cpp_translate_base_wkt(reader, 0, 0, 0, precision, trim);
}

// [[Rcpp::export]]
List cpp_translate_segment_wkb(List segment, int endian, int bufferSize) {
  RcppFieldsProvider provider(segment);
  RcppSegmentReader reader(provider);
  return cpp_translate_base_wkb(reader, 0, 0, 0, endian, bufferSize);
}

// [[Rcpp::export]]
List cpp_translate_segment_wksxp(List segment) {
  RcppFieldsProvider provider(segment);
  RcppSegmentReader reader(provider);
  return cpp_translate_base_wksxp(reader, 0, 0, 0);
}

// [[Rcpp::export]]
List cpp_translate_wkt_segment(CharacterVector wkt, int includeSRID) {
  WKCharacterVectorProvider provider(wkt);
  WKTReader reader(provider);
  return cpp_translate_base_segment(reader, includeSRID);
}

// [[Rcpp::export]]
List cpp_translate_wkb_segment(List wkb, int includeSRID) {
  WKRawVectorListProvider provider(wkb);
  WKBReader reader(provider);
  return cpp_translate_base_segment(reader, includeSRID);
}

// [[Rcpp::export]]
List cpp_translate_wksxp_segment(List wksxp, int includeSRID) {
  WKSEXPProvider provider(wksxp);
  WKSEXPReader reader(provider);
  return cpp_translate_base_segment(reader, includeSRID);
}
