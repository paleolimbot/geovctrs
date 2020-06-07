
#ifndef GEOVCTRS_WK_SEGMENT_H
#define GEOVCTRS_WK_SEGMENT_H

#include <cmath>
#include "geovctrs/wk-fields.h"

template<typename ContainerType, typename RealVectorType, typename IntVectorType>
class GeovctrsWKSegmentReader: public GeovctrsWKFieldsReader<ContainerType> {
public:
  GeovctrsWKSegmentReader(GeovctrsFieldsProvider<ContainerType>& provider):
    GeovctrsWKFieldsReader<ContainerType>(provider) {}

  void readFeature(size_t featureId) {
    this->handler->nextFeatureStart(featureId);

    double x0 = this->provider.template field<double, RealVectorType>(0);
    double y0 = this->provider.template field<double, RealVectorType>(1);
    double x1 = this->provider.template field<double, RealVectorType>(2);
    double y1 = this->provider.template field<double, RealVectorType>(3);
    uint32_t srid = this->provider.template field<uint32_t, IntVectorType>(4);

    WKGeometryMeta meta(WKGeometryType::LineString, false, false, srid != 0);
    meta.hasSize = true;
    meta.srid = srid;

    // treat NA, NA -> NA NA as an empty line
    if (std::isnan(x0) && std::isnan(y0) && std::isnan(x1) && std::isnan(y1)) {
      meta.size = 0;
      this->handler->nextGeometryStart(meta, WKReader::PART_ID_NONE);
      this->handler->nextGeometryEnd(meta, WKReader::PART_ID_NONE);
    } else {
      meta.size = 2;
      this->handler->nextGeometryStart(meta, WKReader::PART_ID_NONE);
      this->handler->nextCoordinate(meta, WKCoord::xy(x0, y0), 0);
      this->handler->nextCoordinate(meta, WKCoord::xy(x1, y1), 1);
      this->handler->nextGeometryEnd(meta, WKReader::PART_ID_NONE);
    }

    this->handler->nextFeatureEnd(featureId);
  }
};

template<typename ContainerType, typename RealVectorType, typename IntVectorType>
class GeovctrsWKSegmentWriter: public GeovctrsWKFieldsWriter<ContainerType> {
public:
  GeovctrsWKSegmentWriter(GeovctrsFieldsExporter<ContainerType>& exporter):
    GeovctrsWKFieldsWriter<ContainerType>(exporter) {}

  virtual void nextFeatureStart(size_t featureId) {
    GeovctrsWKFieldsWriter<ContainerType>::nextFeatureStart(featureId);
  }

  void nextNull(size_t featureId) {
    this->exporter.template setField<double, RealVectorType>(0, NAN);
    this->exporter.template setField<double, RealVectorType>(1, NAN);
    this->exporter.template setField<double, RealVectorType>(2, NAN);
    this->exporter.template setField<double, RealVectorType>(3, NAN);
    this->exporter.template setField<uint32_t, IntVectorType>(4, NAN);
  }

  void nextGeometryStart(const WKGeometryMeta& meta, uint32_t partId) {
    this->newMeta = this->getNewMeta(meta);

    if (this->newMeta.geometryType != WKGeometryType::LineString) {
      throw std::runtime_error("Can't create segment from a non-linestring");
    }

    if (this->newMeta.size == 0) {
      this->exporter.template setField<double, RealVectorType>(0, NAN);
      this->exporter.template setField<double, RealVectorType>(1, NAN);
      this->exporter.template setField<double, RealVectorType>(2, NAN);
      this->exporter.template setField<double, RealVectorType>(3, NAN);

      if (this->newMeta.hasSRID) {
        this->exporter.template setField<uint32_t, IntVectorType>(4, meta.srid);
      } else {
        this->exporter.template setField<uint32_t, IntVectorType>(4, 0);
      }
    }
  }

  void nextCoordinate(const WKGeometryMeta& meta, const WKCoord& coord, uint32_t coordId) {
    if (coordId == 0) {
      this->exporter.template setField<double, RealVectorType>(0, coord.x);
      this->exporter.template setField<double, RealVectorType>(1, coord.y);
    } else if (coordId == 1) {
      this->exporter.template setField<double, RealVectorType>(2, coord.x);
      this->exporter.template setField<double, RealVectorType>(3, coord.y);

      if (this->newMeta.hasSRID) {
        this->exporter.template setField<uint32_t, IntVectorType>(4, meta.srid);
      } else {
        this->exporter.template setField<uint32_t, IntVectorType>(4, 0);
      }
    } else {
      throw std::runtime_error("Segment geometries can only be created from linestrings of length 0 or 2");
    }
  }
};

#endif
