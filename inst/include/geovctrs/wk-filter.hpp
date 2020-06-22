
#ifndef GEOVCTRS_WK_FILTER_H
#define GEOVCTRS_WK_FILTER_H

#include "wk/geometry-handler.hpp"

class WKFilter: public WKGeometryHandler {
public:
  WKFilter(WKGeometryHandler& handler): handler(handler) {}

  virtual void nextFeatureStart(size_t featureId) {
    this->handler.nextFeatureStart(featureId);
  }

  virtual void nextFeatureEnd(size_t featureId) {
    this->handler.nextFeatureEnd(featureId);
  }

  virtual void nextNull(size_t featureId) {
    this->handler.nextNull(featureId);
  }

  virtual void nextGeometryStart(const WKGeometryMeta& meta, uint32_t partId) {
    this->handler.nextGeometryStart(meta, partId);
  }

  virtual void nextGeometryEnd(const WKGeometryMeta& meta, uint32_t partId) {
    this->handler.nextGeometryEnd(meta, partId);
  }

  virtual void nextLinearRingStart(const WKGeometryMeta& meta, uint32_t size, uint32_t ringId) {
    this->handler.nextLinearRingStart(meta, size, ringId);
  }

  virtual void nextLinearRingEnd(const WKGeometryMeta& meta, uint32_t size, uint32_t ringId) {
    this->handler.nextLinearRingEnd(meta, size, ringId);
  }

  virtual void nextCoordinate(const WKGeometryMeta& meta, const WKCoord& coord, uint32_t coordId) {
    this->handler.nextCoordinate(meta, coord, coordId);
  }

  virtual bool nextError(WKParseException& error, size_t featureId) {
    return this->handler.nextError(error, featureId);
  }

protected:
  WKGeometryHandler& handler;
};

#endif
