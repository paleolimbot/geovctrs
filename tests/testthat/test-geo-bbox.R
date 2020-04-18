
test_that("envelope works", {
  # character (works because of as_geovctr())
  expect_identical(
    geo_envelope("LINESTRING (0 2, 10 11)"),
    geo_rect(0, 2, 10, 11)
  )

  expect_identical(
    geo_envelope(geo_wkt("LINESTRING (0 2, 10 11)")),
    geo_rect(0, 2, 10, 11)
  )

  expect_identical(
    geo_envelope(geo_linestring(c(geo_xy(0, 2), geo_xy(10, 11)))),
    geo_rect(0, 2, 10, 11)
  )
})

test_that("envelope works with nested collections", {
  expect_identical(
    geo_envelope(geo_wkt("GEOMETRYCOLLECTION (GEOMETRYCOLLECTION EMPTY)")),
    geo_rect(Inf, Inf, -Inf, -Inf)
  )

  expect_identical(
    geo_envelope(geo_wkt("GEOMETRYCOLLECTION (LINESTRING (0 0, 1 2, 6 4))")),
    geo_rect(0, 0, 6, 4)
  )

  expect_identical(
    geo_envelope(geo_wkt("GEOMETRYCOLLECTION (LINESTRING (nan 0, 1 2, 6 4))"), na.rm = FALSE),
    geo_rect(NA, 0, NA, 4)
  )

  expect_identical(
    geo_envelope(geo_wkt("GEOMETRYCOLLECTION (LINESTRING (nan 0, 1 2, 6 4))"), na.rm = TRUE),
    geo_rect(1, 0, 6, 4)
  )

  expect_identical(
    geo_envelope(geo_wkt("GEOMETRYCOLLECTION (LINESTRING (nan 0, 1 2, 6 4))"), finite = TRUE),
    geo_rect(1, 0, 6, 4)
  )
})

test_that("envelope works with corner cases", {
  expect_identical(geo_envelope(geo_xy()), geo_rect())
  expect_identical(geo_envelope(geo_xy(NA, NA), na.rm = FALSE), geo_rect(NA, NA, NA, NA))
  expect_identical(geo_envelope(geo_xy(NA, NA), na.rm = TRUE), geo_rect(Inf, Inf, -Inf, -Inf))

  expect_identical(geo_envelope(geo_rect()), geo_rect())
  expect_identical(
    geo_envelope(geo_rect(NA, NA, NA, NA)),
    geo_rect(Inf, Inf, -Inf, -Inf)
  )
  expect_identical(
    geo_envelope(geo_rect(NA, NA, NA, NA), na.rm = TRUE),
    geo_rect(Inf, Inf, -Inf, -Inf)
  )

  expect_identical(geo_envelope(geo_segment()), geo_rect())
  expect_identical(
    geo_envelope(geo_segment(geo_xy(NA, NA), geo_xy(NA, NA))),
    geo_rect(Inf, Inf, -Inf, -Inf)
  )
  expect_identical(
    geo_envelope(geo_segment(geo_xy(NA, NA), geo_xy(NA, NA)), na.rm = TRUE),
    geo_rect(Inf, Inf, -Inf, -Inf)
  )

  expect_equal(
    geo_envelope("POINT EMPTY", na.rm = FALSE),
    geo_rect(Inf, Inf, -Inf, -Inf)
  )

  expect_identical(
    geo_envelope("POINT EMPTY", na.rm = TRUE),
    geo_rect(Inf, Inf, -Inf, -Inf)
  )

  expect_identical(
    geo_envelope("MULTIPOINT EMPTY", na.rm = FALSE),
    geo_rect(Inf, Inf, -Inf, -Inf)
  )

  expect_identical(
    geo_envelope("MULTIPOINT EMPTY", na.rm = TRUE),
    geo_rect(Inf, Inf, -Inf, -Inf)
  )

  # GEOS reads these as identical to empty points
  # so they have slightly different bounding box behaviour
  # than XYs
  expect_identical(
    geo_envelope("POINT (nan nan)", na.rm = FALSE),
    geo_rect(Inf, Inf, -Inf, -Inf)
  )
  expect_identical(
    geo_envelope("POINT (nan nan)", na.rm = TRUE),
    geo_rect(Inf, Inf, -Inf, -Inf)
  )

  # even multipoints are funky
  expect_identical(
    geo_envelope("MULTIPOINT (nan nan)", na.rm = FALSE),
    geo_rect(Inf, Inf, -Inf, -Inf)
  )
  expect_identical(
    geo_envelope("MULTIPOINT (nan nan)", na.rm = TRUE),
    geo_rect(Inf, Inf, -Inf, -Inf)
  )

  expect_equal(
    geo_envelope("LINESTRING (nan 1, 5 6)", na.rm = FALSE),
    geo_rect(NA, 1, NA, 6)
  )

  expect_equal(
    geo_envelope("LINESTRING (nan 1, 5 6)", na.rm = TRUE),
    geo_rect(5, 1, 5, 6)
  )

  expect_equal(
    geo_envelope("LINESTRING (nan 1, 5 6)", finite = TRUE),
    geo_rect(5, 1, 5, 6)
  )

  expect_equal(
    geo_envelope("LINESTRING (nan inf, 5 6)", finite = TRUE),
    geo_rect(5, 6, 5, 6)
  )

  expect_equal(
    geo_envelope("LINESTRING (nan 1, nan 6)", na.rm = FALSE),
    geo_rect(NA, 1, NA, 6)
  )

  expect_equal(
    geo_envelope("LINESTRING (nan 1, nan 6)", na.rm = TRUE),
    geo_rect(Inf, 1, -Inf, 6)
  )

  expect_equal(
    geo_envelope("LINESTRING (nan 1, nan 6)", finite = TRUE),
    geo_rect(Inf, 1, -Inf, 6)
  )
})

test_that("bbox works", {
  # character (works because of as_geovctr())
  expect_identical(
    geo_bbox("LINESTRING (0 2, 10 11)"),
    geo_rect(0, 2, 10, 11)
  )

  expect_identical(
    geo_bbox(geo_xy(1:5, 2:6)),
    geo_rect(1, 2, 5, 6)
  )

  expect_identical(
    geo_bbox(geo_wkt("LINESTRING (0 2, 10 11)")),
    geo_rect(0, 2, 10, 11)
  )

  expect_identical(
    geo_bbox(geo_linestring(c(geo_xy(0, 2), geo_xy(10, 11)))),
    geo_rect(0, 2, 10, 11)
  )

  expect_identical(
    geo_bbox(geo_segment(geo_xy(0, 2), geo_xy(10, 11))),
    geo_rect(0, 2, 10, 11)
  )

  expect_identical(
    geo_bbox(geo_rect(0, 2, 10, 11)),
    geo_rect(0, 2, 10, 11)
  )
})

test_that("bbox works with corner cases", {
  # empty geometries always have an inf bbox, regardless of na.rm!

  expect_identical(geo_bbox(geo_xy()), geo_rect(Inf, Inf, -Inf, -Inf, srid = NA))
  expect_identical(geo_bbox(geo_xy(NA, NA), na.rm = FALSE), geo_rect(Inf, Inf, -Inf, -Inf))
  expect_identical(geo_bbox(geo_xy(NA, NA), na.rm = TRUE), geo_rect(Inf, Inf, -Inf, -Inf))

  expect_identical(geo_bbox(geo_rect()), geo_rect(Inf, Inf, -Inf, -Inf, srid = NA))
  expect_identical(
    geo_bbox(geo_rect(NA, NA, NA, NA)),
    geo_rect(Inf, Inf, -Inf, -Inf)
  )
  expect_identical(
    geo_bbox(geo_rect(NA, NA, NA, NA), na.rm = TRUE),
    geo_rect(Inf, Inf, -Inf, -Inf)
  )

  expect_identical(geo_bbox(geo_segment()), geo_rect(Inf, Inf, -Inf, -Inf, srid = NA))
  expect_identical(
    geo_bbox(geo_segment(geo_xy(NA, NA), geo_xy(NA, NA))),
    geo_rect(Inf, Inf, -Inf, -Inf)
  )
  expect_identical(
    geo_bbox(geo_segment(geo_xy(NA, NA), geo_xy(NA, NA)), na.rm = TRUE),
    geo_rect(Inf, Inf, -Inf, -Inf)
  )
})

test_that("misssing values have missing envelopes", {
  expect_identical(geo_envelope(NA_wkt_), NA_rect_)
  expect_identical(geo_envelope(NA_wkb_), NA_rect_)
  expect_identical(geo_envelope(NA_collection_), NA_rect_)
  # tricky corner case...POINT EMPTY is the same in GEOS as POINT (nan nan)
  # for safety, don't drop SRID
  expect_identical(geo_envelope(NA_xy_), geo_rect(NA, NA, NA, NA, srid = 0))
  expect_identical(geo_envelope(NA_segment_), NA_rect_)
  expect_identical(geo_envelope(NA_rect_), NA_rect_)

  expect_identical(geo_envelope(NA_wkt_, na.rm = TRUE), geo_rect(Inf, Inf, -Inf, -Inf, srid = NA))
  expect_identical(geo_envelope(NA_wkb_, na.rm = TRUE), geo_rect(Inf, Inf, -Inf, -Inf, srid = NA))
  expect_identical(geo_envelope(NA_collection_, na.rm = TRUE), geo_rect(Inf, Inf, -Inf, -Inf, srid = NA))
  # tricky corner case...POINT EMPTY is the same in GEOS as POINT (nan nan)
  # for safety, don't drop SRID
  expect_identical(geo_envelope(NA_xy_, na.rm = TRUE), geo_rect(Inf, Inf, -Inf, -Inf, srid = 0))
  expect_identical(geo_envelope(NA_segment_, na.rm = TRUE), geo_rect(Inf, Inf, -Inf, -Inf, srid = NA))
  expect_identical(geo_envelope(NA_rect_, na.rm = TRUE), geo_rect(Inf, Inf, -Inf, -Inf, srid = NA))
})

test_that("misssing values have the correct bounding boxes", {
  # missing values are also empty, so they have the empty bounding box

  expect_identical(geo_bbox(NA_wkt_), geo_rect(Inf, Inf, -Inf, -Inf, srid = NA))
  expect_identical(geo_bbox(NA_wkb_), geo_rect(Inf, Inf, -Inf, -Inf, srid = NA))
  expect_identical(geo_bbox(NA_collection_), geo_rect(Inf, Inf, -Inf, -Inf, srid = NA))
  # tricky corner case...POINT EMPTY is the same in GEOS as POINT (nan nan)
  # for safety, don't drop SRID
  expect_identical(geo_bbox(NA_xy_), geo_rect(Inf, Inf, -Inf, -Inf, srid = 0))
  expect_identical(geo_bbox(NA_segment_), geo_rect(Inf, Inf, -Inf, -Inf, srid = NA))
  expect_identical(geo_bbox(NA_rect_), geo_rect(Inf, Inf, -Inf, -Inf, srid = NA))

  expect_identical(geo_bbox(NA_wkt_, na.rm = TRUE), geo_rect(Inf, Inf, -Inf, -Inf, srid = NA))
  expect_identical(geo_bbox(NA_wkb_, na.rm = TRUE), geo_rect(Inf, Inf, -Inf, -Inf, srid = NA))
  expect_identical(geo_bbox(NA_collection_, na.rm = TRUE), geo_rect(Inf, Inf, -Inf, -Inf, srid = NA))
  # tricky corner case...POINT EMPTY is the same in GEOS as POINT (nan nan)
  # for safety, don't drop SRID
  expect_identical(geo_bbox(NA_xy_, na.rm = TRUE), geo_rect(Inf, Inf, -Inf, -Inf, srid = 0))
  expect_identical(geo_bbox(NA_segment_, na.rm = TRUE), geo_rect(Inf, Inf, -Inf, -Inf, srid = NA))
  expect_identical(geo_bbox(NA_rect_, na.rm = TRUE), geo_rect(Inf, Inf, -Inf, -Inf, srid = NA))
})

test_that("lim functions work", {
  expect_identical(geo_x_range("POINT (30 10)"), geo_lim(30, 30))
  expect_identical(geo_y_range("POINT (30 10)"), geo_lim(10, 10))
  expect_identical(geo_z_range("POINT Z (30 10 20)"), geo_lim(20, 20))
  expect_identical(geo_z_range(c("POINT Z (30 10 20)", "POINT Z (30 10 80)")), geo_lim(20, 80))
  expect_identical(geo_z_envelope(c("POINT Z (30 10 20)", "POINT Z (30 10 80)")), geo_lim(c(20, 80), c(20, 80)))
})
