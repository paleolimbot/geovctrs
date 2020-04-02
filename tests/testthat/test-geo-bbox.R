
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

test_that("envelope works with corner cases", {
  expect_identical(geo_envelope(geo_xy()), geo_rect())
  expect_identical(geo_envelope(geo_xy(NA, NA), na.rm = FALSE), geo_rect(NA, NA, NA, NA))
  expect_identical(geo_envelope(geo_xy(NA, NA), na.rm = TRUE), geo_rect(Inf, Inf, -Inf, -Inf))

  expect_identical(geo_envelope(geo_rect()), geo_rect())
  expect_identical(
    geo_envelope(geo_rect(NA, NA, NA, NA)),
    geo_rect(NA, NA, NA, NA)
  )
  expect_identical(
    geo_envelope(geo_rect(NA, NA, NA, NA), na.rm = TRUE),
    geo_rect(Inf, Inf, -Inf, -Inf)
  )

  expect_identical(geo_envelope(geo_segment()), geo_rect())
  expect_identical(
    geo_envelope(geo_segment(geo_xy(NA, NA), geo_xy(NA, NA))),
    geo_rect(NA, NA, NA, NA)
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
    geo_envelope("LINESTRING (nan 1, nan 6)", na.rm = FALSE),
    geo_rect(NA, 1, NA, 6)
  )

  expect_equal(
    geo_envelope("LINESTRING (nan 1, nan 6)", na.rm = TRUE),
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
  expect_identical(geo_bbox(geo_xy()), geo_rect(Inf, Inf, -Inf, -Inf, srid = NA))
  expect_identical(geo_bbox(geo_xy(NA, NA), na.rm = FALSE), geo_rect(NA, NA, NA, NA))
  expect_identical(geo_bbox(geo_xy(NA, NA), na.rm = TRUE), geo_rect(Inf, Inf, -Inf, -Inf))

  expect_identical(geo_bbox(geo_rect()), geo_rect(Inf, Inf, -Inf, -Inf, srid = NA))
  expect_identical(
    geo_bbox(geo_rect(NA, NA, NA, NA)),
    geo_rect(NA, NA, NA, NA)
  )
  expect_identical(
    geo_bbox(geo_rect(NA, NA, NA, NA), na.rm = TRUE),
    geo_rect(Inf, Inf, -Inf, -Inf)
  )

  expect_identical(geo_bbox(geo_segment()), geo_rect(Inf, Inf, -Inf, -Inf, srid = NA))
  expect_identical(
    geo_bbox(geo_segment(geo_xy(NA, NA), geo_xy(NA, NA))),
    geo_rect(NA, NA, NA, NA)
  )
  expect_identical(
    geo_bbox(geo_segment(geo_xy(NA, NA), geo_xy(NA, NA)), na.rm = TRUE),
    geo_rect(Inf, Inf, -Inf, -Inf)
  )
})
