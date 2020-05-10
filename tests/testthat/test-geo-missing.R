
test_that("built-in missing values are missing", {
  expect_true(is.na(NA_wkt_))
  expect_true(is.na(NA_wkb_))
  expect_true(is.na(NA_collection_))
  expect_true(is.na(NA_xy_))
  expect_true(is.na(NA_segment_))
  expect_true(is.na(NA_rect_))
})

test_that("geo_is_missing works", {
  expect_false(geo_is_missing(geo_xy(NA, 1)))

  expect_true(geo_is_missing(NA_segment_))
  expect_false(geo_is_missing(geo_segment(geo_xy(NA, NA), geo_xy(NA, NA), srid = 1)))
  expect_false(geo_is_missing(geo_segment(geo_xy(1, NA), geo_xy(NA, NA))))
  expect_false(geo_is_missing(geo_segment(geo_xy(NA, 1), geo_xy(NA, NA))))
  expect_false(geo_is_missing(geo_segment(geo_xy(NA, NA), geo_xy(1, NA))))
  expect_false(geo_is_missing(geo_segment(geo_xy(NA, NA), geo_xy(NA, 1))))

  expect_true(geo_is_missing(NA_rect_))
  expect_false(geo_is_missing(geo_rect(NA, NA, NA, NA, srid = 1)))
  expect_false(geo_is_missing(geo_rect(1, NA, NA, NA)))
  expect_false(geo_is_missing(geo_rect(NA, 1, NA, NA)))
  expect_false(geo_is_missing(geo_rect(NA, NA, 1, NA)))
  expect_false(geo_is_missing(geo_rect(NA, NA, NA, 1)))

  # default
  expect_identical(
    geo_is_missing(NA_character_),
    geo_is_missing(NA_wkt_)
  )
})

test_that("geo_has_missing works", {
  skip("skipping has_missing now")

  expect_identical(geo_has_missing(geo_wkt(NA)), NA)
  expect_false(geo_has_missing(geo_wkt("POINT (30 10)")))
  expect_true(geo_has_missing(geo_wkt("POINT (30 nan)")))
  expect_true(geo_has_missing(geo_wkt("POINT (nan 10)")))

  expect_identical(geo_has_missing(NA_wkb_), NA)
  expect_true(geo_has_missing(as_geo_wkb(geo_wkt("LINESTRING (nan 1, 2 3)"))))
  expect_true(geo_has_missing(as_geo_wkb(geo_wkt("LINESTRING (1 1, 2 nan)"))))
  expect_false(geo_has_missing(as_geo_wkb(geo_wkt("LINESTRING (1 1, 2 3)"))))
  expect_true(geo_has_missing(as_geo_wkb(geo_wkt("LINESTRING (nan nan, nan nan)"))))
  expect_true(geo_has_missing(as_geo_wkb(geo_wkt("POLYGON ((1 1, nan nan, nan nan, 1 1))"))))

  expect_true(geo_has_missing(NA_xy_))
  expect_true(geo_has_missing(geo_xy(NA, NA)))
  expect_true(geo_has_missing(geo_xy(NA, 1)))
  expect_true(geo_has_missing(geo_xy(1, NA)))
  expect_false(geo_has_missing(geo_xy(1, 1)))

  expect_identical(geo_has_missing(NA_segment_), NA)
  expect_true(geo_has_missing(geo_segment(geo_xy(NA, NA), geo_xy(NA, NA))))
  expect_true(geo_has_missing(geo_segment(geo_xy(1, NA), geo_xy(NA, NA))))
  expect_true(geo_has_missing(geo_segment(geo_xy(1, 1), geo_xy(NA, NA))))
  expect_true(geo_has_missing(geo_segment(geo_xy(1, 1), geo_xy(1, NA))))
  expect_false(geo_has_missing(geo_segment(geo_xy(1, 1), geo_xy(1, 1))))

  expect_identical(geo_has_missing(NA_rect_), NA)
  expect_true(geo_has_missing(geo_rect(NA, NA, NA, NA)))
  expect_true(geo_has_missing(geo_rect(1, NA, NA, NA)))
  expect_true(geo_has_missing(geo_rect(1, 1, NA, NA)))
  expect_true(geo_has_missing(geo_rect(1, 1, 1, NA)))
  expect_false(geo_has_missing(geo_rect(1, 1, 1, 1)))

  # default
  expect_identical(
    geo_has_missing("LINESTRING (0 nan, 1 1)"),
    geo_has_missing(geo_wkt("LINESTRING (0 nan, 1 1)"))
  )
})

test_that("geo_has_missing works with nested collections", {
  skip("skipping has_missing now")

  expect_false(
    geo_has_missing(geo_wkt("GEOMETRYCOLLECTION (GEOMETRYCOLLECTION EMPTY)"))
  )

  expect_false(
    geo_has_missing(geo_wkt("GEOMETRYCOLLECTION (LINESTRING (0 0, 1 2, 6 4))"))
  )

  expect_false(
    geo_has_missing(geo_wkt("GEOMETRYCOLLECTION (LINESTRING (0 inf, 1 2, 6 4))"))
  )

  expect_true(
    geo_has_missing(geo_wkt("GEOMETRYCOLLECTION (LINESTRING (nan 0, 1 2, 6 4))"))
  )

  expect_true(
    geo_has_missing(geo_wkt("GEOMETRYCOLLECTION (LINESTRING (0 nan, 1 2, 6 4))"))
  )
})

test_that("geo_is_finite works", {
  skip("skipping is_finite now")

  expect_identical(geo_is_finite(geo_wkt(NA)), NA)
  expect_true(geo_is_finite(geo_wkt("POINT (30 10)")))
  expect_false(geo_is_finite(geo_wkt("POINT (30 nan)")))
  expect_false(geo_is_finite(geo_wkt("POINT (nan 10)")))
  expect_false(geo_is_finite(geo_wkt("POINT (30 inf)")))

  expect_identical(geo_is_finite(NA_wkb_), NA)
  expect_false(geo_is_finite(as_geo_wkb(geo_wkt("LINESTRING (nan 1, 2 3)"))))
  expect_false(geo_is_finite(as_geo_wkb(geo_wkt("LINESTRING (1 1, 2 nan)"))))
  expect_true(geo_is_finite(as_geo_wkb(geo_wkt("LINESTRING EMPTY"))))
  expect_true(geo_is_finite(as_geo_wkb(geo_wkt("LINESTRING (1 1, 2 3)"))))
  expect_false(geo_is_finite(as_geo_wkb(geo_wkt("LINESTRING (nan nan, nan nan)"))))
  expect_false(geo_is_finite(as_geo_wkb(geo_wkt("POLYGON ((1 1, nan nan, nan nan, 1 1))"))))

  expect_false(geo_is_finite(geo_xy(Inf, -Inf)))
  expect_false(geo_is_finite(geo_xy(NA, 1)))
  expect_false(geo_is_finite(geo_xy(1, NA)))
  expect_false(geo_is_finite(geo_xy(1, -Inf)))
  expect_true(geo_is_finite(geo_xy(1, 1)))

  expect_identical(geo_is_finite(NA_segment_), NA)
  expect_false(geo_is_finite(geo_segment(geo_xy(NA, NA), geo_xy(NA, NA))))
  expect_false(geo_is_finite(geo_segment(geo_xy(1, NA), geo_xy(NA, NA))))
  expect_false(geo_is_finite(geo_segment(geo_xy(1, 1), geo_xy(NA, NA))))
  expect_false(geo_is_finite(geo_segment(geo_xy(1, 1), geo_xy(1, NA))))
  expect_false(geo_is_finite(geo_segment(geo_xy(1, 1), geo_xy(Inf, 1))))
  expect_true(geo_is_finite(geo_segment(geo_xy(1, 1), geo_xy(1, 1))))

  expect_identical(geo_is_finite(NA_rect_), NA)
  expect_false(geo_is_finite(geo_rect(NA, NA, NA, NA)))
  expect_false(geo_is_finite(geo_rect(1, NA, NA, NA)))
  expect_false(geo_is_finite(geo_rect(1, 1, NA, NA)))
  expect_false(geo_is_finite(geo_rect(1, 1, 1, NA)))
  expect_true(geo_is_finite(geo_rect(1, 1, 1, 1)))

  # default
  expect_identical(
    geo_is_finite("LINESTRING (0 inf, 1 1)"),
    geo_is_finite(geo_wkt("LINESTRING (0 inf, 1 1)"))
  )
})

test_that("geo_is_finite works with nested collections", {
  skip("skipping is_finite now")

  expect_true(
    geo_is_finite(geo_wkt("GEOMETRYCOLLECTION (GEOMETRYCOLLECTION EMPTY)"))
  )

  expect_true(
    geo_is_finite(geo_wkt("GEOMETRYCOLLECTION (LINESTRING (0 0, 1 2, 6 4))"))
  )

  expect_false(
    geo_is_finite(geo_wkt("GEOMETRYCOLLECTION (LINESTRING (nan 0, 1 2, 6 4))"))
  )

  expect_false(
    geo_is_finite(geo_wkt("GEOMETRYCOLLECTION (LINESTRING (0 nan, 1 2, 6 4))"))
  )

  expect_false(
    geo_is_finite(geo_wkt("GEOMETRYCOLLECTION (LINESTRING (0 inf, 1 2, 6 4))"))
  )
})

test_that("geo_is_empty works", {
  # point
  expect_false(geo_is_empty(geo_wkt("POINT (nan nan)")))
  expect_true(geo_is_empty(geo_wkt("POINT EMPTY")))
  expect_false(geo_is_empty(geo_wkt("MULTIPOINT (nan nan)")))
  expect_false(geo_is_empty(geo_wkt("POINT (1 nan)")))

  expect_false(geo_is_empty(as_geo_wkb(geo_wkt("POINT (nan nan)"))))
  expect_false(geo_is_empty(as_geo_wkb(geo_wkt("POINT EMPTY")))) # no empty point in WKB
  expect_true(geo_is_empty(as_geo_wkb(geo_wkt("MULTIPOINT EMPTY"))))
  expect_false(geo_is_empty(as_geo_wkb(geo_wkt("MULTIPOINT (1 nan)"))))
  # "MULTIPOINT (nan nan)" currently cannot be written to WKB
  # expect_true(geo_is_empty(as_geo_wkb(geo_wkt("MULTIPOINT (nan nan)"))))

  expect_true(geo_is_empty(geo_xy(NA, NA)))
  expect_false(geo_is_empty(geo_xy(1, NA)))
  expect_false(geo_is_empty(geo_xy(NA, 1)))

  expect_identical(geo_is_empty(NA_segment_), TRUE)
  expect_true(geo_is_empty(geo_segment(geo_xy(NA, NA), geo_xy(NA, NA))))
  expect_false(geo_is_empty(geo_segment(geo_xy(1, NA), geo_xy(NA, NA))))
  expect_false(geo_is_empty(geo_segment(geo_xy(1, 1), geo_xy(NA, NA))))
  expect_false(geo_is_empty(geo_segment(geo_xy(1, 1), geo_xy(1, NA))))
  expect_false(geo_is_empty(geo_segment(geo_xy(1, 1), geo_xy(Inf, 1))))
  expect_false(geo_is_empty(geo_segment(geo_xy(1, 1), geo_xy(1, 1))))

  expect_identical(geo_is_empty(NA_rect_), TRUE)
  expect_true(geo_is_empty(geo_rect(NA, NA, NA, NA)))
  expect_true(geo_is_empty(geo_rect(1, NA, NA, NA)))
  expect_true(geo_is_empty(geo_rect(1, 1, NA, NA)))
  expect_true(geo_is_empty(geo_rect(1, 1, 1, NA)))
  expect_false(geo_is_empty(geo_rect(1, 1, 1, 1)))

  # default
  expect_identical(geo_is_empty("POINT EMPTY"), geo_is_empty(geo_wkt("POINT EMPTY")))
})

test_that("handling of empty points is consistent across geovctrs", {
  skip("skipping missing/finite for now")

  expect_true(geo_is_missing(NA_xy_))

  expect_false(geo_is_missing(geo_wkt("POINT (nan nan)")))
  expect_false(geo_is_missing(geo_wkt("POINT EMPTY")))
  expect_false(geo_is_missing(geo_wkt("MULTIPOINT EMPTY")))
  expect_false(geo_is_missing(geo_wkt("MULTIPOINT (nan nan)")))
  expect_true(geo_is_missing(geo_xy(NA, NA)))
  expect_false(geo_is_missing(geo_xy(1, NA)))
  expect_false(geo_is_missing(geo_xy(NA, 1)))

  expect_true(geo_is_finite(geo_wkt("POINT (nan nan)")))
  expect_true(geo_is_finite(geo_wkt("POINT EMPTY")))
  expect_true(geo_is_finite(geo_wkt("MULTIPOINT EMPTY")))
  expect_true(geo_is_finite(geo_wkt("MULTIPOINT (nan nan)")))
  expect_false(geo_is_finite(geo_xy(NA, NA)))
  expect_false(geo_is_finite(geo_xy(1, NA)))
  expect_false(geo_is_finite(geo_xy(NA, 1)))

  expect_false(geo_has_missing(geo_wkt("POINT (nan nan)")))
  expect_false(geo_has_missing(geo_wkt("POINT EMPTY")))
  expect_false(geo_has_missing(geo_wkt("MULTIPOINT EMPTY")))
  expect_false(geo_has_missing(geo_wkt("MULTIPOINT (nan nan)")))
  expect_true(geo_has_missing(geo_xy(NA, NA)))
  expect_true(geo_has_missing(geo_xy(1, NA)))
  expect_true(geo_has_missing(geo_xy(NA, 1)))

  expect_false(geo_is_empty(geo_wkt("POINT (nan nan)")))
  expect_true(geo_is_empty(geo_wkt("POINT EMPTY")))
  expect_true(geo_is_empty(geo_wkt("MULTIPOINT EMPTY")))
  expect_false(geo_is_empty(geo_wkt("MULTIPOINT (nan nan)")))
  expect_true(geo_is_empty(geo_xy(NA, NA)))
  expect_false(geo_is_empty(geo_xy(1, NA)))
  expect_false(geo_is_empty(geo_xy(NA, 1)))
})
