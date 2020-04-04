
test_that("geo_is_missing works", {
  expect_true(geo_is_missing(geo_xy()[NA_integer_]))
  expect_true(geo_is_missing(geo_xy(NA, NA)))
  expect_false(geo_is_missing(geo_xy(NA, 1)))

  expect_true(geo_is_missing(geo_segment()[NA_integer_]))
  expect_false(geo_is_missing(geo_segment(geo_xy(NA, NA), geo_xy(NA, NA), srid = 1)))
  expect_false(geo_is_missing(geo_segment(geo_xy(1, NA), geo_xy(NA, NA))))
  expect_false(geo_is_missing(geo_segment(geo_xy(NA, 1), geo_xy(NA, NA))))
  expect_false(geo_is_missing(geo_segment(geo_xy(NA, NA), geo_xy(1, NA))))
  expect_false(geo_is_missing(geo_segment(geo_xy(NA, NA), geo_xy(NA, 1))))

  expect_true(geo_is_missing(geo_rect()[NA_integer_]))
  expect_false(geo_is_missing(geo_rect(NA, NA, NA, NA, srid = 1)))
  expect_false(geo_is_missing(geo_rect(1, NA, NA, NA)))
  expect_false(geo_is_missing(geo_rect(NA, 1, NA, NA)))
  expect_false(geo_is_missing(geo_rect(NA, NA, 1, NA)))
  expect_false(geo_is_missing(geo_rect(NA, NA, NA, 1)))
})

test_that("geo_has_missing works", {
  expect_false(geo_has_missing(geo_wkt("POINT (30 10)")))
  expect_true(geo_has_missing(geo_wkt("POINT (30 nan)")))
  expect_true(geo_has_missing(geo_wkt("POINT (nan 10)")))
  # tricky corner case...GEOS considers this empty
  expect_true(geo_has_missing(geo_wkt("POINT (nan nan)")))

  expect_true(geo_has_missing(as_geo_wkb(geo_wkt("LINESTRING (nan 1, 2 3)"))))
  expect_true(geo_has_missing(as_geo_wkb(geo_wkt("LINESTRING (1 1, 2 nan)"))))
  expect_false(geo_has_missing(as_geo_wkb(geo_wkt("LINESTRING (1 1, 2 3)"))))
  expect_true(geo_has_missing(as_geo_wkb(geo_wkt("LINESTRING (nan nan, nan nan)"))))
  expect_true(geo_has_missing(as_geo_wkb(geo_wkt("POLYGON ((1 1, nan nan, nan nan, 1 1))"))))

  expect_true(geo_has_missing(geo_xy(NA, NA)))
  expect_true(geo_has_missing(geo_xy(NA, 1)))
  expect_true(geo_has_missing(geo_xy(1, NA)))
  expect_false(geo_has_missing(geo_xy(1, 1)))

  expect_true(geo_has_missing(geo_segment(geo_xy(NA, NA), geo_xy(NA, NA))))
  expect_true(geo_has_missing(geo_segment(geo_xy(1, NA), geo_xy(NA, NA))))
  expect_true(geo_has_missing(geo_segment(geo_xy(1, 1), geo_xy(NA, NA))))
  expect_true(geo_has_missing(geo_segment(geo_xy(1, 1), geo_xy(1, NA))))
  expect_false(geo_has_missing(geo_segment(geo_xy(1, 1), geo_xy(1, 1))))

  expect_true(geo_has_missing(geo_rect(NA, NA, NA, NA)))
  expect_true(geo_has_missing(geo_rect(1, NA, NA, NA)))
  expect_true(geo_has_missing(geo_rect(1, 1, NA, NA)))
  expect_true(geo_has_missing(geo_rect(1, 1, 1, NA)))
  expect_false(geo_has_missing(geo_rect(1, 1, 1, 1)))
})

test_that("geo_is_finite works", {
  expect_true(geo_is_finite(geo_wkt("POINT (30 10)")))
  expect_false(geo_is_finite(geo_wkt("POINT (30 nan)")))
  expect_false(geo_is_finite(geo_wkt("POINT (nan 10)")))
  expect_true(geo_is_finite(geo_wkt("POINT EMPTY")))
  expect_false(geo_is_finite(geo_wkt("POINT (30 inf)")))

  # tricky corner case...GEOS considers these empty
  expect_false(geo_is_finite(geo_wkt("POINT (nan nan)")))
  expect_false(geo_is_finite(geo_wkt("MULTIPOINT (nan nan)")))

  expect_false(geo_is_finite(as_geo_wkb(geo_wkt("LINESTRING (nan 1, 2 3)"))))
  expect_false(geo_is_finite(as_geo_wkb(geo_wkt("LINESTRING (1 1, 2 nan)"))))
  expect_true(geo_is_finite(as_geo_wkb(geo_wkt("LINESTRING EMPTY"))))
  expect_true(geo_is_finite(as_geo_wkb(geo_wkt("LINESTRING (1 1, 2 3)"))))
  expect_false(geo_is_finite(as_geo_wkb(geo_wkt("LINESTRING (nan nan, nan nan)"))))
  expect_false(geo_is_finite(as_geo_wkb(geo_wkt("POLYGON ((1 1, nan nan, nan nan, 1 1))"))))

  # geo_xy(NA, NA) is both missing and empty
  expect_true(geo_is_finite(geo_xy(NA, NA)))
  expect_false(geo_is_finite(geo_xy(Inf, -Inf)))
  expect_false(geo_is_finite(geo_xy(NA, 1)))
  expect_false(geo_is_finite(geo_xy(1, NA)))
  expect_false(geo_is_finite(geo_xy(1, -Inf)))
  expect_true(geo_is_finite(geo_xy(1, 1)))

  expect_false(geo_is_finite(geo_segment(geo_xy(NA, NA), geo_xy(NA, NA))))
  expect_false(geo_is_finite(geo_segment(geo_xy(1, NA), geo_xy(NA, NA))))
  expect_false(geo_is_finite(geo_segment(geo_xy(1, 1), geo_xy(NA, NA))))
  expect_false(geo_is_finite(geo_segment(geo_xy(1, 1), geo_xy(1, NA))))
  expect_false(geo_is_finite(geo_segment(geo_xy(1, 1), geo_xy(Inf, 1))))
  expect_true(geo_is_finite(geo_segment(geo_xy(1, 1), geo_xy(1, 1))))

  expect_false(geo_is_finite(geo_rect(NA, NA, NA, NA)))
  expect_false(geo_is_finite(geo_rect(1, NA, NA, NA)))
  expect_false(geo_is_finite(geo_rect(1, 1, NA, NA)))
  expect_false(geo_is_finite(geo_rect(1, 1, 1, NA)))
  expect_true(geo_is_finite(geo_rect(1, 1, 1, 1)))
})
