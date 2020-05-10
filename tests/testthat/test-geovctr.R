context("test-geovctr")

test_that("all geovctrs pass expect_geovctr", {
  expect_geovctr(geo_wkt())
  expect_geovctr(geo_wkb())
  expect_geovctr(geo_collection())
  expect_geovctr(geo_xy())
  expect_geovctr(geo_xyz())
  expect_geovctr(geo_segment())
  expect_geovctr(geo_rect())
})

test_that("geovctrs are left alone by restore_geovctr", {
  expect_identical(restore_geovctr(geo_wkt(), geo_wkb()), geo_wkb())
})

test_that("character works with as_geovctr()", {
  expect_geovctr(as_geovctr("POINT (0 0)"))
})

test_that("data.frame works with as_geovctr()", {
  expect_geovctr(as_geovctr(geo_nc))
  expect_error(as_geovctr(tibble()), "Can't find geovctr")
  expect_error(as_geovctr(tibble(x = geo_wkt(), y = geo_wkt())), "More than one")

  # restore method replaces the geometry column
  expect_identical(restore_geovctr(tibble(x = geo_wkt()), geo_wkb()), tibble(x = geo_wkb()))

  # check with transformation functions
  expect_is(geo_envelope(geo_nc), "data.frame")

  skip("setting srid not implemented")
  expect_is(geo_set_srid(geo_nc, 26920), "data.frame")

  # check with vectorization
  skip("set with set z")
  expect_identical(
    geo_set_z(geo_nc[1, ], 4:6),
    geo_set_z(geo_nc[c(1, 1, 1), ], 4:6)
  )
})


