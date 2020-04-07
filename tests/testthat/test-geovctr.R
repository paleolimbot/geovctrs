
test_that("all geovctrs pass expect_geovctr", {
  expect_geovctr(geo_wkt())
  expect_geovctr(geo_wkb())
  expect_geovctr(geo_collection())
  expect_geovctr(geo_xy())
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
  expect_is(geo_set_srid(geo_nc, 26920), "data.frame")
})

test_that("sf/sfc works with as_geovctr()", {
  skip_if_not_installed("sf")

  sf_nc <- pkg_fun("sf", "read_sf")(system.file("shape/nc.shp", package = "sf"))
  sfc_nc <- sf_nc[[attr(sf_nc, "sf_column")]]

  expect_is(as_geovctr(sf_nc), "geo_wkb")
  expect_length(as_geovctr(sf_nc), nrow(sf_nc))
  expect_identical(as_geovctr(sf_nc), as_geovctr(sfc_nc))

  expect_identical(restore_geovctr(sf_nc, as_geovctr(sf_nc)), sf_nc)
  expect_identical(restore_geovctr(sfc_nc, as_geovctr(sfc_nc)), sfc_nc)

  # check with transformation functions
  expect_is(geo_envelope(sf_nc), "sf")
  expect_is(geo_envelope(sfc_nc), "sfc")
  expect_is(geo_set_srid(sf_nc, 0), "sf")
  expect_is(geo_set_srid(sfc_nc, 0), "sfc")
})
