
context("test-sf-compat")

test_that("sf/sfc works with as_geovctr()", {
  skip_if_not_installed("sf")

  sf_nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  sfc_nc <- sf_nc[[attr(sf_nc, "sf_column")]]

  expect_is(as_geovctr(sf_nc), "wk_wkb")
  expect_length(as_geovctr(sf_nc), nrow(sf_nc))
  expect_identical(as_geovctr(sf_nc), as_geovctr(sfc_nc))

  expect_is(restore_geovctr(sf_nc, as_geovctr(sf_nc)), "sf")
  expect_is(restore_geovctr(sfc_nc, as_geovctr(sfc_nc)), "sfc")

  # check with transformation functions
  expect_is(geo_envelope(sf_nc), "sf")
  expect_is(geo_envelope(sfc_nc), "sfc")
  expect_is(geo_set_z(sf_nc, 12), "sf")
  expect_is(geo_set_z(sfc_nc, 12), "sfc")

  sfc_tiny <- sf::st_sfc(sf::st_point(c(30, 10)))
  sf_tiny <- sf::st_as_sf(tibble(geom = sfc_tiny))

  # check with vectorized parameters
  expect_identical(
    nrow(geo_set_z(sf_nc[1, ], 4:6)),
    3L
  )
})
