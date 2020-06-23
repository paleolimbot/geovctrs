
context("test-sf-compat")

test_that("sf/sfc works with as_geovctr()", {
  skip_if_not_installed("sf")
  skip("sf might not work with vec_cast")

  sf_nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  sfc_nc <- sf_nc[[attr(sf_nc, "sf_column")]]

  expect_is(as_geovctr(sf_nc), "wk_wkb")
  expect_length(as_geovctr(sf_nc), nrow(sf_nc))
  expect_identical(as_geovctr(sf_nc), as_geovctr(sfc_nc))

  expect_is(restore_geovctr(sf_nc, as_geovctr(sf_nc)), "sf")
  expect_is(restore_geovctr(sfc_nc, as_geovctr(sfc_nc)), "sfc")
  expect_is(sf::st_as_sf(as_geovctr(sfc_nc)), "sf")
  expect_is(sf::st_as_sfc(as_geovctr(sfc_nc)), "sfc")

  # check with transformation functions
  expect_is(geo_envelope(sf_nc), "sf")
  expect_is(geo_envelope(sfc_nc), "sfc")
  skip("set srid didn't work")
  expect_is(geo_set_srid(sf_nc, 0), "sf")
  expect_is(geo_set_srid(sfc_nc, 0), "sfc")

  sfc_tiny <- sf::st_sfc(sf::st_point(c(30, 10)))
  sf_tiny <- sf::st_as_sf(tibble(geom = sfc_tiny))

  # check with vectorized parameters
  skip("set z")
  expect_identical(
    nrow(geo_set_z(sf_nc[1, ], 4:6)),
    3L
  )

  # check with plot
  vdiffr::expect_doppelganger(
    "sfc plot",
    function() geo_plot(sfc_tiny)
  )

  vdiffr::expect_doppelganger(
    "sf plot",
    function() geo_plot(sf_tiny)
  )
})

test_that("casting and c() work with sfc", {
  skip_if_not_installed("sf")
  skip("sf might not work with vec_ptype2")

  wkt <- wkt("POINT (30 10)")
  wkb <- as_wkb(wkt)
  col <- as_geo_collection(wkt)
  xy <- as_geo_xy(wkt)
  seg <- geo_segment(0, 0, 30, 10)
  rect <- geo_rect(0, 0, 30, 10)
  sfc <- sf::st_sfc(sf::st_point(c(30, 10)))
  sfc_seg <- sf::st_sfc(sf::st_linestring(rbind(c(0, 0), c(30, 10))))

  expect_is(vec_cast(sfc, wkb()), "wk_wkb")
  expect_is(vec_cast(sfc, wkt()), "wk_wkt")
  expect_is(vec_cast(sfc, geo_xy()), "geovctrs_xy")
  expect_is(vec_cast(sfc_seg, geo_segment()), "geovctrs_segment")

  expect_is(vec_cast(wkb, sf::st_sfc()), "sfc")
  expect_is(vec_cast(wkt, sf::st_sfc()), "sfc")
  expect_is(vec_cast(col, sf::st_sfc()), "sfc")
  expect_is(vec_cast(xy, sf::st_sfc()), "sfc")
  expect_is(vec_cast(seg, sf::st_sfc()), "sfc")
  expect_is(vec_cast(rect, sf::st_sfc()), "sfc")

  expect_is(vec_c(wkb, sfc), "wk_wkb")
  expect_is(vec_c(wkt, sfc), "sfc")
  expect_is(vec_c(col, sfc), "sfc")
  expect_is(vec_c(xy, sfc), "sfc")
  expect_is(vec_c(seg, sfc), "sfc")
  expect_is(vec_c(rect, sfc), "sfc")

  expect_is(vec_c(sfc, wkb), "wk_wkb")
  expect_is(vec_c(sfc, wkt), "sfc")
  expect_is(vec_c(sfc, col), "sfc")
  expect_is(vec_c(sfc, xy), "sfc")
  expect_is(vec_c(sfc, seg), "sfc")
  expect_is(vec_c(sfc, rect), "sfc")
})
