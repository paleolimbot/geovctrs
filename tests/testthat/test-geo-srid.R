
test_that("SRID extraction works", {

  geometries <- c(
    geo_point(geo_xy(259473, 4876249), srid = 26920),
    geo_point(geo_xy(-66, 44), srid = 0)
  )

  geometries_wkt <- geo_wkt(c("POINT (259473 4876249)", "POINT (-66 44)"))
  geometries_wkb <- as_geo_wkb(geometries)

  # collection
  expect_identical(geo_srid(geometries), c(26920L, 0L))
  expect_identical(geo_srid(geo_set_srid(geometries, 0)), c(0L, 0L))

  # wkt
  expect_identical(geo_srid(geo_set_srid(geometries_wkt, 0)), c(0L, 0L))
  expect_error(geo_srid(geo_set_srid(geometries_wkt, 2)), "Can't store SRID")

  # wkb
  expect_identical(geo_srid(geometries_wkb), c(26920L, 0L))
  expect_identical(geo_srid(geo_set_srid(geometries_wkb, 0)), c(0L, 0L))

  # via as_geovctr()
  expect_identical(geo_srid("POINT EMPTY"), 0L)
})

test_that("misssing values have missing SRIDs", {
  expect_identical(geo_srid(NA_wkt_), NA_integer_)
  expect_identical(geo_srid(NA_wkb_), NA_integer_)
  expect_identical(geo_srid(NA_collection_), NA_integer_)
  expect_identical(geo_srid(NA_xy_), NA_integer_)
  expect_identical(geo_srid(NA_segment_), NA_integer_)
  expect_identical(geo_srid(NA_rect_), NA_integer_)
})
