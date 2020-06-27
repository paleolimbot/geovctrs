
test_that("SRID extraction works", {
  geometries_wkt <- wkt(c("SRID=26920;POINT (259473 4876249)", "POINT (-66 44)"))
  geometries_wkb <- as_wkb(geometries_wkt)

  # wkt
  expect_identical(geo_srid(geometries_wkt), c(26920L, 0L))
  expect_identical(geo_srid(geo_set_srid(geometries_wkt, 0)), c(0L, 0L))
  expect_identical(geo_srid(geo_set_srid(geometries_wkt, c(1, 2))), c(1L, 2L))
  expect_identical(geo_set_srid("SRID=4326;POINT (30 10)", 0), wkt("SRID=0;POINT (30 10)"))
  expect_identical(geo_set_srid("SRID=4326;POINT (30 10)", NA), wkt("POINT (30 10)"))
  expect_true(all(geo_srid(geo_set_srid(geo_example_wkt, 321)) == 321, na.rm = TRUE))

  # wkb
  expect_identical(geo_srid(geometries_wkb), c(26920L, 0L))
  expect_identical(geo_srid(geo_set_srid(geometries_wkb, 0)), c(0L, 0L))
  expect_identical(geo_set_srid(as_wkb("SRID=4326;POINT (30 10)"), 0), as_wkb("SRID=0;POINT (30 10)"))
  expect_identical(geo_set_srid(as_wkb("SRID=4326;POINT (30 10)"), NA), as_wkb("POINT (30 10)"))
  expect_true(all(geo_srid(geo_set_srid(as_wkb(geo_example_wkt), 321)) == 321, na.rm = TRUE))

  # wksxp
  expect_identical(geo_srid(as_wksxp(geometries_wkb)), c(26920L, 0L))
  expect_identical(geo_srid(geo_set_srid(as_wksxp(geometries_wkb), 0)), c(0L, 0L))
  expect_identical(geo_set_srid(as_wksxp("SRID=4326;POINT (30 10)"), 0), as_wksxp("SRID=0;POINT (30 10)"))
  expect_identical(geo_set_srid(as_wksxp("SRID=4326;POINT (30 10)"), NA), as_wksxp("POINT (30 10)"))
  expect_true(all(geo_srid(geo_set_srid(as_wksxp(geo_example_wkt), 321)) == 321, na.rm = TRUE))

  # xy
  expect_identical(geo_srid(geo_xy(0, 0)), 0L)
  expect_identical(geo_set_srid(geo_xy(0, 0), NA), geo_xy(0, 0))
  expect_error(geo_set_srid(geo_xy(0, 0), 1L), "Can't store SRID")

  # segment
  expect_identical(geo_srid(geo_segment(0, 0, 1, 1, srid = 1)), 1L)
  expect_identical(
    geo_set_srid(geo_segment(0, 0, 1, 1, srid = 1), 2L),
    geo_segment(0, 0, 1, 1, srid = 2)
  )

  # rect
  expect_identical(geo_srid(geo_rect(0, 1, 2, 3, srid = 1L)), 1L)
  expect_identical(
    geo_set_srid(geo_rect(0, 1, 2, 3, srid = 1L), 2L),
    geo_rect(0, 1, 2, 3, srid = 2L)
  )

  # via as_geovctr()
  expect_identical(geo_srid("POINT EMPTY"), 0L)
})

test_that("geo_set_srid() is vectorized along x and srid", {
  expect_identical(
    geo_srid(geo_set_srid(as_wkb("POINT (10 20)"), 4:6)),
    4:6
  )

  expect_identical(
    geo_srid(geo_set_srid(as_wkb("POINT (10 20)"), integer())),
    integer()
  )

  expect_identical(
    geo_srid(geo_set_srid(wkb(), 1)),
    integer()
  )
})

test_that("misssing values have missing SRIDs", {
  expect_identical(geo_srid(NA_wkt_), NA_integer_)
  expect_identical(geo_srid(NA_wkb_), NA_integer_)
  expect_identical(geo_srid(NA_xy_), NA_integer_)
  expect_identical(geo_srid(NA_segment_), NA_integer_)
  expect_identical(geo_srid(NA_rect_), NA_integer_)
})
