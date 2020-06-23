
test_that("geo_point() works", {
  expect_identical(as_wkt(geo_point(geo_xy(1, 2))), wkt("POINT (1 2)"))
  expect_identical(as_wkt(geo_point(geo_xy(1, 2), srid = 2)), wkt("SRID=2;POINT (1 2)"))
  expect_identical(as_wkt(geo_point(geo_xyz(1, 2, 3))), wkt("POINT Z (1 2 3)"))
})

test_that("geo_linestring() works", {
  expect_identical(as_wkt(geo_linestring(geo_xy(1:2, 2:3))), wkt("LINESTRING (1 2, 2 3)"))
  expect_identical(
    as_wkt(geo_linestring(geo_xy(1:2, 2:3), srid = 2)),
    wkt("SRID=2;LINESTRING (1 2, 2 3)")
  )
  expect_identical(
    as_wkt(geo_linestring(geo_xyz(1:2, 2:3, 3:4))),
    wkt("LINESTRING Z (1 2 3, 2 3 4)")
  )
})

test_that("geo_polygon() works", {
  expect_identical(as_wkt(geo_polygon(geo_xy())), wkt("POLYGON EMPTY"))
  expect_identical(
    as_wkt(geo_polygon(geo_xy(c(0, 10, 10, 0), c(0, 0, 10, 10)))),
    wkt("POLYGON ((0 0, 10 0, 10 10, 0 10, 0 0))")
  )
  expect_identical(
    as_wkt(geo_polygon(geo_xy(c(0, 10, 10, 0), c(0, 0, 10, 10)), srid = 12)),
    wkt("SRID=12;POLYGON ((0 0, 10 0, 10 10, 0 10, 0 0))")
  )
  expect_identical(
    as_wkt(geo_polygon(geo_xyz(c(0, 10, 10, 0), c(0, 0, 10, 10), 3))),
    wkt("POLYGON Z ((0 0 3, 10 0 3, 10 10 3, 0 10 3, 0 0 3))")
  )
})
