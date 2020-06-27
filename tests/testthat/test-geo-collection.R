
test_that("geo_point() works", {
  expect_identical(as_wkt(geo_point()), wkt("POINT EMPTY"))
  expect_identical(as_wkt(geo_point(geo_xy(1, 2))), wkt("POINT (1 2)"))
  expect_identical(as_wkt(geo_point(geo_xy(1, 2), srid = 2)), wkt("SRID=2;POINT (1 2)"))
  expect_identical(as_wkt(geo_point(geo_xyz(1, 2, 3))), wkt("POINT Z (1 2 3)"))
})

test_that("geo_linestring() works", {
  expect_identical(as_wkt(geo_linestring()), wkt("LINESTRING EMPTY"))
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
  expect_identical(as_wkt(geo_polygon()), wkt("POLYGON EMPTY"))
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

test_that("geo_collection() works", {
  expect_identical(as_wkt(geo_collection()), wkt("GEOMETRYCOLLECTION EMPTY"))
  expect_identical(
    as_wkt(geo_collection("POINT (30 10)")),
    wkt("GEOMETRYCOLLECTION (POINT (30 10))")
  )
  expect_identical(
    as_wkt(geo_collection("POINT (30 10)", srid = 12)),
    wkt("SRID=12;GEOMETRYCOLLECTION (POINT (30 10))")
  )
})

test_that("geo_multi*() constructors works", {
  expect_error(geo_multipoint("LINESTRING (0 0, 1 1)"), "All elements")

  expect_identical(as_wkt(geo_multipoint()), wkt("MULTIPOINT EMPTY"))
  expect_identical(
    as_wkt(geo_multipoint(c("POINT (0 0)", "POINT (2 3)"))),
    wkt("MULTIPOINT ((0 0), (2 3))")
  )
  expect_identical(
    as_wkt(geo_multipoint(c("POINT (0 0)", "POINT (2 3)"), srid = 123)),
    wkt("SRID=123;MULTIPOINT ((0 0), (2 3))")
  )

  expect_identical(as_wkt(geo_multilinestring()), wkt("MULTILINESTRING EMPTY"))
  expect_identical(
    as_wkt(geo_multilinestring(c("LINESTRING (0 0, 1 1)", "LINESTRING (2 3, 3 4)"))),
    wkt("MULTILINESTRING ((0 0, 1 1), (2 3, 3 4))")
  )

  expect_identical(as_wkt(geo_multipolygon()), wkt("MULTIPOLYGON EMPTY"))
  expect_identical(
    as_wkt(geo_multipolygon(c("POLYGON ((0 0, 1 1, 0 1, 0 0))", "POLYGON ((0 0, -1 -1, 0 -1, 0 0))"))),
    wkt("MULTIPOLYGON (((0 0, 1 1, 0 1, 0 0)), ((0 0, -1 -1, 0 -1, 0 0)))")
  )
  expect_identical(
    as_wkt(
      geo_multipolygon(
        c("POLYGON ((0 0, 1 1, 0 1, 0 0))", "POLYGON ((0 0, -1 -1, 0 -1, 0 0))"),
        srid = 12
      )
    ),
    wkt("SRID=12;MULTIPOLYGON (((0 0, 1 1, 0 1, 0 0)), ((0 0, -1 -1, 0 -1, 0 0)))")
  )


})
