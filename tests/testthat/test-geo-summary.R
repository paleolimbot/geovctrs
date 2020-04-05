
test_that("geo_coordinate_dimensions works", {
  expect_identical(geo_coordinate_dimensions(NA_character_), NA_integer_)
  expect_identical(geo_coordinate_dimensions("POINT (30 10)"), 2L)
  expect_identical(geo_coordinate_dimensions("POINT Z (30 10 0)"), 3L)
  expect_identical(geo_coordinate_dimensions("LINESTRING (0 0, 10 11)"), 2L)
  expect_identical(geo_coordinate_dimensions("POLYGON EMPTY"), 3L)
})

test_that("geo_n_coordinates works", {
  expect_identical(geo_n_coordinates(NA_character_), NA_integer_)
  expect_identical(geo_n_coordinates("POINT (30 10)"), 1L)
  expect_identical(geo_n_coordinates("LINESTRING (0 0, 10 11)"), 2L)
  expect_identical(geo_n_coordinates("POLYGON EMPTY"), 0L)
})

test_that("geo_geometry_type works", {
  expect_identical(geo_geometry_type(NA_character_), NA_character_)
  expect_identical(geo_geometry_type("POINT (30 10)"), "point")
  expect_identical(geo_geometry_type("LINESTRING (0 0, 10 11)"), "linestring")
  expect_identical(geo_geometry_type("POLYGON EMPTY"), "polygon")
})

test_that("geo_summary components methods work", {
  x <- c(
    "POINT (30 10)",
    "LINESTRING (0 0, 10 11)",
    "POLYGON ((0 0, 0 12, 10 12, 10 0, 0 0))",
    "POLYGON EMPTY",
    NA
  )

  summary_manual <- tibble::tibble(
    geometry_type = geo_geometry_type(x),
    is_empty = geo_is_empty(x),
    n_coordinates = geo_n_coordinates(x),
    n_geometries =  geo_n_geometries(x),
    srid = geo_srid(x),
    coordinate_dimensions = geo_coordinate_dimensions(x),
    first_coordinate = geo_first_coordinate(x)
  )

  expect_identical(summary_manual, geo_summary(geo_wkt(x)))
})

test_that("geo_summary is identical for three generic geometry formats", {
  chr <- c(
    "POINT (30 10)",
    "LINESTRING (0 0, 10 11)",
    "POLYGON ((0 0, 0 12, 10 12, 10 0, 0 0))",
    "POLYGON EMPTY",
    NA
  )

  expect_identical(geo_summary(chr), geo_summary(geo_wkt(chr)))
  expect_identical(geo_summary(chr), geo_summary(as_geo_wkb(geo_wkt(chr))))
  expect_identical(geo_summary(chr), geo_summary(as_geo_collection((geo_wkt(chr)))))
})
