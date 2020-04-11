
test_that("geo_coordinates() works", {
  expect_identical(
    geo_coordinates("POINT EMPTY"),
    tibble(feature = integer(), xy = geo_xy())
  )

  expect_identical(
    geo_coordinates("POINT (30 10)"),
    tibble(feature = 1L, xy = geo_xy(30, 10))
  )

  expect_identical(
    geo_coordinates("LINESTRING EMPTY"),
    tibble(feature = integer(), xy = geo_xy())
  )

  expect_identical(
    geo_coordinates("LINESTRING (30 10, 0 0)"),
    tibble(feature = 1L, xy = geo_xy(c(30, 0), c(10, 0)))
  )

  expect_identical(
    geo_coordinates("POLYGON EMPTY"),
    tibble(feature = integer(), xy = geo_xy(), ring = integer())
  )

  expect_identical(
    geo_coordinates("POLYGON ((30 10, 0 0, -30 10, 30 10))"),
    tibble(feature = 1L, xy = geo_xy(c(30, 0, -30, 30), c(10, 0, 10, 10)), ring = 1L)
  )

  expect_named(
    geo_coordinates(
      geo_example_wkt[geo_geometry_type(geo_example_wkt) != "geometrycollection"]
    ),
    c("feature", "xy", "part", "ring")
  )

  expect_identical(
    geo_coordinates("GEOMETRYCOLLECTION EMPTY"),
    tibble(feature = integer(), xy = geo_xy())
  )

  expect_error(
    geo_coordinates("GEOMETRYCOLLECTION (POINT (30 10))"),
    "Can't return coordinates"
  )
})
