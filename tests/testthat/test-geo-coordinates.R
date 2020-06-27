
test_that("geo_coordinates() works", {
  expect_identical(
    geo_coordinates("POINT EMPTY"),
    tibble(feature = integer(), part = integer(), ring = integer(), xy = geo_xy())
  )

  expect_identical(
    geo_coordinates("POINT (30 10)"),
    tibble(feature = 1L, part = 1L, ring = 0L, xy = geo_xy(30, 10))
  )

  expect_identical(
    geo_coordinates("LINESTRING EMPTY"),
    tibble(feature = integer(), part = integer(), ring = integer(), xy = geo_xy())
  )

  expect_identical(
    geo_coordinates("LINESTRING (30 10, 0 0)"),
    tibble(feature = 1L, part = c(1L, 1L), ring = c(0L, 0L), xy = geo_xy(c(30, 0), c(10, 0)))
  )

  expect_identical(
    geo_coordinates("POLYGON EMPTY"),
    tibble(feature = integer(), part = integer(), ring = integer(), xy = geo_xy())
  )

  expect_identical(
    geo_coordinates("POLYGON ((30 10, 0 0, -30 10, 30 10))"),
    tibble(feature = 1L, part = 1L, ring = 1L,  xy = geo_xy(c(30, 0, -30, 30), c(10, 0, 10, 10)))
  )

  expect_named(
    geo_coordinates(geo_example_wkt),
    c("feature", "part", "ring", "xy")
  )

  expect_identical(
    geo_coordinates("GEOMETRYCOLLECTION EMPTY"),
    tibble(feature = integer(), part = integer(), ring = integer(), xy = geo_xy())
  )

  expect_identical(
    geo_coordinates("GEOMETRYCOLLECTION (POINT (30 10))"),
    tibble(feature = 1L, part = 2L, ring = 0L, xy = geo_xy(30, 10))
  )
})

test_that("geo_coordinates() works with other formats", {
  expect_identical(
    geo_coordinates(as_wkb("POINT (30 10)")),
    tibble(feature = 1L, part = 1L, ring = 0L, xy = geo_xy(30, 10))
  )

  expect_identical(
    geo_coordinates(as_wksxp("POINT (30 10)")),
    tibble(feature = 1L, part = 1L, ring = 0L, xy = geo_xy(30, 10))
  )

  expect_identical(
    geo_coordinates(geo_xy(30, 10)),
    tibble(feature = 1L, part = 1L, ring = 0L, xy = geo_xy(30, 10))
  )

  expect_identical(
    geo_coordinates(geo_segment(30, 10, 0, 1)),
    tibble(
      feature = 1L,
      part = 1L,
      ring = 0L,
      xy = c(geo_xy(30, 10), geo_xy(0, 1))
    )
  )

  expect_identical(
    geo_coordinates(geo_rect(0, 0, 1, 1)),
    tibble(
      feature = 1L,
      part = 1L,
      ring = 1L,
      xy = geo_xy(c(0, 1, 1, 0, 0), c(0, 0, 1, 1, 0))
    )
  )
})

test_that("geo_coordinates() works with the z coordinate", {
  expect_identical(geo_coordinates("POINT Z (1 2 3)")$xy, geo_xyz(1, 2, 3))
  expect_error(geo_coordinates("POINT M (1 2 3)")$xy, "doesn't support the 'm' coordinate")
})
