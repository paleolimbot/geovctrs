
test_that("geo_multipoolygon() works", {
  # empty
  expect_is(geo_multipolygon(geo_polygon(geo_xy())), "geo_collection")
  expect_length(geo_multipolygon(geo_polygon(geo_xy())), 1)

  geo_multipolygon(geo_polygon(geo_xy(c(0, 10, 0), c(0, 0, 10))))

  #  length 3
  expect_is(geo_multipolygon(geo_polygon(geo_xy(c(0, 10, 0), c(0, 0, 10)))), "geo_collection")
  expect_is(
    field(geo_multipolygon(geo_polygon(geo_xy(c(0, 10, 0), c(0, 0, 10)))), "feature")[[1]],
    "geo_multipolygon"
  )
  expect_length(geo_multipolygon(geo_polygon(geo_xy(c(0, 10, 0), c(0, 0, 10)))), 1)
})
