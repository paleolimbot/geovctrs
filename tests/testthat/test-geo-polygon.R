
test_that("geo_polygon() works", {
  # empty
  expect_is(geo_polygon(geo_xy()), "geo_collection")
  expect_length(geo_polygon(geo_xy()), 1)

  # length 3
  expect_is(geo_polygon(geo_xy(c(0, 10, 0), c(0, 0, 10))), "geo_collection")
  expect_is(field(geo_polygon(geo_xy(c(0, 10, 0), c(0, 0, 10))), "feature")[[1]], "geo_polygon")
  expect_length(geo_polygon(geo_xy(c(0, 10, 0), c(0, 0, 10))), 1)

  # length 3 with hole
  poly_hole <- "POLYGON ((35 10, 45 45, 15 40, 10 20, 35 10), (20 30, 35 35, 30 20, 20 30))"
  poly_hole <- geo_polygon(
    geo_xy(
      c(35, 45, 15, 10, 35, 20, 35, 30, 20),
      c(10, 45, 40, 20, 10, 30, 35, 30, 20)
    ),
    ring = c(1, 1, 1, 1, 1, 2, 2, 2, 2)
  )

  #  length 2 should error
  expect_error(geo_polygon(geo_xy(10:11, 30:31)), "is not TRUE")

  # length 1 should error
  expect_error(geo_polygon(geo_xy(10, 30)), "is not TRUE")
})
