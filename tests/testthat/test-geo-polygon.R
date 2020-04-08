
test_that("geo_polygon() works", {
  # empty
  expect_is(geo_polygon(geo_xy()), "geovctrs_collection")
  expect_length(geo_polygon(geo_xy()), 1)

  # length 3
  expect_is(geo_polygon(geo_xy(c(0, 10, 0), c(0, 0, 10))), "geovctrs_collection")
  expect_is(field(geo_polygon(geo_xy(c(0, 10, 0), c(0, 0, 10))), "feature")[[1]], "geo_polygon")
  expect_length(geo_polygon(geo_xy(c(0, 10, 0), c(0, 0, 10))), 1)

  # length 3 with hole
  poly_hole <- geo_polygon(
    geo_xy(
      c(35, 45, 15, 10, 35, 20, 35, 30, 20),
      c(10, 45, 40, 20, 10, 30, 35, 30, 20)
    ),
    ring = c(1, 1, 1, 1, 1, 2, 2, 2, 2)
  )
  expect_is(poly_hole, "geovctrs_collection")
  expect_length(poly_hole, 1)
  expect_is(field(poly_hole, "feature")[[1]], "geo_polygon")

  #  length 2 should error
  expect_error(geo_polygon(geo_xy(10:11, 30:31)), "is not TRUE")

  # length 1 should error
  expect_error(geo_polygon(geo_xy(10, 30)), "is not TRUE")

  # valid with ring that is too small should error
  expect_error(
    geo_polygon(
      geo_xy(c(0, 10, 0, 1), c(0, 0, 10, 1)),
      ring = c(1, 1, 1, 2)
    ),
    "is not TRUE"
  )

  # output
  expect_output(print(geo_polygon(geo_xy())), "POLYGON")
  expect_output(print(geo_polygon(geo_xy(c(0, 10, 0, 0), c(0, 0, 10, 0)))), "POLYGON")
})
