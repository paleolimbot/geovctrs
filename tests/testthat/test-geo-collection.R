
test_that("geo_collection class works", {
  collection <- geo_collection(list(geo_linestring(geo_xy(c(0, 1, 6), c(0, 2, 4)))))
  expect_output(print(collection), "geovctrs_collection")
  expect_output(print(tibble(collection)), "clctn")
  expect_is(collection, "geovctrs_collection")
  expect_true(is_geo_collection(collection))
  expect_true(vec_is(collection))

  # output with nested collection
  expect_output(print(geo_collection(list(collection))), "GEOMETRYCOLLECTION")
  expect_match(format(geo_collection(list(collection))), "GEOMETRYCOLLECTION")

  # create a nested collection
  expect_identical(
    geo_collection(list(collection)),
    geo_collection(collection)
  )
})

test_that("basic casting and coersion work", {
  expect_identical(vec_cast(geo_collection(), geo_wkt()), geo_wkt())
  expect_identical(c(geo_collection(), geo_collection()), geo_collection())
  expect_identical(c(geo_collection(), geo_wkt()), geo_wkt())

  # erroring
  expect_error(vec_cast(5, geo_collection()), class = "vctrs_error_incompatible_cast")
  expect_error(vec_c(geo_collection(), 5), class = "vctrs_error_incompatible_type")
})

test_that("geo_point() works", {
  # length 1
  expect_is(geo_point(geo_xy(10, 30)), "geovctrs_collection")
  expect_is(field(geo_point(geo_xy(10,  30)), "feature")[[1]], "geo_point")
  expect_length(geo_point(geo_xy(10, 30)), 1)

  # empty
  expect_is(geo_point(geo_xy()), "geovctrs_collection")
  expect_length(geo_point(geo_xy()), 1)

  # error on length 2
  expect_error(geo_point(geo_xy(10:11, 30:31)), "is not TRUE")

  # formatting
  expect_output(print(geo_point(geo_xy())), "POINT")
  expect_output(print(geo_point(geo_xy(30, 10))), "POINT")
})

test_that("geo_linestring() works", {
  # empty
  expect_is(geo_linestring(geo_xy()), "geovctrs_collection")
  expect_length(geo_linestring(geo_xy()), 1)

  #  length 2
  expect_is(geo_linestring(geo_xy(10:11, 30:31)), "geovctrs_collection")
  expect_is(field(geo_linestring(geo_xy(10:11, 30:31)), "feature")[[1]], "geo_linestring")
  expect_length(geo_linestring(geo_xy(10:11, 30:31)), 1)

  # length 1 should error
  expect_error(geo_linestring(geo_xy(10, 30)), "is not TRUE")

  # formatting
  expect_output(print(geo_linestring(geo_xy())), "LINESTRING")
  expect_output(print(geo_linestring(geo_xy(10:11, 30:31))), "LINESTRING")
})

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

test_that("geo_multipoint() works", {
  # length 1
  expect_is(geo_multipoint(geo_point(geo_xy(10, 30))), "geovctrs_collection")
  expect_is(field(geo_multipoint(geo_point(geo_xy(10, 30))), "feature")[[1]], "geo_multipoint")
  expect_length(geo_multipoint(geo_point(geo_xy(10, 30))), 1)

  # empty
  expect_is(geo_multipoint(geo_point(geo_xy())), "geovctrs_collection")
  expect_length(geo_multipoint(geo_point(geo_xy())), 1)

  # error
  expect_error(geo_multipoint(geo_linestring(geo_xy())), "All features must be")
})

test_that("geo_multilinestring() works", {
  # empty
  expect_is(geo_multilinestring(geo_linestring(geo_xy())), "geovctrs_collection")
  expect_length(geo_multilinestring(geo_linestring(geo_xy())), 1)

  #  length 2
  expect_is(geo_multilinestring(geo_linestring(geo_xy(10:11, 30:31))), "geovctrs_collection")
  expect_is(
    field(geo_multilinestring(geo_linestring(geo_xy(10:11, 30:31))), "feature")[[1]],
    "geo_multilinestring"
  )
  expect_length(geo_multilinestring(geo_linestring(geo_xy(10:11, 30:31))), 1)

  # erroring
  expect_error(geo_multilinestring(geo_point(geo_xy())), "All features must be")
})

test_that("geo_multipolygon() works", {
  # empty
  expect_is(geo_multipolygon(geo_polygon(geo_xy())), "geovctrs_collection")
  expect_length(geo_multipolygon(geo_polygon(geo_xy())), 1)

  geo_multipolygon(geo_polygon(geo_xy(c(0, 10, 0), c(0, 0, 10))))

  #  length 3
  expect_is(geo_multipolygon(geo_polygon(geo_xy(c(0, 10, 0), c(0, 0, 10)))), "geovctrs_collection")
  expect_is(
    field(geo_multipolygon(geo_polygon(geo_xy(c(0, 10, 0), c(0, 0, 10)))), "feature")[[1]],
    "geo_multipolygon"
  )
  expect_length(geo_multipolygon(geo_polygon(geo_xy(c(0, 10, 0), c(0, 0, 10)))), 1)

  # error
  expect_error(geo_multipolygon(geo_point(geo_xy())), "All features must be")
})
