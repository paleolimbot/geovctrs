
test_that("geo_linestring() works", {
  # empty
  expect_is(geo_linestring(geo_xy()), "geo_collection")
  expect_length(geo_linestring(geo_xy()), 1)

  #  length 2
  expect_is(geo_linestring(geo_xy(10:11, 30:31)), "geo_collection")
  expect_is(field(geo_linestring(geo_xy(10:11, 30:31)), "feature")[[1]], "geo_linestring")
  expect_length(geo_linestring(geo_xy(10:11, 30:31)), 1)

  # length 1 should error
  expect_error(geo_linestring(geo_xy(10, 30)), "is not TRUE")
})
