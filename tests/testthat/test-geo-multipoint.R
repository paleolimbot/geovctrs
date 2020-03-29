
test_that("geo_multipoint() works", {
  # length 1
  expect_is(geo_multipoint(geo_xy(10, 30)), "geo_collection")
  expect_is(field(geo_multipoint(geo_xy(10, 30)), "feature")[[1]], "geo_multipoint")
  expect_length(geo_multipoint(geo_xy(10, 30)), 1)

  # empty
  expect_is(geo_multipoint(geo_xy()), "geo_collection")
  expect_length(geo_multipoint(geo_xy()), 1)

  #  length 2
  expect_is(geo_multipoint(geo_xy(10:11, 30:31)), "geo_collection")
  expect_length(geo_multipoint(geo_xy(10:11, 30:31)), 1)
})
