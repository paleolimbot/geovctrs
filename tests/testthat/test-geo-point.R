
test_that("geo_point() works", {
  # length 1
  expect_is(geo_point(geo_xy(10, 30)), "geo_collection")
  expect_is(field(geo_point(geo_xy(10,  30)), "feature")[[1]], "geo_point")
  expect_length(geo_point(geo_xy(10, 30)), 1)

  # empty
  expect_is(geo_point(geo_xy()), "geo_collection")
  expect_length(geo_point(geo_xy()), 1)

  # error on length 2
  expect_error(geo_point(geo_xy(10:11, 30:31)), "is not TRUE")

  # formatting
  expect_output(print(geo_point(geo_xy())), "POINT")
  expect_output(print(geo_point(geo_xy(30, 10))), "POINT")
})
