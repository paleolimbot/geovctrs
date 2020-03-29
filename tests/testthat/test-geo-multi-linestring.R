
test_that("geo_linestring() works", {
  # empty
  expect_is(geo_multilinestring(geo_linestring(geo_xy())), "geo_collection")
  expect_length(geo_multilinestring(geo_linestring(geo_xy())), 1)

  #  length 2
  expect_is(geo_multilinestring(geo_linestring(geo_xy(10:11, 30:31))), "geo_collection")
  expect_is(
    field(geo_multilinestring(geo_linestring(geo_xy(10:11, 30:31))), "feature")[[1]],
    "geo_multilinestring"
  )
  expect_length(geo_multilinestring(geo_linestring(geo_xy(10:11, 30:31))), 1)
})
