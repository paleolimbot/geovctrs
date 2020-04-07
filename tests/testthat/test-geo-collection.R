
test_that("geo_collection class works", {
  collection <- geo_collection(list(geo_linestring(geo_xy(c(0, 1, 6), c(0, 2, 4)))))
  expect_output(print(collection), "geo_collection")
  expect_output(print(tibble(collection)), "clctn")
  expect_is(collection, "geo_collection")
  expect_true(is_geo_collection(collection))
  expect_true(vec_is(collection))

  # output with  nested collection
  expect_output(print(geo_collection(list(collection))), "GEOMETRYCOLLECTION")

  # create a nested collection
  expect_identical(
    geo_collection(list(collection)),
    geo_collection(collection)
  )
})
