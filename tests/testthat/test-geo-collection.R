
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
