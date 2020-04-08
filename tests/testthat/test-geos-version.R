
test_that("geos_version works", {
  expect_length(geos_version(), 1)
  expect_match(as.character(geos_version()), "3\\.[0-9]{1,2}\\.[0-9]{1,2}")
  expect_match(as.character(geos_capi_version()), "[0-9]{1,2}\\.[0-9]{1,2}\\.[0-9]{1,2}")
})

test_that("GEOS errors are caught and rethrown", {
  # geometry operator
  expect_error(
    cpp_convert(new_geovctrs_wkt("NOT"), geo_collection()),
    "ParseException",
    class = "Rcpp::exception"
  )

  # no vector operator subclasses with which to test this yet
})
