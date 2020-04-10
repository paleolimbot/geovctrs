
test_that("geos_version works", {
  expect_length(geos_version_runtime(), 1)
  expect_length(geos_version_build(), 1)
  expect_match(as.character(geos_version_runtime()), "3\\.[0-9]{1,2}\\.[0-9]{1,2}")
  expect_match(as.character(geos_version_build()), "3\\.[0-9]{1,2}\\.[0-9]{1,2}")
})
