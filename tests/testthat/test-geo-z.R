
test_that("set_z works", {
  wkt <- geo_wkt("POINT (2 3 5)")
  expect_identical(geo_set_z(wkt, 4), geo_wkt("POINT (2 3 4)"))

  geo_set_z(geo_example_wkt, 5)
})
