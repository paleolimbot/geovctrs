
test_that("all geovctrs pass expect_geovctr", {
  expect_geovctr(geo_wkt())
  expect_geovctr(geo_wkb())
  expect_geovctr(geo_collection())
  expect_geovctr(geo_xy())
  expect_geovctr(geo_segment())
  expect_geovctr(geo_rect())
})

test_that("character works with as_geovctr()", {
  expect_geovctr(as_geovctr("POINT (0 0)"))
})
