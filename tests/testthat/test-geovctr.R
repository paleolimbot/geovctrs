
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

test_that("data.frame works with as_geovctr()", {
  expect_geovctr(as_geovctr(geo_nc))
  expect_error(as_geovctr(tibble()), "Can't find geovctr")
  expect_error(as_geovctr(tibble(x = geo_wkt(), y = geo_wkt())), "More than one")
})
