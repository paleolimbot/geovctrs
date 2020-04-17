
test_that("geo-coalesce works", {
  wkt <- geo_wkt(c("POINT (30 10)", "POINT EMPTY", NA))

  expect_identical(geo_coalesce(wkt), wkt)

  expect_identical(
    geo_coalesce(wkt, geo_wkt("POINT (20 20)")),
    geo_wkt(c("POINT (30 10)", "POINT (20 20)", "POINT (20 20)"))
  )

  expect_error(geo_coalesce(), "At least one value")
})
