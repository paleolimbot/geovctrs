
test_that("geo_coalesce works", {
  wkt <- geo_wkt(c("POINT (30 10)", "POINT EMPTY", NA))

  expect_identical(geo_coalesce(wkt), wkt)

  expect_identical(
    geo_coalesce(wkt, geo_wkt("POINT (20 20)")),
    geo_wkt(c("POINT (30 10)", "POINT (20 20)", "POINT (20 20)"))
  )

  expect_identical(
    geo_coalesce(tibble(wkt), geo_wkt("LINESTRING EMPTY")),
    tibble(wkt = geo_wkt(c("POINT (30 10)", "LINESTRING EMPTY", "LINESTRING EMPTY")))
  )
})
