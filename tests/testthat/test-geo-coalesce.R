
test_that("geo_coalesce works", {
  wkt <- wkt(c("POINT (30 10)", "POINT EMPTY", NA))

  expect_identical(geo_coalesce(wkt), wkt)

  expect_identical(
    geo_coalesce(wkt, wkt("POINT (20 20)")),
    wkt(c("POINT (30 10)", "POINT (20 20)", "POINT (20 20)"))
  )

  expect_identical(
    geo_coalesce(tibble(wkt), wkt("LINESTRING EMPTY")),
    tibble(wkt = wkt(c("POINT (30 10)", "LINESTRING EMPTY", "LINESTRING EMPTY")))
  )
})
