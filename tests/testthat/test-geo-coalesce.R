
test_that("geo_coalesce works", {
  wkt <- wkt(c("POINT (30 10)", "LINESTRING EMPTY", NA))

  expect_identical(geo_coalesce(wkt), wkt)

  expect_identical(
    geo_coalesce(wkt, wkt("POINT (20 20)")),
    wkt(c("POINT (30 10)", "POINT (20 20)", "POINT (20 20)"))
  )

  expect_identical(
    geo_coalesce(as_wksxp(wkt), wkt("POINT (20 20)")),
    as_wksxp(c("POINT (30 10)", "POINT (20 20)", "POINT (20 20)"))
  )

  expect_identical(
    geo_coalesce(tibble(wkt), wkt("LINESTRING EMPTY")),
    tibble(wkt = wkt(c("POINT (30 10)", "LINESTRING EMPTY", "LINESTRING EMPTY")))
  )

  expect_identical(
    geo_coalesce(as_wkb(wkt), wkt("POINT (20 20)")),
    as_wkb(c("POINT (30 10)", "POINT (20 20)", "POINT (20 20)"))
  )

  expect_equal(
    geo_coalesce(geo_xy(c(1, NA), c(2, NA)), wkt("POINT (11 12)")),
    geo_xy(c(1, 11), c(2, 12))
  )

  expect_equal(
    geo_coalesce(
      geo_segment(c(1, NA), c(2, NA), c(1, NA), c(2, NA)), geo_segment(11, 12, 13, 14)
    ),
    geo_segment(c(1, 11), c(2, 12), c(1, 13), c(2, 14))
  )

  expect_equal(
    geo_coalesce(
      geo_rect(c(1, NA), c(2, NA), c(1, NA), c(2, NA)), geo_rect(11, 12, 13, 14)
    ),
    geo_rect(c(1, 11), c(2, 12), c(1, 13), c(2, 14))
  )
})
