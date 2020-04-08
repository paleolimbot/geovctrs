
test_that("geo_segment class works", {
  segment <- geo_segment(start = geo_xy(0, 0), end = geo_xy(1, 1))
  expect_output(print(segment), "geovctrs_segment")
  expect_output(print(tibble(segment)), "segment")
  expect_is(segment, "geovctrs_segment")
  expect_true(is_geo_segment(segment))
  expect_true(vec_is(segment))
})


test_that("geo_segment() c() works", {
  segment <- geo_segment(start = geo_xy(0:5, 0:5), end = geo_xy(1:6, 1:6))
  expect_is(c(segment, geo_wkt("POINT (30 10)")), "geovctrs_wkt")
  expect_is(c(segment, as_geo_wkb(geo_wkt("POINT (30 10)"))), "geovctrs_wkb")
  expect_is(c(segment, segment), "geovctrs_segment")
  expect_error(vec_c(5, segment), class = "vctrs_error_incompatible_type")
})

test_that("coersion from segment works", {
  segment <- geo_segment(start = geo_xy(0:5, 0:5), end = geo_xy(1:6, 1:6))

  expect_identical(
    tibble::as_tibble(segment),
    tibble(start = geo_xy(0:5, 0:5), end = geo_xy(1:6, 1:6), srid = rep(0L, 6))
  )

  expect_identical(
    as.data.frame(segment),
    as.data.frame(tibble(start = geo_xy(0:5, 0:5), end = geo_xy(1:6, 1:6), srid = rep(0L, 6)))
  )
})

test_that("coersion to segment works", {
  # self-cast
  expect_identical(vec_cast(geo_segment(), geo_segment()), geo_segment())

  # error cast
  expect_error(vec_cast(394, geo_segment()), class = "vctrs_error_incompatible_cast")

  # wkt
  expect_identical(
    as_geo_segment(geo_wkt("LINESTRING (10 10, 20 20)")),
    geo_segment(geo_xy(10, 10), geo_xy(20, 20))
  )

  expect_identical(
    vec_cast(geo_wkt("LINESTRING (10 10, 20 20)"), geo_segment()),
    geo_segment(geo_xy(10, 10), geo_xy(20, 20))
  )

  # wkb
  expect_identical(
    as_geo_segment(as_geo_wkb(geo_wkt("LINESTRING (10 10, 20 20)"))),
    geo_segment(geo_xy(10, 10), geo_xy(20, 20))
  )

  expect_identical(
    vec_cast(as_geo_wkb(geo_wkt("LINESTRING (10 10, 20 20)")), geo_segment()),
    geo_segment(geo_xy(10, 10), geo_xy(20, 20))
  )

  # collection
  expect_identical(
    as_geo_segment(geo_linestring(geo_xy(c(10, 20), c(10, 20)))),
    geo_segment(geo_xy(10, 10), geo_xy(20, 20))
  )

  expect_identical(
    vec_cast(geo_linestring(geo_xy(c(10, 20), c(10, 20))), geo_segment()),
    geo_segment(geo_xy(10, 10), geo_xy(20, 20))
  )
})
