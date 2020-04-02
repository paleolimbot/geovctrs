
test_that("geo_xy class works", {
  xy <- geo_xy(0, 1)
  expect_output(print(xy), "geo_xy")
  expect_output(print(tibble(xy)), "xy")
  expect_is(xy, "geo_xy")
  expect_true(is_geo_xy(xy))
  expect_true(vec_is(xy))
})

test_that("geo_xy casting works", {
  xy <- geo_xy(0:5, 1:6)

  expect_equal(as.data.frame(xy), data.frame(x = 0:5, y = 1:6))
  expect_equal(tibble::as_tibble(xy), tibble(x = 0:5, y = 1:6))
  expect_identical(dim(as.matrix(xy)), c(6L, 2L))
})

test_that("coersion to xy works", {
  # self-cast
  expect_identical(vec_cast(geo_xy(), geo_xy()), geo_xy())
  expect_identical(as_geo_xy(geo_xy()), geo_xy())

  # error cast
  expect_error(vec_cast(394, geo_xy()), class = "vctrs_error_incompatible_cast")

  # wkt
  expect_identical(
    as_geo_xy(geo_wkt("POINT (30 10)")),
    geo_xy(30, 10)
  )

  expect_identical(
    vec_cast(geo_wkt("POINT (30 10)"), geo_xy()),
    geo_xy(30, 10)
  )

  # wkb
  expect_identical(
    as_geo_xy(as_geo_wkb(geo_wkt("POINT (30 10)"))),
    geo_xy(30, 10)
  )

  expect_identical(
    vec_cast(as_geo_wkb(geo_wkt("POINT (30 10)")), geo_xy()),
    geo_xy(30, 10)
  )

  # collection
  expect_identical(
    as_geo_xy(geo_point(geo_xy(30, 10))),
    geo_xy(30, 10)
  )

  expect_identical(
    as_geo_xy(geo_point(geo_xy(30, 10))),
    geo_xy(30, 10)
  )
})
