
test_that("geo_xy class works", {
  xy <- geo_xy(0, 1)
  expect_output(print(xy), "geovctrs_xy")
  expect_output(print(tibble(xy)), "xy")
  expect_is(xy, "geovctrs_xy")
  expect_true(is_geovctrs_xy(xy))
  expect_true(vec_is(xy))
})

test_that("geo_xy casting works", {
  xy <- geo_xy(0:5, 1:6)

  expect_equal(as.data.frame(xy), data.frame(x = 0:5, y = 1:6))
  expect_equal(tibble::as_tibble(xy), tibble(x = 0:5, y = 1:6))

  mat <- as.matrix(xy)
  expect_identical(dim(mat), c(6L, 2L))
  expect_identical(as_geo_xy(mat), xy)
  # colnames matter!
  mat2 <- mat[, c("y", "x")]
  expect_identical(as_geo_xy(mat2), xy)
  # fallback on order
  colnames(mat2) <- NULL
  expect_identical(as_geo_xy(mat2), geo_xy(1:6, 0:5))
})

test_that("geo_xy c() works", {
  expect_is(c(geo_xy(), geo_xy()), "geovctrs_xy")
  expect_error(c(geo_xy(), 5), class = "vctrs_error_incompatible_type")
})

test_that("coersion to xy works", {
  # cast to- and from xy
  expect_identical(vec_cast(geo_xyz(), geo_xy()), geo_xy())
  expect_identical(as_geo_xy(geo_xyz()), geo_xy())
  expect_identical(vec_cast(geo_xy(), geo_xyz()), geo_xyz())
  expect_identical(as_geo_xyz(geo_xy()), geo_xyz())

  expect_identical(vec_cast(geo_xyz(1, 2, NA), geo_xy()), geo_xy(1, 2))
  expect_identical(as_geo_xy(geo_xyz(1, 2, NA)), geo_xy(1, 2))
  expect_identical(vec_cast(geo_xy(1, 2), geo_xyz()), geo_xyz(1, 2, NA))
  expect_identical(as_geo_xyz(geo_xy(1, 2)), geo_xyz(1, 2, NA))

  # self-cast
  expect_identical(vec_cast(geo_xy(), geo_xy()), geo_xy())
  expect_identical(as_geo_xy(geo_xy()), geo_xy())

  # error cast
  expect_error(vec_cast(394, geo_xy()), class = "vctrs_error_incompatible_type")

  # wkt
  expect_identical(
    as_geo_xy(wkt("POINT (30 10)")),
    geo_xy(30, 10)
  )

  expect_identical(
    vec_cast(wkt("POINT (30 10)"), geo_xy()),
    geo_xy(30, 10)
  )

  # wkb
  expect_identical(
    as_geo_xy(as_wkb(wkt("POINT (30 10)"))),
    geo_xy(30, 10)
  )

  expect_identical(
    vec_cast(as_wkb(wkt("POINT (30 10)")), geo_xy()),
    geo_xy(30, 10)
  )

  # wksxp
  expect_identical(
    as_geo_xy(as_wksxp(wkt("POINT (30 10)"))),
    geo_xy(30, 10)
  )

  expect_identical(
    vec_cast(as_wksxp(wkt("POINT (30 10)")), geo_xy()),
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
