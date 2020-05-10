
test_that("geo_xyz class works", {
  xy <- geo_xyz(0, 1, 2)
  expect_output(print(xy), "geovctrs_xyz")
  expect_output(print(tibble(xy)), "xyz")
  expect_is(xy, "geovctrs_xyz")
  expect_is(xy, "geovctrs_xy")
  expect_true(is_geovctrs_xyz(xy))
  expect_true(is_geovctrs_xy(xy))
  expect_true(vec_is(xy))
})

test_that("geo_xyz casting works", {
  xy <- geo_xyz(0:5, 1:6, z =  2:7)

  expect_equal(as.data.frame(xy), data.frame(x = 0:5, y = 1:6, z = 2:7))
  expect_equal(tibble::as_tibble(xy), tibble(x = 0:5, y = 1:6, z = 2:7))

  mat <- as.matrix(xy)
  expect_identical(dim(mat), c(6L, 3L))
  expect_identical(as_geo_xyz(mat), xy)
  # colnames matter!
  mat2 <- mat[, c("y", "x", "z")]
  expect_identical(as_geo_xyz(mat2), xy)
  # fallback on order
  colnames(mat2) <- NULL
  expect_identical(as_geo_xyz(mat2), geo_xyz(1:6, 0:5, z = 2:7))
})

test_that("geo_xyz c() works", {
  expect_is(c(geo_xyz(), geo_xyz()), "geovctrs_xyz")
  expect_is(c(geo_xy(), geo_xyz()), "geovctrs_xyz")
  expect_is(c(geo_xyz(), geo_xy()), "geovctrs_xyz")
  expect_error(c(geo_xyz(), 5), class = "vctrs_error_incompatible_type")
})

test_that("coersion to xy works", {
  # self-cast
  expect_identical(vec_cast(geo_xyz(), geo_xyz()), geo_xyz())
  expect_identical(as_geo_xyz(geo_xyz()), geo_xyz())

  # cast to- and from xy
  expect_identical(vec_cast(geo_xyz(), geo_xy()), geo_xy())
  expect_identical(as_geo_xy(geo_xyz()), geo_xy())
  expect_identical(vec_cast(geo_xy(), geo_xyz()), geo_xyz())
  expect_identical(as_geo_xyz(geo_xy()), geo_xyz())

  expect_identical(vec_cast(geo_xyz(1, 2, NA), geo_xy()), geo_xy(1, 2))
  expect_identical(as_geo_xy(geo_xyz(1, 2, NA)), geo_xy(1, 2))
  expect_identical(vec_cast(geo_xy(1, 2), geo_xyz()), geo_xyz(1, 2, NA))
  expect_identical(as_geo_xyz(geo_xy(1, 2)), geo_xyz(1, 2, NA))

  # error cast
  expect_error(vec_cast(394, geo_xyz()), class = "vctrs_error_incompatible_cast")
  expect_error(vec_cast(geo_xyz(1, 2, 3), geo_xy()), class = "vctrs_error_cast_lossy")

  # wkt
  expect_identical(
    as_geo_xyz(geo_wkt("POINT Z (30 10 20)")),
    geo_xyz(30, 10, 20)
  )

  expect_identical(
    vec_cast(geo_wkt("POINT  Z (30 10 20)"), geo_xyz()),
    geo_xyz(30, 10, 20)
  )

  # wkb
  expect_identical(
    as_geo_xyz(as_geo_wkb(geo_wkt("POINT Z (30 10 20)"))),
    geo_xyz(30, 10, 20)
  )

  expect_identical(
    vec_cast(as_geo_wkb(geo_wkt("POINT Z (30 10 20)")), geo_xyz()),
    geo_xyz(30, 10, 20)
  )

  # collection
  expect_identical(
    as_geo_xyz(geo_point(geo_xyz(30, 10, 20))),
    geo_xyz(30, 10, 20)
  )
})
