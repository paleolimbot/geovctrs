
test_that("coersion and casting works for wkt types", {
  wkt <- wkt("POINT (30 10)")

  expect_identical(vec_cast(wkt, wkt()), wkt)
  expect_error(vec_cast(5, wkt()), class = "vctrs_error_incompatible_type")

  expect_identical(as_wkt(wkt), wkt)
  expect_identical(as.character(wkt), "POINT (30 10)")

  wkb <- vec_cast(wkt, wkb())
  wkt_roundtrip <- vec_cast(wkb, wkt())
  expect_identical(wkt_roundtrip, wkt)

  expect_identical(vec_cast(wkt, wkb()), wkb)
  expect_identical(as_wkt(wkb), wkt_roundtrip)

  expect_identical(
    vec_cast(wkt("POINT (1 2)"), geo_xy()),
    geo_xy(1, 2)
  )

  expect_identical(
    vec_cast(wkt("POINT Z (1 2 3)"), geo_xy()),
    geo_xyz(1, 2, 3)
  )

  expect_identical(
    vec_cast(wkt("POINT Z (1 2 3)"), geo_xyz()),
    geo_xyz(1, 2, 3)
  )

  expect_identical(
    vec_cast(geo_segment(geo_xy(0, 1), geo_xy(10, 11)), wkt()),
    as_wkt(geo_segment(geo_xy(0, 1), geo_xy(10, 11)), wkt())
  )

  expect_identical(
    vec_cast(geo_rect(0, 1, 10, 11), wkt()),
    as_wkt(geo_rect(0, 1, 10, 11), wkt())
  )

  expect_identical(
    vec_cast(geo_point(geo_xy(1, 2)), wkt()),
    as_wkt(geo_point(geo_xy(1, 2)), wkt())
  )

  expect_identical(
    vec_cast(geo_point(geo_xyz(1, 2, 3)), wkt()),
    as_wkt(geo_point(geo_xyz(1, 2, 3)), wkt())
  )
})
