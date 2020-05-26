
test_that("geo_wkt class works", {
  wkt <- geo_wkt("POINT (30 10)")
  expect_identical(expect_output(print(wkt), "wk_wkt"), wkt)
  expect_identical(expect_output(print(geo_wkt()), "wk_wkt"), geo_wkt())
  expect_match(format(wkt), "POINT")
  expect_output(print(tibble(wkt)), "wkt")
  expect_is(wkt, "wk_wkt")
  expect_true(vec_is(wkt))
  expect_equal(vec_size(wkt), 1)
})

test_that("coersion and casting works for wkt types", {
  wkt <- geo_wkt("POINT (30 10)")

  expect_identical(vec_cast(wkt, geo_wkt()), wkt)
  expect_error(vec_cast(5, geo_wkt()), class = "vctrs_error_incompatible_type")

  expect_identical(as_wkt(wkt), wkt)
  expect_identical(as.character(wkt), "POINT (30 10)")

  wkb <- vec_cast(wkt, geo_wkb())
  wkt_roundtrip <- vec_cast(wkb, geo_wkt())
  expect_identical(wkt_roundtrip, wkt)

  expect_identical(vec_cast(wkt, geo_wkb()), wkb)
  expect_identical(as_wkt(wkb), wkt_roundtrip)

  expect_identical(
    vec_cast(geo_wkt("POINT (1 2)"), geo_xy()),
    geo_xy(1, 2)
  )

  expect_identical(
    vec_cast(geo_wkt("POINT Z (1 2 3)"), geo_xy()),
    geo_xyz(1, 2, 3)
  )

  expect_identical(
    vec_cast(geo_wkt("POINT Z (1 2 3)"), geo_xyz()),
    geo_xyz(1, 2, 3)
  )

  expect_identical(
    vec_cast(geo_segment(geo_xy(0, 1), geo_xy(10, 11)), geo_wkt()),
    as_wkt(geo_segment(geo_xy(0, 1), geo_xy(10, 11)), geo_wkt())
  )

  expect_identical(
    vec_cast(geo_rect(0, 1, 10, 11), geo_wkt()),
    as_wkt(geo_rect(0, 1, 10, 11), geo_wkt())
  )

  expect_identical(
    vec_cast(geo_point(geo_xy(1, 2)), geo_wkt()),
    as_wkt(geo_point(geo_xy(1, 2)), geo_wkt())
  )

  expect_identical(
    vec_cast(geo_point(geo_xyz(1, 2, 3)), geo_wkt()),
    as_wkt(geo_point(geo_xyz(1, 2, 3)), geo_wkt())
  )
})
