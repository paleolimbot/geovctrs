
test_that("wkb casting and coersion works", {
  wkb_raw <- as.raw(
    c(
      0x01, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x3e, 0x40, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x24, 0x40
    )
  )

  wkb <- wkb(list(wkb_raw))
  expect_identical(vec_cast(wkb, wkb()), wkb)

  wkt <- vec_cast(wkb, wkt())
  wkb_roundtrip <- vec_cast(wkt, wkb())
  expect_identical(wkb, wkb_roundtrip)
  expect_identical(as_wkt(wkb), wkt)
  expect_identical(as_wkb(wkt), wkb)

  expect_identical(
    vec_cast(as_wkb(wkt("POINT (1 2)")), geo_xy()),
    geo_xy(1, 2)
  )

  expect_identical(
    vec_cast(as_wkb(wkt("POINT Z (1 2 3)")), geo_xy()),
    geo_xyz(1, 2, 3)
  )

  expect_identical(
    vec_cast(as_wkb(wkt("POINT Z (1 2 3)")), geo_xyz()),
    geo_xyz(1, 2, 3)
  )

  expect_identical(
    vec_cast(geo_segment(geo_xy(0, 1), geo_xy(10, 11)), wkb()),
    as_wkb(geo_segment(geo_xy(0, 1), geo_xy(10, 11)), wkb())
  )

  expect_identical(
    vec_cast(geo_rect(0, 1, 10, 11), wkb()),
    as_wkb(geo_rect(0, 1, 10, 11), wkb())
  )

  expect_identical(
    vec_cast(geo_point(geo_xy(1, 2)), wkb()),
    as_wkb(geo_point(geo_xy(1, 2)), wkb())
  )
})
