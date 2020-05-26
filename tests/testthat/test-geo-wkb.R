
test_that("geo_wkb class works", {
  wkb_raw <- as.raw(
    c(
      0x01, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x3e, 0x40, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x24, 0x40
    )
  )
  wkb <- geo_wkb(list(wkb_raw))
  expect_output(print(wkb), "wk_wkb")
  expect_match(format(wkb), "POINT")
  expect_output(print(tibble(wkb)), "wkb")
  expect_is(wkb, "wk_wkb")
  expect_true(vec_is(wkb))
  expect_equal(vec_size(wkb), 1)
})

test_that("c() works for wkb", {
  expect_is(c(geo_wkb(), geo_wkb()), "wk_wkb")
  expect_error(vec_c(geo_wkb(), 5), class = "vctrs_error_incompatible_type")
})

test_that("subset assignment works for WKB class", {
  wkbs <- as_wkb(geo_example_wkt)

  wkbs[2] <- as_wkb(geo_wkt("POINT (1000 1000)"))
  expect_identical(wkbs[2], as_wkb(geo_wkt("POINT (1000 1000)")))

  wkbs[2] <- geo_wkt("POINT (1234 4321)")
  expect_identical(wkbs[2], as_wkb(geo_wkt("POINT (1234 4321)")))
})

test_that("wkb casting and coersion works", {
  wkb_raw <- as.raw(
    c(
      0x01, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x3e, 0x40, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x24, 0x40
    )
  )

  wkb <- geo_wkb(list(wkb_raw))
  expect_identical(vec_cast(wkb, geo_wkb()), wkb)

  wkt <- vec_cast(wkb, geo_wkt())
  wkb_roundtrip <- vec_cast(wkt, geo_wkb())
  expect_identical(wkb, wkb_roundtrip)
  expect_identical(as_geo_wkt(wkb), wkt)
  expect_identical(as_wkb(wkt), wkb)

  expect_identical(
    vec_cast(as_wkb(geo_wkt("POINT (1 2)")), geo_xy()),
    geo_xy(1, 2)
  )

  expect_identical(
    vec_cast(as_wkb(geo_wkt("POINT Z (1 2 3)")), geo_xy()),
    geo_xyz(1, 2, 3)
  )

  expect_identical(
    vec_cast(as_wkb(geo_wkt("POINT Z (1 2 3)")), geo_xyz()),
    geo_xyz(1, 2, 3)
  )

  expect_identical(
    vec_cast(geo_segment(geo_xy(0, 1), geo_xy(10, 11)), geo_wkb()),
    as_wkb(geo_segment(geo_xy(0, 1), geo_xy(10, 11)), geo_wkb())
  )

  expect_identical(
    vec_cast(geo_rect(0, 1, 10, 11), geo_wkb()),
    as_wkb(geo_rect(0, 1, 10, 11), geo_wkb())
  )

  expect_identical(
    vec_cast(geo_point(geo_xy(1, 2)), geo_wkb()),
    as_wkb(geo_point(geo_xy(1, 2)), geo_wkb())
  )
})
