
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

  expect_error(
    vec_cast(wkt("POINT Z (1 2 3)"), geo_xy()),
    class = "vctrs_error_cast_lossy"
  )

  expect_identical(
    vec_cast(wkt("POINT Z (1 2 3)"), geo_xyz()),
    geo_xyz(1, 2, 3)
  )

  expect_identical(
    vec_cast(geo_segment(0, 1, 10, 11), wkt()),
    as_wkt(geo_segment(0, 1, 10, 11), wkt())
  )

  expect_identical(
    vec_cast(geo_rect(0, 1, 10, 11), wkt()),
    as_wkt(geo_rect(0, 1, 10, 11), wkt())
  )
})


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

  expect_error(
    vec_cast(as_wkb(wkt("POINT Z (1 2 3)")), geo_xy()),
    class = "vctrs_error_cast_lossy"
  )

  expect_identical(
    vec_cast(as_wkb(wkt("POINT Z (1 2 3)")), geo_xyz()),
    geo_xyz(1, 2, 3)
  )

  expect_identical(
    vec_cast(geo_xy(c(0, NA), c(1, NA)), wkb()),
    as_wkb(c("POINT (0 1)", "POINT EMPTY"))
  )

  expect_identical(
    vec_cast(geo_xyz(c(0, NA), c(1, NA), c(2, NA)), wkb()),
    as_wkb(c("POINT Z (0 1 2)", "POINT Z EMPTY"))
  )

  expect_identical(
    vec_cast(geo_segment(0, 1, 10, 11), wkb()),
    as_wkb(geo_segment(0, 1, 10, 11))
  )

  expect_identical(
    vec_cast(geo_rect(0, 1, 10, 11), wkb()),
    as_wkb(geo_rect(0, 1, 10, 11))
  )
})


test_that("coersion and casting works for wksxp types", {
  wksxp <- as_wksxp("POINT (30 10)")

  expect_identical(vec_cast(wksxp, wksxp()), wksxp)
  expect_error(vec_cast(5, wksxp()), class = "vctrs_error_incompatible_type")

  expect_identical(as_wksxp(wksxp), wksxp)
  expect_identical(as.character(wksxp), "<POINT (30 10)>")

  wkb <- vec_cast(wksxp, wkb())
  wksxp_roundtrip <- vec_cast(wkb, wksxp())
  expect_identical(wksxp_roundtrip, wksxp)

  expect_identical(vec_cast(wksxp, wkb()), wkb)
  expect_identical(as_wksxp(wkb), wksxp_roundtrip)

  expect_identical(
    vec_cast(as_wksxp("POINT (1 2)"), geo_xy()),
    geo_xy(1, 2)
  )

  expect_error(
    vec_cast(as_wksxp("POINT Z (1 2 3)"), geo_xy()),
    class = "vctrs_error_cast_lossy"
  )

  expect_identical(
    vec_cast(as_wksxp("POINT Z (1 2 3)"), geo_xyz()),
    geo_xyz(1, 2, 3)
  )

  expect_identical(
    vec_cast(geo_xy(c(0, NA), c(1, NA)), wksxp()),
    as_wksxp(c("POINT (0 1)", "POINT EMPTY"))
  )

  expect_identical(
    vec_cast(geo_xyz(c(0, NA), c(1, NA), c(2, NA)), wksxp()),
    as_wksxp(c("POINT Z (0 1 2)", "POINT Z EMPTY"))
  )

  expect_identical(
    vec_cast(geo_segment(0, 1, 10, 11), wksxp()),
    as_wksxp(geo_segment(0, 1, 10, 11), wksxp())
  )

  expect_identical(
    vec_cast(geo_rect(0, 1, 10, 11), wksxp()),
    as_wksxp(geo_rect(0, 1, 10, 11), wksxp())
  )
})
