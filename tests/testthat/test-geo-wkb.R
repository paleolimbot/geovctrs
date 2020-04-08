
test_that("geo_wkb class works", {
  wkb_raw <- as.raw(
    c(
      0x01, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x3e, 0x40, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x24, 0x40
    )
  )
  wkb <- geo_wkb(list(wkb_raw))
  expect_output(print(wkb), "geovctrs_wkb")
  expect_match(format(wkb), "POINT")
  expect_output(print(tibble(wkb)), "wkb")
  expect_is(wkb, "geovctrs_wkb")
  expect_true(is_geovctrs_wkb(wkb))
  expect_true(vec_is(wkb))
  expect_equal(vec_size(wkb), 1)
})

test_that("geo_wkb parse validation works", {
  wkb_raw <- as.raw(
    c(
      0x01, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x3e, 0x40, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x24, 0x40
    )
  )

  # scrambled a bit
  wkb_bad <- as.raw(
    c(
      0xFF, 0xE9, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x044, 0x3e, 0x40, 0x28, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x24, 0x40, 0xFF, 0xF2
    )
  )

  wkb <- new_geovctrs_wkb(list(wkb_raw, wkb_bad))
  expect_warning(
    expect_equivalent(
      parse_wkb(list(wkb_raw, wkb_bad)),
      geo_wkb(list(wkb_raw, NULL))
    ),
    "parsing failure"
  )
  expect_identical(is.na(cpp_validate_provider(wkb)), c(TRUE, FALSE))

  expect_identical(validate_geovctrs_wkb(wkb[1]), wkb[1])
  expect_warning(
    expect_error(validate_geovctrs_wkb(wkb), "1 geometry", class = "parse_error"),
    "parsing failure"
  )
})

test_that("c() works for wkb", {
  expect_is(c(geo_wkb(), geo_wkb()), "geovctrs_wkb")
  expect_error(vec_c(geo_wkb(), 5), class = "vctrs_error_incompatible_type")
})

test_that("subset assignment works for WKB class", {
  wkbs <- as_geo_wkb(geo_example_wkt)

  wkbs[2] <- as_geo_wkb(geo_wkt("POINT (1000 1000)"))
  expect_identical(wkbs[2], as_geo_wkb(geo_wkt("POINT (1000 1000)")))

  wkbs[2] <- geo_wkt("POINT (1234 4321)")
  expect_identical(wkbs[2], as_geo_wkb(geo_wkt("POINT (1234 4321)")))
})

test_that("wkb casting and coersion works", {
  wkb_raw <- as.raw(
    c(
      0x01, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x3e, 0x40, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x24, 0x40
    )
  )

  # scrambled a bit
  wkb_bad <- as.raw(
    c(
      0xFF, 0xE9, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x044, 0x3e, 0x40, 0x28, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x24, 0x40, 0xFF, 0xF2
    )
  )

  wkb <- geo_wkb(list(wkb_raw))

  expect_identical(vec_cast(wkb, geo_wkb()), wkb)
  expect_identical(vec_cast(list(wkb_raw), geo_wkb()), wkb)

  expect_identical(vec_cast(list(wkb_raw), geo_wkb()), as_geo_wkb(wkb))
  expect_identical(vec_cast(wkb, list())[[1]], unclass(wkb)[[1]])
  expect_identical(as_geo_wkb(list(wkb_raw)), wkb)

  expect_warning(
    expect_error(as_geo_wkb(list(wkb_bad)), class = "parse_error"),
    "parsing failure"
  )
  expect_warning(
    expect_error(vec_cast(list(wkb_bad), geo_wkb()), class = "parse_error"),
    "parsing failure"
  )

  expect_error(as_geo_wkb(5), class = "vctrs_error_incompatible_cast")

  wkt <- vec_cast(wkb, geo_wkt())
  wkb_roundtrip <- vec_cast(wkt, geo_wkb())
  expect_identical(wkb, wkb_roundtrip)
  expect_identical(as_geo_wkt(wkb), wkt)
  expect_identical(as_geo_wkb(wkt), wkb)

  expect_identical(
    vec_cast(geo_segment(geo_xy(0, 1), geo_xy(10, 11)), geo_wkb()),
    as_geo_wkb(geo_segment(geo_xy(0, 1), geo_xy(10, 11)), geo_wkb())
  )

  expect_identical(
    vec_cast(geo_rect(0, 1, 10, 11), geo_wkb()),
    as_geo_wkb(geo_rect(0, 1, 10, 11), geo_wkb())
  )

  expect_identical(
    vec_cast(geo_point(geo_xy(1, 2)), geo_wkb()),
    as_geo_wkb(geo_point(geo_xy(1, 2)), geo_wkb())
  )

  expect_identical(
    as_geo_wkb("POINT Z (21 23 13)"),
    as_geo_wkb(geo_wkt("POINT Z (21 23 13)"))
  )
})

test_that("casting and coercion respects options", {
  wkb <- as_geo_wkb(geo_wkt("POINT Z (1 2 3)"))
  expect_identical(geo_coordinate_dimensions(as_geo_wkb(wkb, dimensions = 3)), 3L)
  expect_identical(geo_coordinate_dimensions(as_geo_wkb(wkb, dimensions = 2)), 2L)
  expect_identical(as.list(as_geo_wkb(wkb, endian = 0))[[1]][1], as.raw(0x00))
})

test_that("geo_wkb can handle empty (multi)points", {
  expect_identical(as_geo_wkt(as_geo_wkb(geo_wkt("POINT EMPTY"))), geo_wkt("POINT EMPTY"))
  expect_identical(as_geo_wkt(as_geo_wkb(geo_wkt("POINT (nan nan)"))), geo_wkt("POINT EMPTY"))
  expect_identical(
    as_geo_wkt(as_geo_wkb(geo_wkt("MULTIPOINT EMPTY"))),
    geo_wkt("MULTIPOINT EMPTY")
  )
  # "MULTIPOINT (nan nan)" currently cannot be written to WKB
  # expect_identical(
  #   as_geo_wkt(as_geo_wkb(geo_wkt("MULTIPOINT (nan nan)"))),
  #   geo_wkt("MULTIPOINT EMPTY")
  # )
  expect_identical(as_geo_xy(as_geo_wkb(geo_xy(NA, NA))), geo_xy(NA, NA))
  expect_identical(as_geo_collection(as_geo_wkb(geo_point(geo_xy()))), geo_point(geo_xy()))
})
