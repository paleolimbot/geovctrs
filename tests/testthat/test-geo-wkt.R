
test_that("geo_wkt class works", {
  wkt <- geo_wkt("POINT (30 10)")
  expect_output(print(wkt), "geo_wkt")
  expect_output(print(tibble(wkt)), "wkt")
  expect_is(wkt, "geo_wkt")
  expect_true(is_geo_wkt(wkt))
  expect_true(vec_is(wkt))
  expect_equal(vec_size(wkt), 1)
})

test_that("parse problems for WKT are detected", {
  expect_warning(
    expect_equivalent(
      parse_wkt(c("POINT EMPTY", "POINT EMTPY")),
      geo_wkt(c("POINT EMPTY", NA))
    ),
    "parsing failure"
  )

  expect_identical(
    is.na(
      cpp_validate_provider(
        new_geo_wkt(c("POINT (30 10)", "POINT EMPTY", "MERR", "POINT FISH"))
      )
    ),
    c(TRUE, TRUE, FALSE, FALSE)
  )

  expect_identical(
    validate_geo_wkt(new_geo_wkt("POINT (30 10)")),
    new_geo_wkt("POINT (30 10)")
  )

  expect_warning(
    expect_error(
      validate_geo_wkt(new_geo_wkt("POINT FISH")),
      class = "parse_error"
    ),
    "parsing failure"
  )

  expect_warning(
    expect_error(
      validate_geo_wkt(rep(new_geo_wkt("POINT FISH"), 21)),
      class = "parse_error"
    ),
    "parsing failures"
  )
})

test_that("subset assignment works for WKT class", {
  wkts <- geo_example_wkt

  wkts[2] <- geo_wkt("POINT (1234 4321)")
  expect_identical(wkts[2], geo_wkt("POINT (1234 4321)"))

  wkts[2] <- as_geo_wkb(geo_wkt("POINT (1000 1000)"))
  expect_identical(wkts[2], geo_wkt("POINT (1000 1000)"))
})

test_that("coersion and casting works for wkt types", {
  wkt <- geo_wkt("POINT (30 10)")

  expect_identical(vec_cast(wkt, geo_wkt()), wkt)
  expect_identical(vec_cast("POINT (30 10)", geo_wkt()), wkt)
  expect_identical(vec_cast(wkt, character()), "POINT (30 10)")
  expect_error(vec_cast(5, geo_wkt()), class = "vctrs_error_incompatible_cast")

  expect_identical(as_geo_wkt(wkt), wkt)
  expect_identical(as_geo_wkt("POINT (30 10)"), wkt)
  expect_identical(as.character(wkt), "POINT (30 10)")
  expect_error(as_geo_wkt(5), class = "vctrs_error_incompatible_cast")

  expect_warning(
    expect_error(as_geo_wkt("FISH"), class = "parse_error"),
    "parsing failure"
  )
  expect_warning(
    expect_error(vec_cast("FISH", geo_wkt()), class = "parse_error"),
    "parsing failure"
  )

  wkb <- vec_cast(wkt, geo_wkb())
  wkt_roundtrip <- vec_cast(wkb, geo_wkt())
  expect_identical(wkt_roundtrip, wkt)

  expect_identical(vec_cast(wkt, geo_wkb()), wkb)
  expect_identical(as_geo_wkt(wkb), wkt_roundtrip)

  expect_identical(
    vec_cast(geo_segment(geo_xy(0, 1), geo_xy(10, 11)), geo_wkt()),
    as_geo_wkt(geo_segment(geo_xy(0, 1), geo_xy(10, 11)), geo_wkt())
  )

  expect_identical(
    vec_cast(geo_rect(0, 1, 10, 11), geo_wkt()),
    as_geo_wkt(geo_rect(0, 1, 10, 11), geo_wkt())
  )

  expect_identical(
    vec_cast(geo_point(geo_xy(1, 2)), geo_wkt()),
    as_geo_wkt(geo_point(geo_xy(1, 2)), geo_wkt())
  )
})
