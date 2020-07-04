
test_that("geo_unnest() works", {
  expect_identical(geo_unnest(NA_character_), wkt(NA_character_))
  expect_identical(
    geo_unnest(
      "GEOMETRYCOLLECTION(MULTIPOINT (30 10, 10 10), LINESTRING (0 0, 1 1), GEOMETRYCOLLECTION EMPTY)",
      keep_multi = FALSE, keep_empty = FALSE, max_depth = 2
    ),
    wkt(c("POINT (30 10)", "POINT (10 10)", "LINESTRING (0 0, 1 1)"))
  )

  expect_identical(
    geo_unnest(
      "GEOMETRYCOLLECTION(MULTIPOINT (30 10, 10 10), LINESTRING (0 0, 1 1), GEOMETRYCOLLECTION EMPTY)",
      keep_multi = FALSE, keep_empty = TRUE, max_depth = 2
    ),
    wkt(c("POINT (30 10)", "POINT (10 10)", "LINESTRING (0 0, 1 1)", "GEOMETRYCOLLECTION EMPTY"))
  )

  expect_identical(
    geo_unnest(
      "GEOMETRYCOLLECTION(MULTIPOINT (30 10, 10 10), LINESTRING (0 0, 1 1), GEOMETRYCOLLECTION EMPTY)",
      keep_multi = FALSE, keep_empty = TRUE, max_depth = 2
    ),
    wkt(c("POINT (30 10)", "POINT (10 10)", "LINESTRING (0 0, 1 1)", "GEOMETRYCOLLECTION EMPTY"))
  )

  expect_identical(
    geo_unnest(
      "SRID=12;GEOMETRYCOLLECTION(MULTIPOINT (30 10, 10 10), LINESTRING (0 0, 1 1), GEOMETRYCOLLECTION EMPTY)",
      keep_multi = FALSE, max_depth = 2
    ),
    wkt(c("SRID=12;POINT (30 10)", "SRID=12;POINT (10 10)", "SRID=12;LINESTRING (0 0, 1 1)"))
  )

  expect_identical(
    geo_unnest(
      as_wkb("GEOMETRYCOLLECTION(MULTIPOINT (30 10, 10 10), LINESTRING (0 0, 1 1), GEOMETRYCOLLECTION EMPTY)"),
      keep_multi = TRUE, max_depth = 2, keep_empty = FALSE
    ),
    as_wkb(c("MULTIPOINT ((30 10), (10 10))", "LINESTRING (0 0, 1 1)"))
  )

  expect_identical(
    geo_unnest(
      as_wkb("GEOMETRYCOLLECTION(MULTIPOINT (30 10, 10 10), LINESTRING (0 0, 1 1), GEOMETRYCOLLECTION EMPTY)"),
      keep_multi = TRUE, max_depth = 2, keep_empty = TRUE
    ),
    as_wkb(c("MULTIPOINT ((30 10), (10 10))", "LINESTRING (0 0, 1 1)", "GEOMETRYCOLLECTION EMPTY"))
  )

  expect_identical(
    geo_unnest(
      as_wkb("GEOMETRYCOLLECTION(MULTIPOINT (30 10, 10 10), LINESTRING (0 0, 1 1), GEOMETRYCOLLECTION EMPTY)"),
      keep_multi = FALSE, max_depth = 2, keep_empty = FALSE
    ),
    as_wkb(c("POINT (30 10)", "POINT (10 10)", "LINESTRING (0 0, 1 1)"))
  )

  expect_identical(
    geo_unnest(
      as_wksxp("GEOMETRYCOLLECTION(MULTIPOINT (30 10, 10 10), LINESTRING (0 0, 1 1), GEOMETRYCOLLECTION EMPTY)"),
      keep_multi = TRUE, max_depth = 2, keep_empty = TRUE
    ),
    as_wksxp(c("MULTIPOINT ((30 10), (10 10))", "LINESTRING (0 0, 1 1)", "GEOMETRYCOLLECTION EMPTY"))
  )

  expect_identical(
    geo_unnest(
      as_wksxp("GEOMETRYCOLLECTION(MULTIPOINT (30 10, 10 10), LINESTRING (0 0, 1 1), GEOMETRYCOLLECTION EMPTY)"),
      keep_multi = FALSE, max_depth = 2, keep_empty = FALSE
    ),
    as_wksxp(c("POINT (30 10)", "POINT (10 10)", "LINESTRING (0 0, 1 1)"))
  )
})

test_that("geo_unnest(max_depth) is respected", {
  expect_identical(
    geo_unnest(
      "GEOMETRYCOLLECTION (GEOMETRYCOLLECTION (GEOMETRYCOLLECTION (POINT (0 1))))",
      max_depth = 0
    ),
    wkt("GEOMETRYCOLLECTION (GEOMETRYCOLLECTION (GEOMETRYCOLLECTION (POINT (0 1))))")
  )

  expect_identical(
    geo_unnest(
      "GEOMETRYCOLLECTION (GEOMETRYCOLLECTION (GEOMETRYCOLLECTION (POINT (0 1))))",
      max_depth = 1
    ),
    wkt("GEOMETRYCOLLECTION (GEOMETRYCOLLECTION (POINT (0 1)))")
  )

  expect_identical(
    geo_unnest(
      "GEOMETRYCOLLECTION (GEOMETRYCOLLECTION (GEOMETRYCOLLECTION (POINT (0 1))))",
      max_depth = 2
    ),
    wkt("GEOMETRYCOLLECTION (POINT (0 1))")
  )

  expect_identical(
    geo_unnest(
      "GEOMETRYCOLLECTION (GEOMETRYCOLLECTION (GEOMETRYCOLLECTION (POINT (0 1))))",
      max_depth = 3
    ),
    wkt("POINT (0 1)")
  )

  expect_identical(
    geo_unnest(
      "SRID=21;GEOMETRYCOLLECTION (GEOMETRYCOLLECTION (GEOMETRYCOLLECTION (POINT (0 1))))",
      max_depth = 3
    ),
    wkt("SRID=21;POINT (0 1)")
  )
})

test_that("unnesting works on data frames", {
  expect_identical(geo_unnest(geo_nc[integer(0), ]), geo_nc[integer(0), ])

  nc_unnested <- geo_nc[c("NAME", "geometry")] %>% geo_unnest(keep_multi = F)
  nc_sizes <- wk::wkb_meta(geo_nc$geometry)$size
  unnested_rle <- rle(nc_unnested$NAME)
  expect_identical(unnested_rle$lengths, nc_sizes)
})
