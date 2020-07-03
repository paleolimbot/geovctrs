
test_that("unnest_collection() works", {
  expect_identical(geo_unnest_collection(NA_character_), wksxp(list(NULL)))
  expect_identical(geo_unnest_collection("GEOMETRYCOLLECTION EMPTY"), wksxp())
  expect_identical(
    geo_unnest_collection("GEOMETRYCOLLECTION EMPTY", keep_empty = TRUE),
    as_wksxp("GEOMETRYCOLLECTION EMPTY")
  )

  expect_identical(
    geo_unnest_collection("GEOMETRYCOLLECTION (POINT (1 2), POINT (3 4))"),
    as_wksxp(c("POINT (1 2)", "POINT (3 4)"))
  )

  expect_identical(
    geo_unnest_collection("SRID=1234;GEOMETRYCOLLECTION (POINT Z (1 2 3), POINT (3 4))"),
    as_wksxp(c("SRID=1234;POINT Z (1 2 3)", "SRID=1234;POINT (3 4)"))
  )
})

test_that("unnest_all() works", {
  expect_identical(geo_unnest_all(NA_character_), wkt(NA_character_))
  expect_identical(
    geo_unnest_all(
      "GEOMETRYCOLLECTION(MULTIPOINT (30 10, 10 10), LINESTRING (0 0, 1 1), GEOMETRYCOLLECTION EMPTY)"
    ),
    wkt(c("POINT (30 10)", "POINT (10 10)", "LINESTRING (0 0, 1 1)"))
  )

  expect_identical(
    geo_unnest_all(
      "GEOMETRYCOLLECTION(MULTIPOINT (30 10, 10 10), LINESTRING (0 0, 1 1), GEOMETRYCOLLECTION EMPTY)",
      keep_empty = TRUE
    ),
    wkt(c("POINT (30 10)", "POINT (10 10)", "LINESTRING (0 0, 1 1)", "GEOMETRYCOLLECTION EMPTY"))
  )

  expect_identical(
    geo_unnest_all(
      "SRID=12;GEOMETRYCOLLECTION(MULTIPOINT (30 10, 10 10), LINESTRING (0 0, 1 1), GEOMETRYCOLLECTION EMPTY)"
    ),
    wkt(c("SRID=12;POINT (30 10)", "SRID=12;POINT (10 10)", "SRID=12;LINESTRING (0 0, 1 1)"))
  )

  expect_identical(
    geo_unnest_all(
      as_wkb("GEOMETRYCOLLECTION(MULTIPOINT (30 10, 10 10), LINESTRING (0 0, 1 1), GEOMETRYCOLLECTION EMPTY)")
    ),
    as_wkb(c("POINT (30 10)", "POINT (10 10)", "LINESTRING (0 0, 1 1)"))
  )

  expect_identical(
    geo_unnest_all(
      as_wksxp("GEOMETRYCOLLECTION(MULTIPOINT (30 10, 10 10), LINESTRING (0 0, 1 1), GEOMETRYCOLLECTION EMPTY)")
    ),
    as_wksxp(c("POINT (30 10)", "POINT (10 10)", "LINESTRING (0 0, 1 1)"))
  )
})
