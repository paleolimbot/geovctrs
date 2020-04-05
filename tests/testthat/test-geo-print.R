
test_that("geo_format works", {
  # all the examples from
  # https://en.wikipedia.org/wiki/Well-known_text_representation_of_geometry
  wkts <- c(
    NA,
    "POINT (30 10)",
    "POINT EMPTY",
    "POINT Z (1 1 5)",
    "MULTIPOINT (10 40, 40 30, 20 20, 30 10)",
    "MULTIPOINT EMPTY",
    "POLYGON ((30 10, 40 40, 20 40, 10 20, 30 10))",
    "POLYGON ((35 10, 45 45, 15 40, 10 20, 35 10), (20 30, 35 35, 30 20, 20 30))",
    "POLYGON EMPTY",
    "MULTIPOINT ((10 40), (40 30), (20 20), (30 10))",
    "MULTIPOINT (10 40, 40 30, 20 20, 30 10)",
    "MULTIPOINT EMPTY",
    "MULTILINESTRING ((10 10, 20 20, 10 40), (40 40, 30 30, 40 20, 30 10))",
    "MULTILINESTRING EMPTY",
    "MULTIPOLYGON (((30 20, 45 40, 10 40, 30 20)), ((15 5, 40 10, 10 20, 5 10, 15 5)))",
    "MULTIPOLYGON (
      ((40 40, 20 45, 45 30, 40 40)), ((20 35, 10 30, 10 10, 30 5, 45 20, 20 35),
      (30 20, 20 15, 20 25, 30 20))
    )",
    "MULTIPOLYGON EMPTY"
    # "GEOMETRYCOLLECTION (
    #   POINT (40 10),
    #   LINESTRING (10 10, 20 20, 10 40),
    #   POLYGON ((40 40, 20 45, 45 30, 40 40))
    # )"
  )

  formatted_long <- geo_format(wkts, short = FALSE, col = FALSE)
  formatted_short <- geo_format(wkts, short = TRUE, col = FALSE)

  expect_match(formatted_long[is.na(wkts)], "^NA_wkt_$")
  expect_match(formatted_long[!is.na(wkts) & geo_is_empty(wkts)], "EMPTY")
  expect_match(formatted_short[!is.na(wkts) & geo_is_empty(wkts)], "EMPTY")
  expect_match(formatted_long[!is.na(wkts) & geo_geometry_type(wkts) == "point"], "POINT")

  expect_output(geo_print(as_geo_wkb(as_geo_wkt(wkts))), "POINT")
})
