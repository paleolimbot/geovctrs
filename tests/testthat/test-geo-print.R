
test_that("geo_format works", {
  # all the examples from
  # https://en.wikipedia.org/wiki/Well-known_text_representation_of_geometry

  wkts <- geo_example_wkt

  formatted_long <- geo_format(wkts, short = FALSE, col = FALSE)
  formatted_short <- geo_format(wkts, short = TRUE, col = FALSE)

  expect_match(formatted_long[is.na(wkts)], "^NA_wkt_$")
  expect_match(formatted_long[!is.na(wkts) & geo_is_empty(wkts)], "EMPTY")
  expect_match(formatted_short[!is.na(wkts) & geo_is_empty(wkts)], "EMPTY")
  expect_match(formatted_long[!is.na(wkts) & geo_geometry_type(wkts) == "point"], "POINT")

  expect_output(geo_print(as_geo_wkb(as_geo_wkt(wkts))), "POINT")

  expect_output(print(tibble(geom = wkts)), "tibble")
  expect_output(print(tibble(geom = as_geo_wkb(wkts))), "tibble")
  expect_output(print(tibble(geom = as_geo_collection(wkts))), "tibble")
})
