
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

  # default method
  expect_output(expect_identical(geo_print(geo_nc), geo_nc), "tbl_df...geovctrs_wkb")

  # named
  expect_output(geo_print(setNames(geo_wkt("POINT (30 10)"), "a name")), "a name")

  # zero len format +  print
  expect_length(geo_format(geo_wkt()), 0)
  expect_output(geo_print(geo_wkt()), "geovctrs_wkt\\[0\\]")
})

test_that("printing works without unicode/colour support", {
  withr::with_options(list(crayon.enabled = FALSE, cli.unicode =  FALSE), {
    expect_output(geo_print(geo_nc, col = TRUE), "MULTIPOLYGON")
    expect_output(geo_print(geo_nc, short = TRUE), "mply")
    expect_output(print(geo_rect(1, 2, 3, 4)), "...")
    expect_output(print(geo_segment(geo_xy(1, 2), geo_xy(3, 4))), "---")
  })
})

test_that("geo_format() and geo_print() don't error for non-parsable geometries", {
  expect_warning(geo_format(new_geovctrs_wkt("POINT ENTPY")), "1 geometry failed to parse")
  expect_output(
    expect_warning(geo_print(new_geovctrs_wkt("POINT ENTPY")), "1 geometry failed to parse"),
    "ParseException"
  )
  expect_output(
    expect_warning(
      print(tibble(new_geovctrs_wkt("POINT ENTPY"))),
      "1 geometry failed to parse"
    ),
    "ParseException"
  )
})

test_that("all geovctrs work in the RStudio viewer", {
  if (FALSE) {
    View(geo_nc) # wkb
    View(tibble(as_geo_wkt(geo_nc$geometry))) # wkt
    View(tibble(as_geo_collection(geo_nc$geometry))) # collection
    View(geo_summary(geo_nc)) # xy
    View(tibble(geo_segment(geo_xy(0, 0), geo_xy(12, 11)))) # segment
    View(tibble(geo_envelope(geo_nc))) # rect
  }

  expect_true(TRUE)
})
