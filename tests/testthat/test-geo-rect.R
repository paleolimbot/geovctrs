
test_that("geo_rect class works", {
  rect <- geo_rect(xmin = 0, ymin = 0, xmax = 1, ymax = 1)
  expect_output(print(rect), "geovctrs_rect")
  expect_output(print(tibble(rect)), "rect")
  expect_is(rect, "geovctrs_rect")
  expect_true(is_geovctrs_rect(rect))
  expect_true(vec_is(rect))
})


test_that("geo_rect c() works", {
  rect <- geo_rect(xmin = 0:5, ymin = 0:5, xmax = 1:6, ymax = 1:6)
  expect_is(c(rect, wkt("POINT (30 10)")), "wk_wkt")
  expect_is(c(rect, as_wkb(wkt("POINT (30 10)"))), "wk_wkb")
  expect_is(c(rect, rect), "geovctrs_rect")
  expect_error(vec_c(5, rect), class = "vctrs_error_incompatible_type")
})

test_that("geo_rect casting works", {
  rect <- geo_rect(xmin = 0:5, ymin = 0:5, xmax = 1:6, ymax = 1:6)

  expect_equal(
    as.data.frame(rect),
    data.frame(xmin = 0:5,  ymin = 0:5, xmax = 1:6, ymax = 1:6, srid = NA_integer_)
  )
  expect_equal(
    tibble::as_tibble(rect),
    tibble(xmin = 0:5,  ymin = 0:5, xmax = 1:6, ymax = 1:6, srid = NA_integer_)
  )
})

test_that("coersion to rect works", {
  # self-cast
  expect_identical(vec_cast(geo_rect(), geo_rect()), geo_rect())
  expect_identical(as_geo_rect(geo_rect()), geo_rect())

  # error cast
  expect_error(vec_cast(394, geo_rect()), class = "vctrs_error_incompatible_type")
})
