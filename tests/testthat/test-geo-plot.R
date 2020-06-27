
context("test-geo-plot")

test_that("plot generics work", {
  expect_is(plot(geo_xy(1:5, 1:5), pch = 16, col = "red"), "geovctrs_xy")
  expect_is(plot(geo_segment(0, 5:1, 5, 5:1), lty = 2, col = "blue", add = TRUE), "geovctrs_segment")
  expect_is(plot(geo_rect(1:2, 3, 4, 5), col = rgb(0, 0, 0, 0.25), add = TRUE), "geovctrs_rect")
})
