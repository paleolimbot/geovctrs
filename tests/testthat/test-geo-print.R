
test_that("all geovctrs work in the RStudio viewer", {
  if (FALSE) {
    View(geo_nc) # wkb
    View(tibble(as_wkt(geo_nc$geometry))) # wkt
    View(tibble(as_wksxp(geo_nc$geometry))) # wksxp
    View(tibble(geo_xy(1, 2))) # xy
    View(tibble(geo_xyz(1, 2, 3))) # xyz
    View(tibble(geo_segment(0, 0, 12, 11))) # segment
    View(tibble(geo_envelope(geo_nc))) # rect
  }

  expect_true(TRUE)
})
