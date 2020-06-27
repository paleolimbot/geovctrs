
test_that("geo_geometry_type() works", {
  expect_identical(geo_geometry_type("POINT EMPTY"), "point")
  expect_identical(geo_geometry_type(wkt("POINT EMPTY")), "point")
  expect_identical(geo_geometry_type(as_wkb("POINT (1 2)")), "point")
  expect_identical(geo_geometry_type(as_wksxp("POINT EMPTY")), "point")

  expect_identical(geo_geometry_type(geo_xy(1, 2)), "point")
  expect_identical(geo_geometry_type(geo_segment(1, 2,  3, 4)), "linestring")
  expect_identical(geo_geometry_type(geo_rect(1, 2, 3, 4)), "polygon")
})
