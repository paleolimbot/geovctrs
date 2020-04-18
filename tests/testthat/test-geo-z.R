
test_that("set_z works", {
  expect_identical(geo_set_z("POINT Z (2 3 10)", 4), geo_wkt("POINT Z (2 3 4)"))
  expect_identical(geo_set_z("POINT (2 3)", 4), geo_wkt("POINT Z (2 3 4)"))

  expect_identical(geo_set_z(geo_xyz(2, 3, 10), 4), geo_xyz(2, 3, 4))
  expect_identical(geo_set_z(geo_xy(2, 3), 4), geo_xyz(2, 3, 4))

  example_z <- geo_set_z(geo_example_wkt, 5)
  expect_identical(example_z[1], NA_wkt_)
  expect_true(all(geo_coordinate_dimensions(example_z[!geo_is_empty(example_z)]) == 3))
  expect_identical(geo_is_empty(example_z), geo_is_empty(geo_example_wkt))
})

test_that("drop_z works", {
  expect_identical(geo_drop_z("POINT Z (2 3 10)"), geo_wkt("POINT (2 3)"))
  expect_identical(geo_drop_z("POINT (2 3)"), geo_wkt("POINT (2 3)"))

  expect_identical(geo_drop_z(geo_xyz(2, 3, 10)), geo_xy(2, 3))
  expect_identical(geo_drop_z(geo_xy(2, 3)), geo_xy(2, 3))

  example_z <- geo_drop_z(geo_example_wkt)
  expect_identical(example_z[1], NA_wkt_)
  expect_true(all(geo_coordinate_dimensions(example_z[!geo_is_empty(example_z)]) == 2))
  expect_identical(geo_is_empty(example_z), geo_is_empty(geo_example_wkt))
})
