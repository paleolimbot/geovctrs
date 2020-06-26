
test_that("set_z works", {
  expect_identical(geo_set_z("POINT Z (2 3 10)", 4), wkt("POINT Z (2 3 4)"))
  expect_identical(geo_set_z("POINT (2 3)", 4), wkt("POINT Z (2 3 4)"))

  expect_identical(geo_set_z(geo_xyz(2, 3, 10), 4), geo_xyz(2, 3, 4))
  expect_identical(geo_set_z(geo_xy(2, 3), 4), geo_xyz(2, 3, 4))

  expect_identical(geo_z_range(geo_example_wkt), geo_lim(5, 5))
  example_z <- geo_set_z(geo_example_wkt, 10)
  expect_identical(example_z[1], NA_wkt_)
  expect_true(all(wk::wkt_meta(example_z[!geo_is_empty(example_z)])$has_z))
  expect_identical(geo_z_range(example_z[!geo_is_empty(example_z)]), geo_lim(10, 10))
  expect_identical(geo_is_empty(example_z), geo_is_empty(geo_example_wkt))
})

test_that("set_z is vectorized along x and z", {
  expect_identical(
    geo_set_z(wkt("POINT Z (10 20 30)"), 4:6),
    wkt(c("POINT Z (10 20 4)", "POINT Z (10 20 5)", "POINT Z (10 20 6)"))
  )

  expect_identical(
    geo_set_z(wkt("POINT Z (10 20 30)"), double()),
    wkt()
  )

  expect_identical(
    geo_set_z(wkt(), 1),
    wkt()
  )
})

test_that("drop_z works", {
  expect_identical(geo_drop_z("POINT Z (2 3 10)"), wkt("POINT (2 3)"))
  expect_identical(geo_drop_z("POINT (2 3)"), wkt("POINT (2 3)"))

  expect_identical(geo_drop_z(geo_xyz(2, 3, 10)), geo_xy(2, 3))
  expect_identical(geo_drop_z(geo_xy(2, 3)), geo_xy(2, 3))

  example_z <- geo_drop_z(geo_example_wkt)
  expect_identical(example_z[1], NA_wkt_)
  expect_true(all(!wk::wkt_meta(example_z[!geo_is_empty(example_z)])$has_z))
  expect_identical(geo_z_range(example_z), geo_lim(Inf, -Inf))
  expect_identical(geo_is_empty(example_z), geo_is_empty(geo_example_wkt))
})
