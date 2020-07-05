
test_that("geo_transform() works", {
  t_trans <- matrix(c(1, 0, 0, 0, 1, 0, 12, 13, 1), ncol = 3)
  expect_identical(geo_transform("POINT (30 10)", t_trans), wkt("POINT (42 23)"))
  expect_identical(geo_transform(as_wkt("POINT (30 10)"), t_trans), wkt("POINT (42 23)"))
  expect_identical(geo_transform(as_wkb("POINT (30 10)"), t_trans), as_wkb("POINT (42 23)"))
  expect_identical(geo_transform(as_wksxp("POINT (30 10)"), t_trans), as_wksxp("POINT (42 23)"))
  expect_identical(geo_transform(geo_xy(30, 10), t_trans), geo_xy(42, 23))
  expect_identical(geo_transform(geo_segment(0, 0, 30, 10), t_trans), geo_segment(12, 13, 42, 23))

  expect_is(geo_transform(geo_nc, t_trans), "data.frame")
})
