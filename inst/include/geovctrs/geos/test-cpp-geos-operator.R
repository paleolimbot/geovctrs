
test_that("test buffer operator fails without segfault", {
  # working op
  expect_length(geovctrs_cpp_test_buffer2(wkt("POINT (0 0)"), wkt()), 1)

  # these have bad C++ sytax and should give a helpful error
  # without segfaulting
  expect_error(
    geovctrs_cpp_test_buffer2_bad_provider(wkt("POINT (0 0)"), wkt()),
    "initProvider"
  )

  expect_error(
    geovctrs_cpp_test_buffer2_bad_exporter(wkt("POINT (0 0)"), wkt()),
    "initExporter"
  )
})

test_that("recursive operator recreates geometries perfectly by default", {
  expect_identical(
    geovctrs_cpp_test_recursive_identity(geo_example_wkt, wkb()),
    as_wkb(geo_example_wkt)
  )
})

test_that("recycling a constant provider won't segfault from deleting the geometry", {
  skip("don't test providers")

  expect_identical(
    geovctrs_cpp_set_z(wkt("POINT Z (10 20 30)"), 4:6),
    wkt(c("POINT Z (10 20 4)", "POINT Z (10 20 5)", "POINT Z (10 20 6)"))
  )
})
