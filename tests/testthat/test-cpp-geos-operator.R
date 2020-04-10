
test_that("test buffer operator fails without segfault", {
  # working op
  expect_length(geovctrs_cpp_test_buffer2(geo_wkt("POINT (0 0)"), geo_wkt()), 1)

  # these have bad C++ sytax and should give a helpful error
  # without segfaulting
  expect_error(
    geovctrs_cpp_test_buffer2_bad_provider(geo_wkt("POINT (0 0)"), geo_wkt()),
    "initProvider"
  )
  expect_error(
    geovctrs_cpp_test_buffer2_bad_exporter(geo_wkt("POINT (0 0)"), geo_wkt()),
    "initExporter"
  )
})
