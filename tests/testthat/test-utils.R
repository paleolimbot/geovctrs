
test_that("SRID summariser works", {
  expect_warning(summarise_srids(1:2), "SRID")
  expect_silent(summarise_srids(1))
  expect_silent(summarise_srids(integer()))
})
