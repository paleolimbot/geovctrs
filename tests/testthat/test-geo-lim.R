
test_that("geo_lim class works", {
  lim <- geo_lim(0, 1)
  expect_output(print(lim), "geovctrs_lim")
  expect_output(print(tibble(lim)), "lim")
  expect_is(lim, "geovctrs_lim")
  expect_true(is_geovctrs_lim(lim))
  expect_true(vec_is(lim))
})


test_that("geo_lim casting works", {
  lim <- geo_lim(0:5, 1:6)

  expect_equal(as.data.frame(lim), data.frame(lower = 0:5, upper = 1:6))
  expect_equal(tibble::as_tibble(lim), tibble(lower = 0:5, upper = 1:6))

  mat <- as.matrix(lim)
  expect_identical(dim(mat), c(6L, 2L))
  expect_identical(as_geo_lim(mat), lim)
  # colnames matter!
  mat2 <- mat[, c("upper", "lower")]
  expect_identical(as_geo_lim(mat2), lim)
  # fallback on order
  colnames(mat2) <- NULL
  expect_identical(as_geo_lim(mat2), geo_lim(1:6, 0:5))
})

test_that("geo_lim c() works", {
  expect_is(c(geo_lim(), geo_lim()), "geovctrs_lim")
  expect_error(c(geo_lim(), 5), class = "vctrs_error_incompatible_type")
})
