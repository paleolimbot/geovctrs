
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

test_that("geo_range() works", {

  # mostly making sure the na.rm and finite args were propogated
  expect_identical(geo_range(1:5), geo_lim(1, 5))
  expect_identical(geo_range(c(1, NA)), geo_lim(NA, NA))
  expect_identical(geo_range(c(1, NA), na.rm = TRUE), geo_lim(1, 1))
  expect_identical(geo_range(c(1, Inf)), geo_lim(1, Inf))
  expect_identical(geo_range(c(1, Inf), finite = TRUE), geo_lim(1, 1))

  expect_identical(geo_range(c(geo_lim(81, Inf), geo_lim(-100, 12))), geo_lim(-100, Inf))
  expect_identical(
    geo_range(c(geo_lim(81, Inf), geo_lim(-100, 12)), finite = TRUE),
    geo_lim(-100, 81)
  )
  expect_identical(
    geo_range(c(geo_lim(81, NA), geo_lim(-100, 12)), na.rm = TRUE),
    geo_lim(-100, 81)
  )
})
