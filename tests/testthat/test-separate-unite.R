
test_that("unite_xy() works", {
  tbl <- tibble(x = 1, y = 2)
  expect_identical(
    unite_xy(tbl, "xy", x, y, remove = TRUE),
    tibble(xy = geo_xy(1, 2))
  )

  expect_identical(
    unite_xy(tbl, "xy", x, y, remove = FALSE),
    tibble(xy = geo_xy(1, 2), x = 1, y = 2)
  )

  tbl2 <- tibble(a = "unrelated", x = 1, y = 2)
  expect_identical(
    unite_xy(tbl2, "xy", x, y, remove = TRUE),
    tibble(a = "unrelated", xy = geo_xy(1, 2))
  )

  expect_identical(
    unite_xy(tbl2, "xy", x, y, remove = FALSE),
    tibble(a = "unrelated", xy = geo_xy(1, 2), x = 1, y = 2)
  )
})

test_that("unite_xyz() works", {
  tbl <- tibble(x = 1, y = 2, z = 3)
  expect_identical(
    unite_xyz(tbl, "xy", x, y, z, remove = TRUE),
    tibble(xy = geo_xyz(1, 2, 3))
  )

  expect_identical(
    unite_xyz(tbl, "xy", x, y, z, remove = FALSE),
    tibble(xy = geo_xyz(1, 2, 3), x = 1, y = 2, z = 3)
  )

  tbl2 <- tibble(a = "unrelated", x = 1, y = 2, z = 3)
  expect_identical(
    unite_xyz(tbl2, "xy", x, y, z, remove = TRUE),
    tibble(a = "unrelated", xy = geo_xyz(1, 2, 3))
  )

  expect_identical(
    unite_xyz(tbl2, "xy", x, y, z, remove = FALSE),
    tibble(a = "unrelated", xy = geo_xyz(1, 2, 3), x = 1, y = 2, z = 3)
  )
})

test_that("separate_xy() works", {
  tbl <- tibble(xy = geo_xy(1, 2))
  expect_identical(separate_xy(tbl, xy, remove = TRUE), tibble(x = 1, y = 2))
  expect_identical(separate_xy(tbl, xy, remove = FALSE), tibble(x = 1, y = 2, xy = geo_xy(1, 2)))

  tbl2 <- tibble(a = "unrelated", xy = geo_xy(1, 2))
  expect_identical(
    separate_xy(tbl2, xy, remove = TRUE),
    tibble(a = "unrelated", x = 1, y = 2)
  )
  expect_identical(
    separate_xy(tbl2, xy, remove = FALSE),
    tibble(a = "unrelated", x = 1, y = 2, xy = geo_xy(1, 2))
  )
})

test_that("unite_segment() works", {
  tbl <- tibble(a = geo_xy(392, 191), b = geo_xy(102, 191))
  expect_identical(
    unite_segment(tbl, "s", a, b, remove = TRUE),
    tibble(s = geo_segment(geo_xy(392, 191), geo_xy(102, 191)))
  )

  expect_identical(
    unite_segment(tbl, "s", a, b, remove = FALSE),
    tibble(
      s = geo_segment(geo_xy(392, 191), geo_xy(102, 191)),
      a = geo_xy(392, 191), b = geo_xy(102, 191)
    )
  )
})

test_that("unite_rect() works", {
  tbl <- tibble(a = 10, b = 11, c = 12, d = 14)
  expect_identical(
    unite_rect(tbl, "rect", a, b, c, d, remove = TRUE),
    tibble(rect = geo_rect(10, 11, 12, 14))
  )
  expect_identical(
    unite_rect(tbl, "rect", a, b, c, d, remove = FALSE),
    tibble(rect = geo_rect(10, 11, 12, 14), a = 10, b = 11, c = 12, d = 14)
  )

  tbl2 <- tibble(z = "unrelated", a = 10, b = 11, c = 12, d = 14)
  expect_identical(
    unite_rect(tbl2, "rect", a, b, c, d, remove = TRUE),
    tibble(z = "unrelated", rect = geo_rect(10, 11, 12, 14))
  )
  expect_identical(
    unite_rect(tbl2, "rect", a, b, c, d, remove = FALSE),
    tibble(z = "unrelated", rect = geo_rect(10, 11, 12, 14), a = 10, b = 11, c = 12, d = 14)
  )
})

test_that("separate_rect() works", {
  tbl <- tibble(rect = geo_rect(0, 1, 2, 3))
  expect_identical(
    separate_rect(tbl, rect, remove = FALSE),
    tibble(xmin = 0, ymin = 1, xmax = 2, ymax = 3, rect = geo_rect(0, 1, 2, 3))
  )
  expect_identical(
    separate_rect(tbl, rect, remove = TRUE),
    tibble(xmin = 0, ymin = 1, xmax = 2, ymax = 3)
  )

  tbl2 <- tibble(a = "unrelated", rect = geo_rect(0, 1, 2, 3))
  expect_identical(
    separate_rect(tbl2, rect, remove = FALSE),
    tibble(a = "unrelated", xmin = 0, ymin = 1, xmax = 2, ymax = 3, rect = geo_rect(0, 1, 2, 3))
  )
  expect_identical(
    separate_rect(tbl2, rect, remove = TRUE),
    tibble(a = "unrelated", xmin = 0, ymin = 1, xmax = 2, ymax = 3)
  )
})

test_that("insert_column() works", {
  tbl <- tibble(a = 1:2, b = 3:4)

  expect_identical(
    insert_column(tbl, "fish", NA, 1, remove = TRUE),
    tibble(fish = c(NA, NA), b = 3:4)
  )

  expect_identical(
    insert_column(tbl, "fish", NA, 1, remove = FALSE),
    tibble(fish = c(NA, NA), a = 1:2, b = 3:4)
  )
})

test_that("insert_vector() works", {
  expect_identical(
    insert_vector(c("one", "two", "three"), "XXX", 1),
    c("XXX", "one", "two", "three")
  )

  expect_identical(
    insert_vector(c("one", "two", "three"), "XXX", 2),
    c("one", "XXX", "two", "three")
  )

  expect_identical(
    insert_vector(c("one", "two", "three"), "XXX", 4),
    c("one", "two", "three", "XXX")
  )

  expect_identical(
    insert_vector(c("one", "two", "three"), c("XXX1", "XXX2"), 1),
    c("XXX1", "XXX2", "one", "two", "three")
  )

  expect_identical(
    insert_vector(c("one", "two", "three"), c("XXX1", "XXX2"), 4),
    c("one", "two", "three", "XXX1", "XXX2")
  )
})
