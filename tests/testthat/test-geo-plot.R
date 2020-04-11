
context("test-geo-plot")

test_that("geo plotting works", {
  vdiffr::expect_doppelganger(
    "points",
    function() geo_plot(geo_wkt("POINT (10 40)"))
  )

  vdiffr::expect_doppelganger(
    "linestring",
    function() geo_plot(geo_wkt("LINESTRING (30 10, 10 30, 40 40)"))
  )

  vdiffr::expect_doppelganger(
    "multipoint",
    function() geo_plot(geo_wkt("MULTIPOINT ((10 40), (40 30))"))
  )

  vdiffr::expect_doppelganger(
    "multilinestring",
    function() geo_plot(geo_wkt("MULTILINESTRING ((10 10, 20 20, 10 40), (40 40, 30 30, 40 20, 30 10))"))
  )

  vdiffr::expect_doppelganger(
    "polygon",
    function() geo_plot(geo_wkt("POLYGON ((30 10, 10 30, 40 40, 30 10))"))
  )


  vdiffr::expect_doppelganger(
    "multipolygon",
    function() geo_plot(
      geo_wkt(
        "MULTIPOLYGON (((40 40, 20 45, 45 30, 40 40)),
         ((20 35, 10 30, 10 10, 30 5, 45 20, 20 35), (30 20, 20 15, 20 25, 30 20)))"
      ),
      col = "grey90"
    )
  )

  vdiffr::expect_doppelganger(
    "example WKTs",
    function() geo_plot(geo_example_wkt)
  )
})

test_that("limiting with bbox works", {
  vdiffr::expect_doppelganger(
    "NC limited by bbox",
    function() geo_plot(geo_nc, bbox = geo_rect(-82, 35, -76, 36))
  )
})

test_that("plot generics work", {
  vdiffr::expect_doppelganger(
    "geo_wkt generic",
    function() plot(geo_wkt("POINT (30 40)"))
  )

  vdiffr::expect_doppelganger(
    "geo_collection generic",
    function() plot(as_geo_collection(geo_wkt("POINT (30 40)")))
  )

  vdiffr::expect_doppelganger(
    "geo_wkb generic",
    function() plot(as_geo_wkb(geo_wkt("POINT (30 40)")))
  )

  vdiffr::expect_doppelganger(
    "geo_xy generic",
    function() plot(as_geo_xy(geo_wkt("POINT (30 40)")))
  )

  vdiffr::expect_doppelganger(
    "geo_segment generic",
    function() plot(geo_segment(geo_xy(0, 0), geo_xy(10, -10:10)))
  )

  vdiffr::expect_doppelganger(
    "geo_rect generic",
    function() plot(geo_rect(0:5, 1:6, 10:15, 11:16))
  )

  # should work with character via geo_plot_add.default
  vdiffr::expect_doppelganger(
    "geo_plot_add.default",
    function() geo_plot("POINT (30 40)")
  )

  vdiffr::expect_doppelganger(
    "geo_plot_add.data.frame",
    function() geo_plot(tibble(geo_wkt("POINT (30 40)")))
  )
})

test_that("geo_nc can be plotted", {
  # simple grey transformer
  make_col <- function(x, lim = range(x, na.rm = TRUE)) {
    width <- diff(lim)
    rescaled <- (x - lim[1]) / width
    rgb(rescaled, rescaled, rescaled)
  }

  vdiffr::expect_doppelganger(
    "basic plot of NC",
    function() geo_plot(geo_nc, col = make_col(BIR79))
  )
})

test_that("separate with na works", {
  expect_identical(
    separate_groups_with_na(numeric(0), numeric(0)),
    numeric(0)
  )

  expect_equal(
    separate_groups_with_na(1:5, c(1, 1, 2, 2, 2)),
    c(1, 2, NA, 3, 4, 5)
  )

  expect_equal(
    separate_groups_with_na(1:5, factor(c(1, 1, 2, 2, 2))),
    c(1, 2, NA, 3, 4, 5)
  )
})
