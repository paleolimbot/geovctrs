
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
})