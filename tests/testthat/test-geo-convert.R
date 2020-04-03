
test_that("wkt conversion works", {
  wkt <- geo_wkt(c("POINT (30 10)", "POINT (20 20)"))
  wkt_roundtrip <- cpp_convert(wkt, new_geo_wkt())
  expect_is(wkt_roundtrip, "geo_wkt")
  expect_match(wkt_roundtrip, "^POINT")
  expect_match(wkt_roundtrip[1], c("\\(30\\."))
  expect_match(wkt_roundtrip[1], c("10\\.0+\\)"))
  expect_match(wkt_roundtrip[2], c("\\(20\\."))
})

test_that("wkb conversion works", {
  wkb_raw <- as.raw(
    c(
      0x01, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x3e, 0x40, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x24, 0x40
    )
  )

  wkb <- geo_wkb(list(wkb_raw))
  wkb_roundtrip <- cpp_convert(wkb, new_geo_wkb())
  expect_identical(wkb, wkb_roundtrip)
})

test_that("wkb conversion propogates SRIDs by default", {
  collection_empty <- geo_collection(list(geo_collection()), srid = 23)
  collection_wkb <- cpp_convert(collection_empty, geo_wkb())
  expect_identical(cpp_convert(collection_wkb, geo_collection()), collection_empty)
})

test_that("geo_point conversion works", {
  wkt_empty <- geo_wkt("POINT EMPTY")
  collection_empty <- geo_point(geo_xy())
  wkt <- geo_wkt("POINT (10 40)")
  collection <- geo_point(geo_xy(10, 40))

  expect_identical(cpp_convert(wkt_empty, geo_collection()), collection_empty)
  expect_identical(cpp_convert(wkt, geo_collection()), collection)

  expect_identical(cpp_convert(collection_empty, geo_wkt()), wkt_empty)
  expect_identical(cpp_convert(cpp_convert(collection, geo_wkt()), geo_collection()), collection)
})

test_that("geo_linestring conversion works", {
  wkt_empty <- geo_wkt("LINESTRING EMPTY")
  collection_empty <- geo_linestring(geo_xy())
  wkt <- geo_wkt("LINESTRING (30 10, 10 30, 40 40)")
  collection <- geo_linestring(geo_xy(c(30, 10, 40), c(10, 30, 40)))

  expect_identical(cpp_convert(wkt_empty, geo_collection()), collection_empty)
  expect_identical(cpp_convert(wkt, geo_collection()), collection)

  expect_identical(cpp_convert(collection_empty, geo_wkt()), wkt_empty)
  expect_identical(cpp_convert(cpp_convert(collection, geo_wkt()), geo_collection()), collection)
})

test_that("geo_multipoint conversion works", {
  wkt_empty <- geo_wkt("MULTIPOINT EMPTY")
  collection_empty <- geo_multipoint(geo_collection())
  wkt <- geo_wkt("MULTIPOINT ((10 40), (40 30))")
  collection <- geo_multipoint(geo_xy(c(10, 40), c(40, 30)))

  expect_identical(cpp_convert(wkt_empty, geo_collection()), collection_empty)
  expect_identical(cpp_convert(wkt, geo_collection()), collection)

  expect_identical(cpp_convert(collection_empty, geo_wkt()), wkt_empty)
  expect_identical(cpp_convert(cpp_convert(collection, geo_wkt()), geo_collection()), collection)
})

test_that("geo_multilinestring conversion works", {
  wkt_empty <- geo_wkt("MULTILINESTRING EMPTY")
  collection_empty <- geo_multilinestring(geo_collection())
  wkt <- geo_wkt("MULTILINESTRING ((10 10, 20 20, 10 40), (40 40, 30 30, 40 20, 30 10))")
  collection <- geo_multilinestring(
    c(
      geo_linestring(
        geo_xy(
          c(10, 20, 10),
          c(10, 20, 40)
        )
      ),
      geo_linestring(
        geo_xy(
          c(40, 30, 40, 30),
          c(40, 30, 20, 10)
        )
      )
    )
  )

  expect_identical(cpp_convert(wkt_empty, geo_collection()), collection_empty)
  expect_identical(cpp_convert(wkt, geo_collection()), collection)

  expect_identical(cpp_convert(collection_empty, geo_wkt()), wkt_empty)
  expect_identical(cpp_convert(cpp_convert(collection, geo_wkt()), geo_collection()), collection)
})

test_that("geo_polygon conversion works", {
  wkt_empty <- geo_wkt("POLYGON EMPTY")
  collection_empty <- geo_polygon(geo_xy())
  wkt <- geo_wkt("POLYGON ((30 10, 10 30, 40 40, 30 10))")
  collection <- geo_polygon(geo_xy(c(30, 10, 40, 30), c(10, 30, 40, 10)))

  wkt_hole <- geo_wkt(
    "POLYGON ((35 10, 45 45, 15 40, 10 20, 35 10), (20 30, 35 35, 30 20, 20 30))"
  )
  collection_hole <- geo_polygon(
    geo_xy(
      c(35, 45, 15, 10, 35, 20, 35, 30, 20),
      c(10, 45, 40, 20, 10, 30, 35, 20, 30)
    ),
    ring = c(1, 1, 1, 1, 1, 2, 2, 2, 2)
  )

  expect_identical(cpp_convert(wkt_empty, geo_collection()), collection_empty)
  expect_identical(cpp_convert(wkt, geo_collection()), collection)
  expect_identical(cpp_convert(wkt_hole, geo_collection()), collection_hole)

  expect_identical(cpp_convert(collection_empty, geo_wkt()), wkt_empty)
  expect_identical(cpp_convert(cpp_convert(collection, geo_wkt()), geo_collection()), collection)
  expect_identical(cpp_convert(cpp_convert(collection_hole, geo_wkt()), geo_collection()), collection_hole)
})

test_that("geo_multipolygon conversion works", {
  wkt_empty <- geo_wkt("MULTIPOLYGON EMPTY")
  collection_empty <- geo_multipolygon(geo_collection())
  wkt <- geo_wkt(
    "MULTIPOLYGON (((40 40, 20 45, 45 30, 40 40)),
            ((20 35, 10 30, 10 10, 30 5, 45 20, 20 35), (30 20, 20 15, 20 25, 30 20)))
    ")
  collection <- geo_multipolygon(
    c(
      geo_polygon(
        geo_xy(
          c(40, 20, 45, 40),
          c(40, 45, 30, 40)
        )
      ),
      geo_polygon(
        geo_xy(
          c(20, 10, 10, 30, 45, 20, 30, 20, 20, 30),
          c(35, 30, 10, 5,  20, 35, 20, 15, 25, 20)
        ),
        ring = c(1, 1, 1, 1, 1, 1, 2, 2, 2, 2)
      )
    )
  )

  expect_identical(cpp_convert(wkt_empty, geo_collection()), collection_empty)
  expect_identical(cpp_convert(wkt, geo_collection()), collection)

  expect_identical(cpp_convert(collection_empty, geo_wkt()), wkt_empty)
  expect_identical(cpp_convert(cpp_convert(collection, geo_wkt()), geo_collection()), collection)
})

test_that("geo_collection() conversion works", {
  wkt_empty <- geo_wkt("GEOMETRYCOLLECTION EMPTY")
  collection_empty <- geo_collection(list(geo_collection()))
  wkt <- geo_wkt(
    "
    GEOMETRYCOLLECTION (
      POINT (40 10),
      LINESTRING (10 10, 20 20, 10 40),
      POLYGON ((40 40, 20 45, 45 30, 40 40))
    )
    "
  )
  collection <- geo_collection(
    list(
      c(
        geo_point(geo_xy(40, 10)),
        geo_linestring(geo_xy(c(10, 20, 10), c(10, 20, 40))),
        geo_polygon(geo_xy(c(40, 20, 45, 40), c(40, 45, 30, 40)))
      )
    )
  )

  expect_identical(cpp_convert(wkt_empty, geo_collection()), collection_empty)
  expect_identical(cpp_convert(wkt, geo_collection()), collection)

  expect_identical(cpp_convert(collection_empty, geo_wkt()), wkt_empty)
  expect_identical(cpp_convert(cpp_convert(collection, geo_wkt()), geo_collection()), collection)
})

test_that("geo_collection() conversion propogates SRIDs", {
  collection_empty <- geo_collection(list(geo_collection()), srid = 23)
  expect_identical(cpp_convert(collection_empty, geo_collection()), collection_empty)


  collection <- geo_collection(
    list(
      c(
        # subgeometries all get assigned the same SRID as the parent
        # in GEOS (I think)
        geo_point(geo_xy(40, 10), srid = 1721),
        geo_linestring(geo_xy(c(10, 20, 10), c(10, 20, 40)), srid = 1721),
        geo_polygon(geo_xy(c(40, 20, 45, 40), c(40, 45, 30, 40)), srid = 1721)
      ),
      # a bit awkward...
      unclass(geo_point(geo_xy(30, 3332)))$feature[[1]]
    ),
    srid = c(1721, 2)
  )

  expect_identical(cpp_convert(collection, geo_collection()), collection)
})

test_that("xy conversion works", {
  collection <- c(geo_point(geo_xy(1, 6)), geo_point(geo_xy(2, 7)))
  xy <- geo_xy(1:2, 6:7)

  expect_identical(cpp_convert(xy, geo_collection()), collection)
  expect_identical(cpp_convert(collection, geo_xy()), xy)

  # handling of NA, NaN, inf
  # GEOS doesn't differentiate between POINT (nan nan) and POINT EMPTY,
  # we handle this through missing values for XY
  expect_equal(cpp_convert(geo_xy(NA, NA), geo_wkt()), geo_wkt(NA))
  expect_equal(cpp_convert(geo_xy(-Inf, Inf), geo_wkt()), geo_wkt("POINT (-inf inf)"))

  expect_equal(cpp_convert(geo_wkt("POINT (nan nan)"), geo_xy()),  geo_xy(NA, NA))
  expect_equal(cpp_convert(geo_xy(-Inf, Inf), geo_wkt()), geo_wkt("POINT (-inf inf)"))

  # errors: linestring as XY, point with SRID
  expect_error(cpp_convert(geo_wkt("LINESTRING EMPTY"), geo_xy()), "Can't represent")
  expect_warning(cpp_convert(geo_point(geo_xy(), srid = 1), geo_xy()), "Dropping SRID")
})

test_that("segment conversion works", {
  segment <- geo_segment(geo_xy(0, 10), geo_xy(20, 30))

  expect_identical(
    cpp_convert(geo_wkt("LINESTRING (0 10, 20 30)"), geo_segment()),
    segment
  )

  expect_identical(
    cpp_convert(cpp_convert(segment, geo_wkt()), geo_segment()),
    segment
  )

  # empty == all NAs
  expect_identical(
    cpp_convert(geo_wkt("LINESTRING EMPTY"), geo_segment()),
    geo_segment(geo_xy(NA, NA), geo_xy(NA, NA))
  )

  # errors: non linestring, more than two points
  expect_error(cpp_convert(geo_wkt("POINT (10 30)"), geo_segment()), "non-linestring")
  expect_error(cpp_convert(geo_wkt("LINESTRING (10 30, 0 0, 10 4)"), geo_segment()), "exactly two points")

})

test_that("missings are propogated through conversions between wkt, wkb, and collection", {
  expect_identical(cpp_convert(geo_wkt(NA), geo_wkb()), geo_wkb(list(NULL)))
  expect_identical(cpp_convert(geo_wkt(NA), geo_collection()), geo_collection(list(NULL), srid = NA))
  expect_identical(cpp_convert(geo_wkt(NA), geo_xy()), geo_xy(NA, NA))

  expect_identical(cpp_convert(geo_wkb(list(NULL)), geo_wkt()), geo_wkt(NA))
  expect_identical(cpp_convert(geo_wkb(list(NULL)), geo_collection()), geo_collection(list(NULL), srid = NA))
  expect_identical(cpp_convert(geo_wkb(list(NULL)), geo_xy()), geo_xy(NA, NA))

  expect_identical(cpp_convert(geo_collection(list(NULL)), geo_wkt()), geo_wkt(NA))
  expect_identical(cpp_convert(geo_collection(list(NULL)), geo_wkb()), geo_wkb(list(NULL)))
  expect_identical(cpp_convert(geo_collection(list(NULL)), geo_xy()), geo_xy(NA, NA))

  expect_identical(cpp_convert(geo_xy(NA, NA), geo_wkt()), geo_wkt(NA))
  expect_identical(cpp_convert(geo_xy(NA, NA), geo_wkb()), geo_wkb(list(NULL)))
  expect_identical(cpp_convert(geo_xy(NA, NA), geo_collection()), geo_collection(list(NULL), srid = NA))
})

test_that("error occurs with unknown object in conversions", {
  expect_error(cpp_convert(as.Date("2020-01-01"), new_geo_wkt()), "Can't resolve")
  expect_error(cpp_convert(geo_wkt("POINT EMPTY"), as.Date("2020-01-01")), "Can't resolve")
})
