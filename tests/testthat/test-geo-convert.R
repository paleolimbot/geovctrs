
test_that("wkt conversion works", {
  wkt <- geo_wkt(c("POINT (30 10)", "POINT (20 20)"))
  wkt_roundtrip <- geovctrs_cpp_convert(wkt, new_geovctrs_wkt())
  expect_identical(wkt_roundtrip, wkt)
})

test_that("conversion prototype args are used", {
  wkt <- geo_wkt("POINT Z (10 11 12)")
  expect_identical(
    geovctrs_cpp_convert(wkt, new_geovctrs_wkt(trim = FALSE, precision = 2L)),
    geo_wkt("POINT Z (10.00 11.00 12.00)")
  )

  expect_identical(
    geovctrs_cpp_convert(wkt, new_geovctrs_wkt(trim = FALSE, precision = 2L, dimensions = 2L)),
    geo_wkt("POINT (10.00 11.00)")
  )

  # make sure casting applies args
  expect_identical(
    as_geo_wkt(wkt, trim = FALSE, precision = 2, dimensions = 2),
    geo_wkt("POINT (10.00 11.00)")
  )

  expect_identical(
    as_geo_wkt(wkt, trim = FALSE, precision = 2, dimensions = 3),
    geo_wkt("POINT Z (10.00 11.00 12.00)")
  )
})

test_that("roundtrip to- and from- geo_wkb() works", {
  expect_identical(
    geovctrs_cpp_convert(
      geovctrs_cpp_convert(
        geo_example_wkt,
        geo_wkb()
      ),
      geo_collection()
    ),
    as_geo_collection(geo_example_wkt)
  )
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
  wkb_roundtrip <- geovctrs_cpp_convert(wkb, new_geovctrs_wkb())
  expect_identical(wkb, wkb_roundtrip)
})

test_that("wkb writer options are respected", {
  collection_empty <- geo_collection(list(geo_collection()), srid = 23)
  collection_wkb <- geovctrs_cpp_convert(collection_empty, geo_wkb())
  expect_identical(geovctrs_cpp_convert(collection_wkb, geo_collection()), collection_empty)

  collection_with_srid <- c(
    geo_point(geo_xy(0, 1), srid = 12),
    geo_point(geo_xy(10, 12), srid = 27)
  )

  expect_identical(
    unclass(as_geo_wkb(collection_with_srid, endian = 1))[[1]][1],
    as.raw(0x01)
  )

  expect_identical(
    unclass(as_geo_wkb(collection_with_srid, endian = 0))[[1]][1],
    as.raw(0x00)
  )

  expect_identical(
    geo_srid(as_geo_wkb(collection_with_srid)),
    c(12L, 27L)
  )

  expect_identical(
    geo_srid(as_geo_wkb(collection_with_srid, include_srid = TRUE)),
    c(12L, 27L)
  )

  expect_identical(
    geo_srid(geovctrs_cpp_convert(collection_with_srid, new_geovctrs_wkb(include_srid = FALSE))),
    c(0L, 0L)
  )

  expect_identical(
    geo_coordinate_dimensions(geovctrs_cpp_convert(geo_wkt("POINT Z (1 2 3)"), new_geovctrs_wkb(dimensions = 2))),
    2L
  )
  expect_identical(
    geo_coordinate_dimensions(geovctrs_cpp_convert(geo_wkt("POINT Z (1 2 3)"), new_geovctrs_wkb(dimensions = 3))),
    3L
  )

  wkb <- as_geo_wkb(geo_wkt("POINT Z (1 2 3)"))
  expect_identical(geovctrs_cpp_convert(wkb, new_geovctrs_wkb(dimensions = 3)), wkb)
})

test_that("roundtrip to- and from- geo_collection() works", {
  expect_identical(
    geovctrs_cpp_convert(
      geovctrs_cpp_convert(
        geo_example_wkt,
        geo_collection()
      ),
      geo_wkb()
    ),
    as_geo_wkb(geo_example_wkt)
  )
})

test_that("geo_point conversion works", {
  wkt_empty <- geo_wkt("POINT EMPTY")
  collection_empty <- geo_point(geo_xy())
  wkt <- geo_wkt("POINT (10 40)")
  collection <- geo_point(geo_xy(10, 40))

  expect_identical(geovctrs_cpp_convert(wkt_empty, geo_collection()), collection_empty)
  expect_identical(geovctrs_cpp_convert(wkt, geo_collection()), collection)

  expect_identical(geovctrs_cpp_convert(collection_empty, geo_wkt()), wkt_empty)
  expect_identical(geovctrs_cpp_convert(geovctrs_cpp_convert(collection, geo_wkt()), geo_collection()), collection)
})

test_that("geo_linestring conversion works", {
  wkt_empty <- geo_wkt("LINESTRING EMPTY")
  collection_empty <- geo_linestring(geo_xy())
  wkt <- geo_wkt("LINESTRING (30 10, 10 30, 40 40)")
  collection <- geo_linestring(geo_xy(c(30, 10, 40), c(10, 30, 40)))

  expect_identical(geovctrs_cpp_convert(wkt_empty, geo_collection()), collection_empty)
  expect_identical(geovctrs_cpp_convert(wkt, geo_collection()), collection)

  expect_identical(geovctrs_cpp_convert(collection_empty, geo_wkt()), wkt_empty)
  expect_identical(geovctrs_cpp_convert(geovctrs_cpp_convert(collection, geo_wkt()), geo_collection()), collection)
})

test_that("geo_multipoint conversion works", {
  wkt_empty <- geo_wkt("MULTIPOINT EMPTY")
  collection_empty <- geo_multipoint(geo_collection())
  wkt <- geo_wkt("MULTIPOINT ((10 40), (40 30))")
  collection <- geo_multipoint(geo_xy(c(10, 40), c(40, 30)))

  expect_identical(geovctrs_cpp_convert(wkt_empty, geo_collection()), collection_empty)
  expect_identical(geovctrs_cpp_convert(wkt, geo_collection()), collection)

  expect_identical(geovctrs_cpp_convert(collection_empty, geo_wkt()), wkt_empty)
  expect_identical(geovctrs_cpp_convert(geovctrs_cpp_convert(collection, geo_wkt()), geo_collection()), collection)
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

  expect_identical(geovctrs_cpp_convert(wkt_empty, geo_collection()), collection_empty)
  expect_identical(geovctrs_cpp_convert(wkt, geo_collection()), collection)

  expect_identical(geovctrs_cpp_convert(collection_empty, geo_wkt()), wkt_empty)
  expect_identical(geovctrs_cpp_convert(geovctrs_cpp_convert(collection, geo_wkt()), geo_collection()), collection)
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

  expect_identical(geovctrs_cpp_convert(wkt_empty, geo_collection()), collection_empty)
  expect_identical(geovctrs_cpp_convert(wkt, geo_collection()), collection)
  expect_identical(geovctrs_cpp_convert(wkt_hole, geo_collection()), collection_hole)

  expect_identical(geovctrs_cpp_convert(collection_empty, geo_wkt()), wkt_empty)
  expect_identical(geovctrs_cpp_convert(geovctrs_cpp_convert(collection, geo_wkt()), geo_collection()), collection)
  expect_identical(geovctrs_cpp_convert(geovctrs_cpp_convert(collection_hole, geo_wkt()), geo_collection()), collection_hole)
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

  expect_identical(geovctrs_cpp_convert(wkt_empty, geo_collection()), collection_empty)
  expect_identical(geovctrs_cpp_convert(wkt, geo_collection()), collection)

  expect_identical(geovctrs_cpp_convert(collection_empty, geo_wkt()), wkt_empty)
  expect_identical(geovctrs_cpp_convert(geovctrs_cpp_convert(collection, geo_wkt()), geo_collection()), collection)
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

  expect_identical(geovctrs_cpp_convert(wkt_empty, geo_collection()), collection_empty)
  expect_identical(geovctrs_cpp_convert(wkt, geo_collection()), collection)

  expect_identical(geovctrs_cpp_convert(collection_empty, geo_wkt()), wkt_empty)
  expect_identical(geovctrs_cpp_convert(geovctrs_cpp_convert(collection, geo_wkt()), geo_collection()), collection)
})

test_that("geo_collection() conversion propogates SRIDs", {
  collection_empty <- geo_collection(list(geo_collection()), srid = 23)
  expect_identical(geovctrs_cpp_convert(collection_empty, geo_collection()), collection_empty)


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

  expect_identical(geovctrs_cpp_convert(collection, geo_collection()), collection)
})

test_that("xy conversion works", {
  collection <- c(geo_point(geo_xy(1, 6)), geo_point(geo_xy(2, 7)))
  xy <- geo_xy(1:2, 6:7)

  expect_identical(geovctrs_cpp_convert(xy, geo_collection()), collection)
  expect_identical(geovctrs_cpp_convert(collection, geo_xy()), xy)

  # empty points are also NA points
  expect_equal(geovctrs_cpp_convert(geo_xy(NA, NA), geo_wkt()), geo_wkt("POINT EMPTY"))
  expect_equal(geovctrs_cpp_convert(geo_xy(-Inf, Inf), geo_wkt()), geo_wkt("POINT (-inf inf)"))

  expect_equal(geovctrs_cpp_convert(geo_wkt("POINT (nan nan)"), geo_xy()),  geo_xy(NA, NA))
  expect_equal(geovctrs_cpp_convert(geo_xy(-Inf, Inf), geo_wkt()), geo_wkt("POINT (-inf inf)"))

  # errors: linestring as XY, point with SRID
  expect_error(geovctrs_cpp_convert(geo_wkt("LINESTRING EMPTY"), geo_xy()), "Can't represent")
  expect_warning(geovctrs_cpp_convert(geo_point(geo_xy(), srid = 1), geo_xy()), "Dropping SRID")
})

test_that("segment conversion works", {
  segment <- geo_segment(geo_xy(0, 10), geo_xy(20, 30))

  expect_identical(
    geovctrs_cpp_convert(geo_wkt("LINESTRING (0 10, 20 30)"), geo_segment()),
    segment
  )

  expect_identical(
    geovctrs_cpp_convert(geovctrs_cpp_convert(segment, geo_wkt()), geo_segment()),
    segment
  )

  # empty == all NAs
  expect_identical(
    geovctrs_cpp_convert(geo_wkt("LINESTRING EMPTY"), geo_segment()),
    geo_segment(geo_xy(NA, NA), geo_xy(NA, NA))
  )

  # errors: non linestring, more than two points
  expect_error(geovctrs_cpp_convert(geo_wkt("POINT (10 30)"), geo_segment()), "non-linestring")
  expect_error(geovctrs_cpp_convert(geo_wkt("LINESTRING (10 30, 0 0, 10 4)"), geo_segment()), "exactly two points")

})

test_that("missings are propogated through conversions between wkt, wkb, and collection", {
  expect_identical(geovctrs_cpp_convert(NA_wkt_, geo_wkb()), NA_wkb_)
  expect_identical(geovctrs_cpp_convert(NA_wkt_, geo_collection()), NA_collection_)
  expect_identical(geovctrs_cpp_convert(NA_wkt_, geo_xy()), NA_xy_)
  expect_identical(geovctrs_cpp_convert(NA_wkt_, geo_segment()), NA_segment_)

  expect_identical(geovctrs_cpp_convert(NA_wkb_, geo_wkt()), NA_wkt_)
  expect_identical(geovctrs_cpp_convert(NA_wkb_, geo_collection()), NA_collection_)
  expect_identical(geovctrs_cpp_convert(NA_wkb_, geo_xy()), NA_xy_)
  expect_identical(geovctrs_cpp_convert(NA_wkb_, geo_segment()), NA_segment_)

  expect_identical(geovctrs_cpp_convert(NA_collection_, geo_wkt()), NA_wkt_)
  expect_identical(geovctrs_cpp_convert(NA_collection_, geo_wkb()), NA_wkb_)
  expect_identical(geovctrs_cpp_convert(NA_collection_, geo_xy()), NA_xy_)
  expect_identical(geovctrs_cpp_convert(NA_collection_, geo_segment()), NA_segment_)

  # NA_xy_ is POINT EMPTY
  expect_identical(geovctrs_cpp_convert(NA_xy_, geo_wkt()), geo_wkt("POINT EMPTY"))
  expect_identical(geovctrs_cpp_convert(NA_xy_, geo_wkb()), as_geo_wkb(geo_wkt("POINT EMPTY")))
  expect_identical(geovctrs_cpp_convert(NA_xy_, geo_collection()), geo_point(geo_xy()))

  expect_identical(geovctrs_cpp_convert(NA_segment_, geo_wkt()), NA_wkt_)
  expect_identical(geovctrs_cpp_convert(NA_segment_, geo_wkb()), NA_wkb_)
  expect_identical(geovctrs_cpp_convert(NA_segment_, geo_collection()), NA_collection_)
  expect_identical(geovctrs_cpp_convert(NA_segment_, geo_xy()), NA_xy_)

  expect_identical(geovctrs_cpp_convert(NA_rect_, geo_wkt()), NA_wkt_)
  expect_identical(geovctrs_cpp_convert(NA_rect_, geo_wkb()), NA_wkb_)
  expect_identical(geovctrs_cpp_convert(NA_rect_, geo_collection()), NA_collection_)
  expect_identical(geovctrs_cpp_convert(NA_rect_, geo_xy()), NA_xy_)
})

test_that("error occurs with unknown object in conversions", {
  # test WKT and WKB because the readers/writers both need to handle these cases
  # this is a mostly a test of the deleters and whether or not they cause segfaults
  expect_error(geovctrs_cpp_convert(as.Date("2020-01-01"), new_geovctrs_wkt()), "Can't resolve")
  expect_error(geovctrs_cpp_convert(geo_wkt("POINT EMPTY"), as.Date("2020-01-01")), "Can't resolve")

  expect_error(geovctrs_cpp_convert(as.Date("2020-01-01"), new_geovctrs_wkb()), "Can't resolve")
  expect_error(geovctrs_cpp_convert(as_geo_wkb("POINT EMPTY"), as.Date("2020-01-01")), "Can't resolve")
})

test_that("error occurs with invalid objects", {
  wkb_bad <- as.raw(
    c(
      0x01, 0xff, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x3e, 0x40, 0x00, 0x00, 0x00,
      0x00, 0x00, 0x00, 0x24, 0x40
    )
  )

  wkb <- new_geovctrs_wkb(list(wkb_bad))

  # test WKT and WKB because the readers/writers both need to handle these cases
  # this is a mostly a test of the deleters and whether or not they cause segfaults
  expect_error(geovctrs_cpp_convert(wkb, new_geovctrs_wkt()), "ParseException")
  expect_error(geovctrs_cpp_convert(new_geovctrs_wkt("POINT NOPE"), geo_wkb()), "ParseException")
})
