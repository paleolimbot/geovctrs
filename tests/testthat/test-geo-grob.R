
test_that("geo_grob() works", {
  expect_is(geo_grob(character(0)), "gTree")
  char_grob <- geo_grob("POINT (0.5 0.5)", pch = 16, col = "blue", default.units = "npc")
  expect_is(char_grob, "points")

  expect_is(geo_grob(wkt()), "gTree")
  wkt_grob <- geo_grob(wkt("POINT (0.5 0.5)"), pch = 16, col = "red", default.units = "npc")
  expect_is(wkt_grob, "points")

  expect_is(geo_grob(wkb()), "gTree")
  wkb_grob <- geo_grob(as_wkb("POINT (0.5 0.5)"), pch = 16, col = "magenta", default.units = "npc")
  expect_is(wkb_grob, "points")

  expect_is(geo_grob(geo_xy()), "gTree")
  xy_grob <- geo_grob(as_geo_xy(wkt("POINT (0.5 0.5)")), pch = 16, col = "cyan", default.units = "npc")
  expect_is(xy_grob, "points")

  expect_is(geo_grob(geo_segment()), "gTree")
  seg_grob <- geo_grob(geo_segment(0, 0, 1, 1), col = "red", default.units = "npc")
  expect_is(seg_grob, "segments")

  expect_is(geo_grob(geo_segment()), "gTree")
  rect_grob <- geo_grob(
    geo_rect(0.1, 0.2, 0.3, 0.4),
    fill = "grey90",
    col = "purple",
    default.units = "npc"
  )
  expect_is(rect_grob, "rect")

  grid::grid.newpage()
  grid::grid.draw(char_grob)
  grid::grid.draw(wkt_grob)
  grid::grid.draw(wkb_grob)
  grid::grid.draw(xy_grob)
  grid::grid.draw(seg_grob)
  grid::grid.draw(rect_grob)
})
