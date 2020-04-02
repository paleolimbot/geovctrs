
sf_nc <- sf::read_sf(system.file("shape/nc.shp", package="sf"))

nc_wkb <- sf:::CPL_write_wkb(sf_nc$geometry, EWKB = TRUE)
nc_tbl <- sf::st_set_geometry(sf_nc, NULL)

geo_nc <- dplyr::select(tibble::as_tibble(nc_tbl),-CNTY_, -AREA, -PERIMETER)
geo_nc$geometry <- geovctrs::geo_wkb(nc_wkb)

usethis::use_data(geo_nc, overwrite = TRUE)
