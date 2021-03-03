# latlong ----
shape <- sf::read_sf(here::here("data-raw/strata_shapefiles", "BTS_Strata.shp"))

usethis::use_data(shape)
