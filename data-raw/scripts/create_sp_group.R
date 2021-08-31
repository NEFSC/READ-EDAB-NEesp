
sp_group <- rio::import(here::here("data-raw", "species_groupings_V2.csv"))
usethis::use_data(sp_group)
