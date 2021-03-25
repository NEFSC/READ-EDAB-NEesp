bio_survey <- readRDS(here::here("data-raw", "survdat_03022021_B.RDS"))

bio_survey <- bio_survey$survdat

usethis::use_data(bio_survey, overwrite = TRUE)
