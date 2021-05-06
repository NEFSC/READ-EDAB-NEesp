rec_effort <- read.csv(here::here("data-raw", "mrip_effort_1981_2020_rec_trips_MA_NE.csv"), stringsAsFactors = T, fileEncoding = "UTF-8-BOM")

usethis::use_data(rec_effort)
