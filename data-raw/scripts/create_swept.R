`%>%` <- magrittr::`%>%`

## swept area ----

og_pull <- readRDS(here::here("data-raw", "survdat_03032021.RDS"))
shape <- sf::read_sf(here::here("data-raw/strata_shapefiles", "BTS_Strata.shp"))

area <- survdat::get_area(shape, "STRATA")

og_pull$survdat$STRATUM <- as.numeric(og_pull$survdat$STRATUM)

mod_data <- survdat::strat_prep(
  surveyData = og_pull$survdat %>%
  #  dplyr::filter(SEASON == "FALL"),
   dplyr::filter(SEASON == "SPRING"),
  areaPolygon = shape,
  areaDescription = "STRATA"
)

mean_info <- survdat::strat_mean(mod_data,
  areaDescription = "STRATA",
  seasonFlag = TRUE,
  poststratFlag = FALSE
)

test <- survdat::swept_area(mod_data,
  stratmeanData = mean_info,
  areaDescription = "STRATA"
)
# test

# parse out survey data for NE species only, add common name
load(here::here("data", "species_key.rda"))

matches <- as.numeric(test$SVSPP) %in% as.numeric(species_key$SVSPP)

data <- test[matches, ]
# data

species_name <- c()
for (i in 1:length(data$SVSPP)) {
  svspp <- as.numeric(data$SVSPP[i])
  r <- which(species_key$SVSPP == svspp)
  species_name[i] <- species_key[r, 2]
}

data$Species <- species_name

write.csv(data, here::here("data-raw", "swept_area_info_spring.csv"))
#write.csv(data, here::here("data-raw", "swept_area_info_fall.csv"))

spring <- read.csv(here::here("data-raw", "swept_area_info_spring.csv"))
fall <- read.csv(here::here("data-raw", "swept_area_info_fall.csv"))

spring <- spring %>%
  dplyr::mutate(Season = "Spring")
fall <- fall %>%
  dplyr::mutate(Season = "Fall")

all <- rbind(spring, fall)
write.csv(all, here::here("data-raw", "swept_area_info.csv"))

swept <- read.csv(here::here("data-raw", "swept_area_info.csv")) %>%
  NEesp::update_species_names(species_col = "Species")

swept <- swept %>%
  dplyr::select(-X.1, -X)

usethis::use_data(swept, overwrite = TRUE)

# no regions - figure out how to retain regions
# cut data into regions (by species??)
# can the pull be parsed or will that mess up survdat functions?
