
channel <- dbutils::connect_to_database(
  server = "sole",
  uid = "atyrell"
)

pull <- survdat::get_survdat_data(channel,
  all.season = TRUE,
  getBio = FALSE
) # problem with bio pull

saveRDS(pull, here::here("data-raw", "survdat_03032021_bio.RDS"))
# still too short :(

data <- readRDS(here::here("data-raw", "survdat_03032021.RDS")) # does NOT have bio data
data <- tibble::as_tibble(data$survdat)

# parse out survey data for NE species only, add common name
matches <- as.numeric(data$SVSPP) %in% as.numeric(species_key$SVSPP)

data2 <- data[matches, ]

species_name <- c()
for (i in 1:length(data2$SVSPP)) {
  svspp <- as.numeric(data2$SVSPP[i])
  r <- which(species_key$SVSPP == svspp)
  species_name[i] <- species_key[r, 2]
}

data2$Species <- species_name # using monkfish not goosefish

# add region to survey data

# rename regions
key <- read.csv("https://raw.githubusercontent.com/NOAA-EDAB/ECSA/master/data/seasonal_stock_strata.csv")
for (i in 1:length(key$stock_area)) {
  if (key$stock_area[i] == "gbk") {
    key$stock_area[i] <- "Georges Bank"
  }
  if (key$stock_area[i] == "gom") {
    key$stock_area[i] <- "Gulf of Maine"
  }
  if (key$stock_area[i] == "snemab") {
    key$stock_area[i] <- "Southern New England / Mid-Atlantic"
  }
  if (key$stock_area[i] == "gbkgom") {
    key$stock_area[i] <- "Gulf of Maine / Georges Bank"
  }
  if (key$stock_area[i] == "ccgom") {
    key$stock_area[i] <- "Cape Cod / Gulf of Maine"
  }
  if (key$stock_area[i] == "south") {
    key$stock_area[i] <- "Southern Georges Bank / Mid"
  }
  if (key$stock_area[i] == "north") {
    key$stock_area[i] <- "Gulf of Maine / Northern Georges Bank"
  }
  if (key$stock_area[i] == "sne") {
    key$stock_area[i] <- "Georges Bank / Southern New England"
  }
  if (key$stock_area[i] == "unit") {
    key$stock_area[i] <- "all"
  }
}

key$Species <- stringr::str_to_sentence(key$COMNAME) %>%
  stringr::str_replace("Goosefish", "Monkfish")
key$spst <- paste(key$Species, key$strata %>% as.numeric())
key3 <- key %>% dplyr::select(spst, stock_area)

data2$spst <- paste(data2$Species, data2$STRATUM %>% as.numeric())

data2$spst %in% key3$spst

data3 <- dplyr::left_join(data2, key3, by = "spst")
data3$Region <- data3$stock_area

# some NAs when survey caught fish outside stock areas - replace
data3$Region <- tidyr::replace_na(data3$Region, "Outside stock area")

# add unique way to identify fish observations
date_time <- data3$EST_TOWDATE %>% stringr::str_split(" ", simplify = TRUE)
data3$date <- date_time[, 1]
data3$fish_id <- paste(data3$CRUISE6, data3$STRATUM,
  data3$TOW, data3$date, data3$Species,
  sep = "_"
) # unique incidences of observing a species

# write.csv(data3, file = here::here("data", "survey_data.csv"))
saveRDS(data3, file = here::here("data-raw", "survey_data.RDS"))

survey <- readRDS(here::here("data-raw", "survey_data.RDS")) #%>%
  #update_species_names(species_col = "Species")
survey <- survey[-which(survey$Species == "Jonah crab" & survey$LENGTH >= 99.9), ] # remove error jonah crab
# sum(numlen) and abundance may not be equal
# abundnace has been corrected with conversion factor, but numlen has not

# fix survey numbers treated as characters
survey <- survey %>%
  dplyr::mutate(
    CRUISE6 = CRUISE6 %>% as.numeric(),
    STATION = STATION %>% as.numeric(),
    STRATUM = STRATUM %>% as.numeric(),
    TOW = TOW %>% as.numeric(),
    SVSPP = SVSPP %>% as.numeric(),
    CATCHSEX = CATCHSEX %>% as.numeric(),
    YEAR = YEAR %>% as.numeric()
  )

usethis::use_data(survey, overwrite = TRUE)
