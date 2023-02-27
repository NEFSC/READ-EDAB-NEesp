`%>%` <- magrittr::`%>%`

# download data ----

# MRIP data was accessed through the links on this public-facing page:
# https://www.fisheries.noaa.gov/recreational-fishing-data/recreational-fishing-data-downloads

# Survey data was accessed through the URLs linked here:
# https://www.st.nmfs.noaa.gov/st1/recreational/MRIP_Survey_Data/
# Survey trip and catch data description (copied from website):
# TRIP_YYYYW.sas7bdat: Trip-level data (analogous to MRFSS i1 dataset)
# and variables required for estimation.
# Contains one record per angler trip interview (identified by id_code).
# CATCH_YYYYW.sas7bdat: Catch-level data and variables required for estimation.
# Contains one record per species for each angler trip interview.

# Estimate data was accessed through the URLs linked here:
# https://www.st.nmfs.noaa.gov/st1/recreational/MRIP_Estimate_Data/
# Estimate data description (copied from website):
# Calibrated estimates are available for the Atlantic and Gulf coasts beginning in 1981.

## functions ----

download_and_extract <- function(url,
                                 pattern = "ps_.{1,}",
                                 exdir = "MRIP_data") {
  name <- stringr::str_extract(url, pattern = pattern)
  download.file(url, name)

  unzip(here::here(name),
        exdir = exdir
  )
}

compile <- function(type,
                    dir = "MRIP_data",
                    outdir = "data-raw/share/",
                    species = "BLUEFISH") {

  files <- list.files(here::here(dir),
                      pattern = type,
                      full.names = TRUE
  )

  big_data <- tibble::tibble()
  for (i in 1:length(files)) {
    this_data <- read.csv(files[i],
                          colClasses = "character"
    ) %>%
      tibble::as_tibble()

    colnames(this_data) <- stringr::str_to_lower(colnames(this_data))

    #message(nrow(this_data))

    if (nrow(this_data) > 0) {
      if (type == "catch" |
          type == "catch_wave") {
        this_data <- this_data %>%
          dplyr::filter(stringr::str_detect(
            common,
            species
          ))
      }
      # keep all species for trip data - to calculate % bluefish trips

      if (nrow(big_data) == 0) {
        big_data <- this_data
      }

      big_data <- dplyr::full_join(big_data, this_data) %>%
        invisible()

      print((i / length(files)) %>% round(digits = 2)) # keep track of progress
    }
  }
  saveRDS(big_data,
          file = paste0(outdir, type, ".RDS")
  )
}

## directed trips ----
# downloaded manually from MRIP query tool
# https://www.fisheries.noaa.gov/data-tools/recreational-fisheries-statistics-queries
# downloaded atlantic coast by state, all modes by mode, all waves, all areas by area
# selected all bluefish associated trips - if it was a target species, if it was
# caught, if it was released

# put together into one file
files <- list.files(here::here("data-raw/dont-share/MRIP_directed_trips"),
                    full.names = TRUE)

big_dat <- c()
for(i in files){
  this_dat <- read.csv(i,
                       skip = 24,
                       na.strings = ".")
  message(unique(this_dat$Year)) # make sure all years are downloaded
  big_dat <- rbind(big_dat, this_dat)
}

# update name here, depending on species
bluefish_directed_trips <- big_dat
usethis::use_data(bluefish_directed_trips)

# ESTIMATE data ----

## *code keys ----
area_key <- tibble::tibble(area_x = as.character(1:5),
                           area_x_words = c("Ocean <= 3 mi (all but WFL)",
                                            "Ocean > 3 mi (all but WFL)",
                                            "Ocean <= 10 mi (WFL only)",
                                            "Ocean > 10 mi (WFL only)",
                                            "Inland"))

fl_key <- tibble::tibble(fl_reg = as.character(1:5),
                         fl_reg_words = c("BAY, DIXIE, ESCAMBIA, FRANKLIN, GULF, JEFFERSON, OKALOOSA, SANTA ROSA, TAYLOR, WAKULLA, WALTON",
                                          "CHARLOTTE, CITRUS, COLLIER, HERNANDO, HILLSBOROUGH, LEE, LEVY, MANATEE, PASCO, PINELLAS, SARASOTA",
                                          "MONROE",
                                          "BROWARD, DADE, INDIAN RIVER, MARTIN, PALM BEACH ST. LUCIE",
                                          "BREVARD, CLAY, DUVAL, FLAGLER, NASSAU, ST. JOHNS, VOLUSIA"))

mode_key <- tibble::tibble(mode_fx = as.character(3:7),
                           mode_fx_words = c("Shore",
                                             "Party Boat",
                                             "Charter Boat",
                                             "Party/Charter Boat",
                                             "Private/Rental Boat"))

st_key <- tibble::tibble(st = as.character(
  c(1, 9:10, 12:13, 22:25, 28, 33:34, 36:37, 44:45, 51)),
  st_words = c("Alabama", "Connecticut", "Delaware",
               "Florida", "Georgia", "Louisiana",
               "Maine", "Maryland", "Massachusetts",
               "Mississippi", "New Hampshire", "New Jersey",
               "New York", "North Carolina", "Rhode Island",
               "South Carolina", "Virginia"))

## *download estimate data ----
urls <- paste0("https://www.st.nmfs.noaa.gov/st1/recreational/MRIP_Estimate_Data/CSV/mrip_",
               c("catch_1981_1989", "catch_1990_1999", "catch_2000_2009", "catch_2010_2016",
                 "catch_2017", "catch_2018", "catch_2019", "catch_2020",
                 "catch_2021_preliminary",
                 "effort_1981_1989", "effort_1990_1999", "effort_2000_2009", "effort_2010_2016",
                 "effort_2017", "effort_2018", "effort_2019", "effort_2020",
                 "effort_2021_preliminary"),
               "_csv.zip")

# download and extract all existing data
lapply(urls, download_and_extract, pattern = "mrip_")

compile("catch_wave", dir = "data-raw/share")
compile("effort", dir = "data-raw/share")

# *clean data and save to package ----

## **catch ----
mrip_catch <- readRDS(here::here("data-raw/share/catch_wave.RDS"))
mrip_catch <- mrip_catch %>%
  dplyr::select(-c(status, wave, sub_reg, st, sp_code, mode_fx, area_x)) %>%
  as.data.frame()

for (i in 8:24) {
  mrip_catch[, i] <- as.numeric(gsub(",", "", mrip_catch[, i]))
}
mrip_catch <- tibble::as_tibble(mrip_catch)
usethis::use_data(mrip_catch, overwrite = TRUE)

## **effort ----
mrip_effort <- readRDS(here::here("data-raw/share/effort.RDS"))

mrip_effort <- mrip_effort %>%
  dplyr::mutate(
    estrips = estrips %>%
      stringr::str_remove_all(",")) %>%
  dplyr::left_join(area_key, by = "area_x") %>%
  dplyr::left_join(mode_key, by = "mode_fx") %>%
  dplyr::left_join(st_key, by = "st") %>%
  dplyr::select(-c(area_x, fl_reg, mode_fx, st)) %>%
  dplyr::rename(area_x = area_x_words,
                fl_reg = fl_reg_words,
                mode_fx = mode_fx_words,
                st = st_words) %>%
  tidyr::drop_na(st) # don't include states outside of the atlantic coast

usethis::use_data(mrip_effort)

# future updates ----

# in future years, you don't have to re-pull all of the data
# (unless you want to)
# download new data only and join to existing data
# this function is preliminary, should verify before using

update_mrip <- function(url, # url of new data, or vector of urls
                        type,
                        original_data) {
  for (i in 1:length(url)) {
    name <- stringr::str_extract(url[i], pattern = "ps_.{1,}")
    download.file(url[i], name)

    unzip(here::here(name),
          exdir = "new_data"
    )
  }

  files <- list.files(here::here("new_data"),
                      pattern = type,
                      full.names = TRUE
  )

  for (i in 1:length(files)) {
    this_data <- read.csv(files[i],
                          colClasses = "character"
    ) %>%
      tibble::as_tibble()

    colnames(this_data) <- stringr::str_to_lower(colnames(this_data))

    if (nrow(this_data) > 0) {
      if (type == "catch") {
        this_data <- this_data %>%
          dplyr::filter(stringr::str_detect(common, "BLUEFISH")) # filter to bluefish only
      } else if (type == "trip") {
        this_data <- this_data %>%
          dplyr::filter(stringr::str_detect(prim2_common, "BLUEFISH") |
                          stringr::str_detect(prim1_common, "BLUEFISH")) # filter to bluefish only
      }
      big_data <- dplyr::full_join(original_data, this_data) %>%
        invisible()

      print((i / length(files)) %>% round(digits = 2))
    }
  }
  saveRDS(big_data,
          file = paste0(type, "_updated.RDS")
  )

  unlink("new_data", recursive = TRUE)
}

# example
update_mrip(
  url = "https://www.st.nmfs.noaa.gov/st1/recreational/MRIP_Survey_Data/CSV/ps_2021_preliminary_csv.zip",
  type = "catch",
  original_data = dat %>%
    dplyr::filter(year != "2021")
)

dat2 <- readRDS("catch_updated.RDS")
head(dat2)

# update package data with usethis::use_data(..., overwrite = TRUE)
