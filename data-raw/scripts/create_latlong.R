`%>%` <- magrittr::`%>%`

## lat/long data ----
data <- read.csv("https://raw.githubusercontent.com/NOAA-EDAB/ECSA/master/data/seasonal_stock_strata.csv")
crs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

unique(data$stock_area)

# create stock regions to match other data
unique(data$stock_area)
# Region == "Gulf of Maine / Georges Bank" |
# Region == "Eastern Georges Bank" |
#  Region == "Georges Bank" |
#  Region == "Gulf of Maine" |
#  Region == "Gulf of Maine / Cape Hatteras" |
#  Region == "Georges Bank / Southern New England" |
#  Region == "Southern New England / Mid" |
#  Region == "Gulf of Maine / Northern Georges Bank" |
#  Region == "Southern Georges Bank / Mid" |
#  Region == "Cape Cod / Gulf of Maine"

Region <- c()
for (i in 1:length(data$stock_area)) {
  if (data$stock_area[i] == "gbk") {
    Region[i] <- "Georges Bank"
  }
  if (data$stock_area[i] == "gom") {
    Region[i] <- "Gulf of Maine"
  }
  if (data$stock_area[i] == "snemab") {
    Region[i] <- "Southern New England / Mid"
  }
  if (data$stock_area[i] == "gbkgom") {
    Region[i] <- "Gulf of Maine / Georges Bank"
  }
  if (data$stock_area[i] == "ccgom") {
    Region[i] <- "Cape Cod / Gulf of Maine"
  }
  if (data$stock_area[i] == "south") {
    Region[i] <- "Southern Georges Bank / Mid"
  }
  if (data$stock_area[i] == "north") {
    Region[i] <- "Gulf of Maine / Northern Georges Bank"
  }
  if (data$stock_area[i] == "sne") {
    Region[i] <- "Georges Bank / Southern New England"
  }
  if (data$stock_area[i] == "unit") {
    Region[i] <- "all"
  }
}

data$Region <- Region
data$Species <- stringr::str_to_sentence(data$COMNAME)

write.csv(data, file = here::here("data-raw", "geo_range_data.csv"))

latlong <- read.csv(here::here("data-raw", "geo_range_data.csv")) %>%
  dplyr::rename(stock_season = season_) %>%
  update_species_names(species_col = "Species")

usethis::use_data(latlong)
