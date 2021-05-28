load(here::here("data-raw", "Annual_Black_Seabass_zooplankton_anomalies.rda"))
annual <- bsb.yr.anom
head(annual)

load(here::here("data-raw", "Spring_Black_Seabass_zooplankton_anomalies.rda"))
spring <- bsb.spr.anom
head(spring)

load(here::here("data-raw", "Fall_Black_Seabass_zooplankton_anomalies.rda"))
fall <- bsb.fall.anom
head(fall)

`%>%` <- magrittr::`%>%`

annual <- annual %>%
  dplyr::mutate(Species = "Black sea bass",
                Season = "Annual")

spring <- spring %>%
  dplyr::mutate(Species = "Black sea bass",
                Season = "Spring")

fall <- fall %>%
  dplyr::mutate(Species = "Black sea bass",
                Season = "Fall")

zoop_stock <- rbind(annual, spring, fall)
usethis::use_data(zoop_stock)
