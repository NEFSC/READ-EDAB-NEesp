`%>%` <- dplyr::`%>%`

# risk ----
risk <- read.csv(here::here("data-raw/risk_ranking", "full_risk_data.csv")) %>%
  dplyr::mutate(Species = Species %>% stringr::str_replace("Goosefish", "Monkfish"))
risk_year_hist <- read.csv(here::here("data-raw/risk_ranking", "full_historical_risk_data_over_time.csv")) %>%
  dplyr::mutate(Species = Species %>% stringr::str_replace("Goosefish", "Monkfish"))
risk_year_value <- read.csv(here::here("data-raw/risk_ranking", "full_risk_data_value_over_time.csv")) %>%
  dplyr::mutate(Species = Species %>% stringr::str_replace("Goosefish", "Monkfish"))
risk_species <- read.csv(here::here("data-raw/risk_ranking", "full_risk_data_by_species.csv")) %>%
  dplyr::mutate(Species = Species %>% stringr::str_replace("Goosefish", "Monkfish"))

usethis::use_data(risk)
usethis::use_data(risk_year_hist)
usethis::use_data(risk_year_value)
usethis::use_data(risk_species)
