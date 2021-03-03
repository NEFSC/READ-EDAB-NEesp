`%>%` <- magrittr::`%>%`

## commercial catch ----
com <- read.csv(here::here("data-raw", "com_landings_clean_20201222.csv"))
com$Species <- stringr::str_to_sentence(com$common_name)
com <- com %>%
  dplyr::select(Species, Year, State, Pounds, Dollars) %>%
  dplyr::filter(is.na(Year) == FALSE)

# adjust com for inflation
quantmod::getSymbols("CPIAUCSL", src='FRED')
avg.cpi <- xts::apply.yearly(CPIAUCSL, mean) 
cf <- avg.cpi/as.numeric(avg.cpi['2019']) # using 2019 as the base year 
cf <- cf %>%
  as.data.frame() %>% 
  tibble::rownames_to_column()
cf$Year <- cf$rowname %>% 
  stringr::str_sub(start = 1, end = 4) %>%
  as.numeric()

com <- dplyr::left_join(com, cf, by = "Year")
com$Dollars_adj <- com$Dollars / com$CPIAUCSL

com <- com %>%
  dplyr::select(Species, Year, State, Pounds, Dollars_adj)

write.csv(com, here::here("data-raw", "com_landings_clean_20201222_formatted.csv"))

com <- read.csv(here::here("data-raw", "com_landings_clean_20201222_formatted.csv")) 
com$Species <- com$Species %>%
  stringr::str_replace("Windowpane flounder", "Windowpane")

com_catch <- com
usethis::use_data(com_catch)