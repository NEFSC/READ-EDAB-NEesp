nrcc <- read.csv("../NRCC/data/nrcc_overall_average.csv")
head(nrcc)

`%>%` <- magrittr::`%>%`

nrcc <- nrcc %>%
  dplyr::select(-X) %>%
  dplyr::mutate(Indicator = "NRCC_ranking",
                Year = "NRCC_risk_assessment") %>%
  dplyr::rename(Value = overall_average)

species_key <- NEesp::species_key
nrcc <- NEesp::get_risk(nrcc, year_source = "Year", value_source = "Value", analysis = "most_recent", high = "high_risk", indicator_name = "NRCC_ranking")
nrcc %>% dplyr::arrange(norm_rank) %>% tail

head(NEesp::nrcc)
tail(NEesp::nrcc)

usethis::use_data(nrcc, overwrite = TRUE)
