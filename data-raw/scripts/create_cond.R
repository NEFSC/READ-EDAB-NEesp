`%>%` <- magrittr::`%>%`

# condition ----
cond <- rbind(
  read.csv("https://raw.githubusercontent.com/Laurels1/Condition/master/data/AnnualRelCond2018_GB.csv"),
  read.csv("https://raw.githubusercontent.com/Laurels1/Condition/master/data/AnnualRelCond2018_GOM.csv"),
  read.csv("https://raw.githubusercontent.com/Laurels1/Condition/master/data/AnnualRelCond2018_MAB.csv"),
  read.csv("https://raw.githubusercontent.com/Laurels1/Condition/master/data/AnnualRelCond2018_SS.csv")
) %>%
  update_species_names(species_col = "Species")
cond$Species <- stringr::str_to_sentence(cond$Species)
cond$Species <- cond$Species %>% stringr::str_replace("Atl cod", "Atlantic cod")
cond$Species <- cond$Species %>% stringr::str_replace("Atl herring", "Atlantic herring")
cond$Species <- cond$Species %>% stringr::str_replace("Yellowtail", "Yellowtail flounder")
cond$Species <- cond$Species %>% stringr::str_replace("Windowpane flounder", "Windowpane")

usethis::use_data(cond)
