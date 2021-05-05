`%>%` <- magrittr::`%>%`

## commercial catch ----
com <- read.csv(here::here("data-raw", "com_landings_clean_20201222.csv"))
com$Species <- stringr::str_to_sentence(com$common_name)
com <- com %>%
  dplyr::select(Species, Year, State, Pounds, Dollars) %>%
  dplyr::filter(is.na(Year) == FALSE)

# adjust com for inflation
quantmod::getSymbols("CPIAUCSL", src = "FRED")
avg.cpi <- xts::apply.yearly(CPIAUCSL, mean)
cf <- avg.cpi / as.numeric(avg.cpi["2019"]) # using 2019 as the base year
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

com_catch <- NEesp::com_catch %>%
  dplyr::select(-X, -rowname, -CPIAUCSL)

usethis::use_data(com_catch, overwrite = TRUE)

# archival copy of data wrangleing of raw foss data used to produce the clean dataset

# list.files(here::here("commercial"))

# #load in the stocks managed in NEFSC
# stock_list_all_strata <- read.csv("https://raw.githubusercontent.com/NOAA-EDAB/ECSA/master/data/stock_list.csv")
# #lowercase all of the names
# stock_list_all_strata$common_name <- stringr::str_to_lower(stock_list_all_strata$common_name)
# #extract out the unique names and rename column
# stock_list<-stock_list_all_strata %>% distinct(common_name, .keep_all=TRUE)
#
#
# #load commercial landings data
# com_landings<-readr::read_csv(here::here("data","foss_ commercial_landings_NE_MA_1950_2019.csv"))
# #renaming cols to aling with stocklist
# com_landings<-com_landings %>% rename(common_name=`NMFS Name`)
# #simplifying the names to be lowercase
# com_landings$common_name<-stringr::str_to_lower(com_landings$common_name)
#
# #removing empty rows due to confidentiality and counting the number of records removed
# confid<-com_landings %>%
#   filter(common_name =="withheld for confidentiality" | Confidentiality == "Confidential" )%>%
#   group_by(Year) %>%
#   summarise(count=n())
# com_landings<-com_landings %>% filter(!common_name =="withheld for confidentiality" , !Confidentiality == "Confidential", !is.na(Pounds), !is.na(Dollars), Dollars >1)
#
#
# # removing aggrigated landings records and counting their occurance
# grouped_landings<-com_landings %>% filter(stringr::str_detect(common_name, "\\*+"))
#
# # rearranging the species names to align with NEFSC conventions
# com_landings<-com_landings %>% filter(!stringr::str_detect(common_name, "\\*+"))
#
# #splitting off the records without a comma
#
#
# single_name<-com_landings %>% filter(!stringr::str_detect(common_name, "\\,\\s"))
# multipe_name<-com_landings %>% filter(stringr::str_detect(common_name, "\\,\\s"))
# split_names<-multipe_name%>% separate (col=common_name,sep ="\\,\\s", into = c("first" ,"second", "third"))
# #fixing plaice to remove flounder
# split_names<- split_names %>% dplyr::mutate(first=replace(first, which(second=="american plaice"), NA))
# #fixing the shark tag for dogfish
# split_names<- split_names %>% dplyr::mutate(first=replace(first, which(second=="dogfish"), NA))
# #longfin loligo into inshore
# split_names<- split_names %>% dplyr::mutate(second=replace(second, which(second=="longfin loligo"), "longfin inshore"))
# #shortfin illex into northern shortfin
# split_names<- split_names %>% dplyr::mutate(second=replace(second, which(second=="shortfin illex"), "northern shortfin"))
# #goosefish into monkfish
# single_name<- single_name %>% dplyr::mutate(common_name=replace(common_name, which(common_name=="goosefish"), "monkfish"))
# #adding flounder to windowpane
# single_name<- single_name %>% dplyr::mutate(common_name=replace(common_name, which(common_name=="windowpane"), "windowpane flounder"))
# split_names<-split_names %>% unite(col = "last", c(third, second),sep = " ", remove = TRUE ,na.rm = TRUE)
# split_names<-split_names %>% unite(col = "common_name", c(last, first),sep = " ", remove = TRUE ,na.rm = TRUE)
#
# joined_names<- dplyr::bind_rows(split_names,single_name)
# #used to check that there are no missing stocks in the data
# #missing_stocks<-dplyr::anti_join(stock_list,joined_names, by= "common_name")
#
#
# #final clean dataset
# com_landings_clean<-dplyr::full_join(stock_list,joined_names, by= "common_name")
# write.csv(com_landings_clean, "FOSS_com_landings_clean_20201222.csv")
