`%>%` <- magrittr::`%>%`

## recreational catch ----
files <- dir(here::here("data-raw/MRIP"))
read_files <- files[stringr::str_detect(files, "catch_year") %>% which()]
col_to_keep <- read.csv(here::here("data-raw/MRIP", read_files[1])) %>%
  colnames()

big_data <- c()
for (i in 1:length(read_files)) {
  this_data <- read.csv(here::here("data-raw/MRIP", read_files[i])) %>%
    tibble::as_tibble() %>%
    dplyr::select(col_to_keep)
  big_data <- rbind(big_data, this_data)
}
head(big_data)
tail(big_data)

big_data$tot_cat <- stringr::str_replace_all(big_data$tot_cat, ",", "") # %>%
as.numeric()
big_data$Species <- stringr::str_to_sentence(big_data$common)

write.csv(big_data, file = here::here("data-raw/MRIP", "all_MRIP_catch_year.csv"))

rec_catch <- read.csv(here::here("data-raw/MRIP", "all_MRIP_catch_year.csv")) %>%
  dplyr::filter(sub_reg_f == "NORTH ATLANTIC" |
    sub_reg_f == "MID-ATLANTIC") %>%
  dplyr::mutate(lbs_ab1 = lbs_ab1 %>%
    stringr::str_replace_all(",", "") %>%
    as.numeric()) %>%
  update_species_names(species_col = "Species")

usethis::use_data(rec_catch)

rec_catch_small <- NEesp::rec_catch %>%
  dplyr::select(-X, -status, -wave, -wave_f, -sub_reg, -st, -sp_code, -common,
                -mode_fx, -area_x, -area_x_f, -estclaim, -estclvar, -estharv,
                -landing, -land_var, -estrel, -estrlvar, -var_lbs, -wgt_ab1,
                -var_wab1, -miss_fish, -ALT_FLAG, -esthvar)

usethis::use_data(rec_catch_small)

# data wrangling - archival ----
#effort data
rec_effort<-read.csv(here::here("data","mrip_effort_1981_2020_rec_trips_MA_NE.csv"), stringsAsFactors = T, fileEncoding = 'UTF-8-BOM')
#read in recreational landings data
rec_landings_1950_2019<-readr::read_csv(here::here("data","foss_landings_REC_1950_2019_MA_NE.csv"))
rec_landings_1950_2019<-rec_landings_1950_2019 %>% dplyr::rename( "common_name"= "NMFS Name")
rec_landings_1950_2019<-as.data.frame(rec_landings_1950_2019)
#load in the stocks managed in NEFSC
stock_list_all_strata <- read.csv("https://raw.githubusercontent.com/NOAA-EDAB/ECSA/master/data/stock_list.csv")
#lowercase all of the names
stock_list_all_strata$common_name <- stringr::str_to_lower(stock_list_all_strata$common_name)
#extract out the unique names and rename column  
stock_list<-as_tibble(unique(stock_list_all_strata$common_name),column_name = "common_name")
stock_list<-stock_list %>% dplyr::rename(common_name= value)
#summarize the catches to species and year 
rec_by_sp<-rec_landings_1950_2019 %>% group_by(common_name,Year) %>% summarise(total= sum(Pounds), sd = sd(Pounds))
#lower case the name for easier comparisons
rec_by_sp$common_name<-stringr::str_to_lower(rec_by_sp$common_name)
rec_landings_1950_2019$common_name<-stringr::str_to_lower(rec_landings_1950_2019$common_name)
#split out the records with commas to be re arranged
rec_w_comma<-rec_by_sp[str_detect(rec_by_sp$common_name, "\\,\\s"),]
rec_landings_1950_2019_comma<-rec_landings_1950_2019[str_detect(rec_landings_1950_2019$common_name, "\\,\\s"),]
# detect the comma space formatting and separate out each term 
rec_w_comma_split<-rec_w_comma %>% separate(col=common_name,sep ="\\,\\s", into = c("first" ,"second", "third"))
rec_landings_1950_2019_comma_split<-rec_landings_1950_2019_comma %>% separate(col=common_name,sep ="\\,\\s", into = c("first" ,"second", "third"))
#reorder the tibble to match NEFSC names
rec_w_comma_split<-rec_w_comma_split %>% select( third, second, first,Year, total ,sd)
rec_landings_1950_2019_comma_split<-rec_landings_1950_2019_comma_split %>% select(third, second, first, Year, State, Pounds, Dollars, Collection, Confidentiality)
########################## renaming rows to align with NEFSC naming 
#removing the shark tag from the first name term
rec_w_comma_split<- as.data.frame(rec_w_comma_split)
rec_w_comma_split[str_detect(rec_w_comma_split$first, "shark"),3]<-NA
#changing tibble back to df to simplify the string replacement
rec_landings_1950_2019_comma_split[str_detect(rec_landings_1950_2019_comma_split$first, "shark"),3]<-NA
# fixing flounder plaice 
rec_w_comma_split[str_detect(rec_w_comma_split$second, "plaice"),3]<-NA
rec_landings_1950_2019_comma_split[str_detect(rec_landings_1950_2019_comma_split$second, "plaice"),3]<-NA
#used to check for near matches 
#fish_match<-rec_w_comma_whole[str_detect(rec_w_comma_whole$common_name, "herring"),]
rec_w_comma_union<-rec_w_comma_split %>% unite(col = "firstname", c(third, second),sep = " ", remove = TRUE ,na.rm = TRUE)
rec_w_comma_whole<-rec_w_comma_union %>% unite(col = "common_name", c(firstname, first ),sep = " ", remove = TRUE ,na.rm = TRUE)
rec_landings_1950_2019_comma_u<-rec_landings_1950_2019_comma_split %>% unite(col = "firstname", c(third, second),sep = " ", remove = TRUE ,na.rm = TRUE)
rec_landings_1950_2019_comma_w<-rec_landings_1950_2019_comma_u %>% unite(col = "common_name", c(firstname, first ),sep = " ", remove = TRUE ,na.rm = TRUE)
#bring back in the records of single name fishes
rec_w_o_comma<-rec_by_sp[!str_detect(rec_by_sp$common_name, "\\,\\s"),]
rec_landings_1950_2019<-as.data.frame(rec_landings_1950_2019)
rec_landings_1950_2019_w_o_comma<-rec_landings_1950_2019[!str_detect(rec_landings_1950_2019$common_name, "\\,\\s"),]
# rename col names to merge with the corrected dataset
colnames(rec_w_o_comma)<-c("common_name","Year","total","sd")
rec_by_sp_whole<-dplyr::bind_rows(rec_w_o_comma,rec_w_comma_whole)
#common_name ,Year, State, Pounds, Dollars, Collection, Confidentiality
rec_landings_1950_2019_w_o_comma<-rec_landings_1950_2019_w_o_comma %>% dplyr::select("common_name" ,"Year", "State", "Pounds", "Dollars", "Collection", "Confidentiality")
rec_landings_1950_2019_whole<-dplyr::bind_rows(rec_landings_1950_2019_w_o_comma,rec_landings_1950_2019_comma_w)
#fixing windowpane
rec_by_sp_whole[stringr::str_detect(rec_by_sp_whole$common_name, "window"),1]<-"windowpane flounder"
rec_landings_1950_2019_whole[stringr::str_detect(rec_landings_1950_2019_whole$common_name,"window"),1 ]<-"windowpane flounder"
#check how many matches already exist 
#there are 42 species in NEFSC stock list
#length(stock_list$common_name)
stock_semi<-semi_join(stock_list, rec_by_sp_whole, by = "common_name") 
stock_missing<-anti_join(stock_list, stock_semi, by = "common_name") 
#length(stock_missing$common_name)
#the missing stocks 
#  1 atlantic hagfish no records     
#  2 offshore hake    multiple species, ECSA uses red and silver hake, not sure what to do with other species  
#fish_match<-rec_w_comma_whole[str_detect(rec_w_comma_whole$common_name, "hake"),]
# 3 witch flounder  no records       
# 4 monkfish no records              
# 5 american lobster no records      
# 6 northern shrimp   no records     
# 7 northern shortfin squid no records
# 8 longfin inshore squid  
#final dataset is 
# rec_landings_clean<-rec_by_sp_whole
rec_landings_1950_2019_clean <-rec_landings_1950_2019_whole %>% mutate(common_name=stringr::str_to_sentence(common_name))
#write.csv(rec_landings_1950_2019_clean, "rec_landings_1950_2019_tidy.csv")
# rm(list =c("rec_by_sp", "rec_landings_1950_2019", "rec_landings_1950_2019_comma", "rec_landings_1950_2019_comma_split" "rec_w_comma", "rec_w_comma_split", "stock_list", "stock_list_all_strata") )

