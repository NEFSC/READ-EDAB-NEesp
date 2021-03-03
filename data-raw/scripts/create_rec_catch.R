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
