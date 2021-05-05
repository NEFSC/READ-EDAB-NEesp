# used to convert the .mat file into well behaved data frame

# copied from main ----

# ich.data.raw<-readMat(here::here("Data","ichthyoplankton_all_species.mat"))
# ich.data<-data.frame(matrix(unlist(ich.data.raw), nrow=length(ich.data.raw$taxa), ncol = (length(ich.data.raw))))
# ich.names<-c("taxa" ,"year", "season", "strata" ,"lat", "lon" ,"area" ,"mean.abund","rel.proportion")
# colnames(ich.data)<-ich.names
# ich.data <- ich.data %>%   mutate_at(vars(-taxa) ,as.numeric)
# Save the data to a useful format

# saveRDS(ich.data, file="ichthyoplankton_MARMAP_ECOMON.rds")

# January-February, March-April, May-June, July-August, September-October, and November-December

# make data ----
`%>%` <- magrittr::`%>%`
ich.data <- readRDS(here::here("data-raw", "ichthyoplankton_MARMAP_ECOMON.RDS"))

# recode season as month for readability
# January-February, March-April, May-June, July-August, September-October, and November-December)

ich.data <- ich.data %>% dplyr::mutate(season.month = dplyr::recode_factor(season, "1" = "January-February", "2" = "March-April", "3" = "May-June", "4" = "July-August", "5" = "September-October", "6" = "November-December"))

stock_list_all_strata <- read.csv("https://raw.githubusercontent.com/NOAA-EDAB/ECSA/master/data/stock_list.csv")

# restructure the taxa to join with common name data set
ich.data <- ich.data %>% dplyr::mutate(sci_name = stringr::str_replace(taxa, "_", " "))
ich.data <- ich.data %>% dplyr::mutate(sci_name = stringr::str_to_sentence(sci_name))

ich.data.common <- dplyr::left_join(ich.data, stock_list_all_strata, by = "sci_name")
head(ich.data.common)

ichthyo <- ich.data.common %>%
  dplyr::select(-X) %>%
  dplyr::mutate(common_name = common_name %>%
                  stringr::str_to_sentence() %>%
                  stringr::str_replace("Yellowtail flounder", "Yellowtail")) %>%
  dplyr::filter(!is.na(common_name)) # remove entries with no fish species associated
head(ichthyo)

usethis::use_data(ichthyo, overwrite = TRUE)
