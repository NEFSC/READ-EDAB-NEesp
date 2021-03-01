source(here::here("R/full_report_functions", "get_survdat_info.R"))

get_len_data_risk <- function(x){
  
  y <- x %>% 
    dplyr::filter(LENGTH > 0, ABUNDANCE > 0) %>%
    dplyr::select(Species, YEAR, SEASON, Region, fish_id, LENGTH, NUMLEN) %>%
    dplyr::distinct() %>% # problem with repeat rows
    dplyr::group_by(Species, YEAR, SEASON, Region) %>%
    dplyr::mutate(n_fish = sum(NUMLEN)) %>%
    dplyr::filter(n_fish > 10) # only year-season-region with >10 fish
  
  y <- y %>% 
    dplyr::group_by(Species, YEAR, SEASON, Region) %>%
    dplyr::summarise(mean_len = sum(LENGTH*NUMLEN)/sum(NUMLEN),
                     min_len = min(LENGTH),
                     max_len = max(LENGTH)
    )
  
  return(y)
}



