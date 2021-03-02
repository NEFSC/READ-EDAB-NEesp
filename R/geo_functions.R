#' Create a map of seasonal species distribution
#'
#' This function creates a map of species distribution. Built on the `ecodata` function `map_strata`.
#' 
#' @param data A data frame or tibble, containing data on one species. Must contain season and strata information.
#' @param species_name The common name of the species
#' @return A ggplot
#' @importFrom magrittr %>%
#' @export

map_strata_ecsa <- function(data, species_name){
  
  # not good to use source in a package, but not sure how else to include this function 
  # it is hidden in the ecodata package
  
  source("https://raw.githubusercontent.com/NOAA-EDAB/ECSA/master/R/map_strata.R")
  
  data <- data %>%
    dplyr::filter(is.na(stock_season) == FALSE)
  
  stock_season <- data$stock_season %>% unique()
  
  map_strata(stock_name = species_name, 
             common_name = species_name,
             strata = data,
             stock_season = stock_season,
             save_plot = FALSE)
}

#' Create a summary table of species range
#'
#' This function creates a summary table of species distribution (latitude/longitude range). 
#' 
#' @param data A data frame or tibble, containing data on one species. Must contain season and strata information.
#' @param x The common name of the species.
#' @param shapefile A shapefile to convert strata ID to a shape.
#' @return A ggplot
#' @importFrom magrittr %>%
#' @export

get_latlong <- function(x, data, shapefile){
  
  range_coord <- c()
  
  data <- dplyr::filter(data, Species == x)
  
  for(i in unique(data$Region)){
    
    for(j in unique(data$stock_season)){
      
      data2 <- data %>% 
        dplyr::filter(stock_season == j, Region == i)
      
      if(nrow(data2) > 0){
        
        log_statement <- paste("STRATA == ", unique(data2$strata), collapse = " | ")
        
        strata <- dplyr::filter(shapefile, eval(parse(text = log_statement)))
        
        temp <- c(i, j, round(sf::st_bbox(strata), digits = 2))
        
        missing_data <- match(unique(data2$strata), unique(shapefile$STRATA)) %>% 
          is.na() %>% 
          sum()
        
        if(missing_data == 0) {warning <- "none"}
        if(missing_data > 0) {warning <- "shapefile is missing some strata data"}
        
        range_coord <- rbind(range_coord, c(temp, warning))
        
      }
    }
  }
  
  if(length(range_coord) > 0) {
    colnames(range_coord) <- c("Region", "Season", "Lat_min", 
                               "Long_min", "Lat_max", "Long_max", "Warning")
  }
  
  return(range_coord)
}