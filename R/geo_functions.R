#' Create a map of seasonal species distribution
#'
#' This function creates a map of species distribution. Built on the `ecodata` function `map_strata`.
#'
#' @param data A data frame or tibble, containing data on one species. Must contain season and strata information.
#' @param species_name The common name of the species
#' @return A ggplot
#' @importFrom magrittr %>%
#' @export

map_strata_ecsa <- function(data, species_name) {

  data <- data %>%
    dplyr::filter(is.na(stock_season) == FALSE)

  stock_season <- data$stock_season %>% unique()

  NEesp::map_strata(
    stock_name = species_name,
    common_name = species_name,
    strata = data,
    stock_season = stock_season,
    save_plot = FALSE
  )
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

get_latlong <- function(x, data, shapefile) {
  range_coord <- c()

  data <- dplyr::filter(data, Species == x)

  for (i in unique(data$Region)) {
    for (j in unique(data$stock_season)) {
      data2 <- data %>%
        dplyr::filter(stock_season == j, Region == i)

      if (nrow(data2) > 0) {
        log_statement <- paste("STRATA == ", unique(data2$strata), collapse = " | ")

        strata <- dplyr::filter(shapefile, eval(parse(text = log_statement)))

        temp <- c(i, j, round(sf::st_bbox(strata), digits = 2))

        missing_data <- match(unique(data2$strata), unique(shapefile$STRATA)) %>%
          is.na() %>%
          sum()

        if (missing_data == 0) {
          warning <- "none"
        }
        if (missing_data > 0) {
          warning <- "shapefile is missing some strata data"
        }

        range_coord <- rbind(range_coord, c(temp, warning))
      }
    }
  }

  if (length(range_coord) > 0) {
    colnames(range_coord) <- c(
      "Region", "Season", "Lat_min",
      "Long_min", "Lat_max", "Long_max", "Warning"
    )
  }

  return(range_coord)
}

#' Map strata
#' 
#' Copied from `NOAA-EDAB/ECSA` but with shapefile input modified.
#'
#' @param common_name 
#' @param spring_strata 
#' @param fall_strata 
#' @param overwrite 
#' @param save_plot 
#'
#' @return a ggplot2 object
#' 
#' @importFrom magrittr "%>%"
#' @export
#'
#' @examples
#' spring_strata <- c(1010L, 1020L, 1030L, 1040L, 1050L, 1060L, 1070L, 1080L, 1090L, 
#' 1100L, 1110L, 1120L, 1610L, 1620L, 1630L, 1640L, 1650L, 1660L, 
#' 1670L, 1680L, 1690L, 1700L, 1710L, 1720L, 1730L, 1740L, 1750L, 
#' 1760L)
#' 
#' fall_strata <- c(1010L, 1050L, 1090L, 1610L, 1650L, 1690L, 1730L, 3010L, 3020L, 
#' 3030L, 3040L, 3050L, 3060L, 3070L, 3080L, 3090L, 3100L, 3110L, 
#' 3120L, 3130L, 3140L, 3150L, 3160L, 3170L, 3180L, 3190L, 3200L, 
#' 3210L, 3220L, 3230L, 3240L, 3250L, 3260L, 3270L, 3280L, 3290L, 
#' 3300L, 3310L, 3320L, 3330L, 3340L, 3350L, 3360L, 3370L, 3380L, 
#' 3390L, 3400L, 3410L, 3420L, 3430L, 3440L, 3450L, 3460L, 3470L, 
#' 3480L, 3490L, 3500L, 3510L, 3520L, 3530L, 3540L, 3550L, 3560L, 
#' 3570L, 3580L, 3590L, 3600L, 3610L)
#' 
#' 
#' map_strata(common_name = "summer flounder", spring_strata = spring_strata,
#' fall_strata = fall_strata, overwrite = FALSE, save_plot = FALSE)
#' 
map_strata <- function(stock_name, common_name, stock_season, strata,
                       overwrite = FALSE, save_plot, get_sf = F) {
  
  
#  `%>%` <- magrittr::`%>%`
  
  ## General mapping parameters
  xmin = -77
  xmax = -65
  ymin = 35
  ymax = 45
  
  xlims <- c(xmin, xmax)
  ylims <- c(ymin, ymax)
  crs <-  "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" 
  
  ## Download data layers
  
  ## 1) Strata  
  # source("R/get_strata.R")
  #get_strata(overwrite = overwrite)
  
  ## 2) North America layer
  ne_countries <- rnaturalearth::ne_countries(scale = 10,
                                              continent = "North America",
                                              returnclass = "sf") %>%
    sf::st_crs(value = crs) %>%
    sf::st_transform(crs = crs)
  
  ## 3) State layer
  ne_states <- rnaturalearth::ne_states(country = "united states of america",
                                        returnclass = "sf") %>% 
    sf::st_crs(value = crs) %>%
    sf::st_transform(crs = crs)
  
  
  strata_spring <- strata %>% dplyr::filter(stock_season == "spring") %>% dplyr::pull(strata)
  strata_fall <- strata %>% dplyr::filter(stock_season == "fall") %>% dplyr::pull(strata)
  
  
  if (any(stock_season == "both")){
    strata_both <- strata %>%
      dplyr::filter(stock_season == "both") %>%
      dplyr::mutate(stock_season = "spring and fall") %>% 
      dplyr::pull(strata)
  } else {
    strata_both <- base::intersect(strata_spring, strata_fall)
  }
  
  ### SHellfish Survey species (sea scallop)
  # strata_int <- sf::st_read(here::here("data/strata_shapefiles/shellfish_strata.shp"),
  #                           quiet = TRUE) %>% 
  #   dplyr::mutate(both = dplyr::case_when(STRATUM %in% strata_both ~ "spring and fall", TRUE ~ NA_character_),
  #                 spring = dplyr::case_when(STRATUM %in% strata_spring ~ "spring", TRUE ~ NA_character_),
  #                 fall = dplyr::case_when(STRATUM %in% strata_fall ~ "fall" ,TRUE ~ NA_character_),
  #                 SEASON = dplyr::case_when(STRATUM %in% base::intersect(strata_spring, strata_fall) ~ "spring and fall",
  #                                           STRATUM %in% strata_both ~ "spring and fall",
  #                                           STRATUM %in% strata_spring ~ "spring",
  #                                           STRATUM %in% strata_fall ~ "fall",
  #                                           TRUE ~ NA_character_)) %>% 
  #   # dplyr::filter(!is.na(SEASON)) %>% 
  #   dplyr::select(SEASON, both, fall, spring, geometry)
  
  ### BTS species 
  #strata_int <- sf::st_read(here::here("data/strata_shapefiles/BTS_Strata.shp"),
  #                          quiet = TRUE) %>%
  strata_int <- NEesp::shape %>%
    dplyr::mutate(both = dplyr::case_when(STRATA %in% strata_both ~ "spring and fall", TRUE ~ NA_character_),
                  spring = dplyr::case_when(STRATA %in% strata_spring ~ "spring", TRUE ~ NA_character_),
                  fall = dplyr::case_when(STRATA %in% strata_fall ~ "fall" ,TRUE ~ NA_character_),
                  SEASON = dplyr::case_when(STRATA %in% base::intersect(strata_spring, strata_fall) ~ "spring and fall",
                                            STRATA %in% strata_both ~ "spring and fall",
                                            STRATA %in% strata_spring ~ "spring",
                                            STRATA %in% strata_fall ~ "fall",
                                            TRUE ~ NA_character_)) %>%
    # dplyr::filter(!is.na(SEASON)) %>%
    dplyr::select(SEASON, both, fall, spring, geometry)
  
  #For export
#  strata_grid <- strata_int %>% 
#    dplyr::select(-SEASON) %>% 
#    dplyr::filter_at(vars(both, fall, spring), any_vars(!is.na(.)))
  
  if (!get_sf){
    #For plotting
    strata_plot <- strata_int %>% 
      dplyr::select(SEASON, geometry) %>% 
      dplyr::filter(!is.na(SEASON))
    
    
    p1 <- ggplot2::ggplot() +
      ggplot2::geom_sf(data = strata_int, fill = "white", alpha = 0.9, size = 0.01, color = "grey30") +
      ggplot2::geom_sf(data = strata_plot, ggplot2::aes(fill = SEASON), size = 0.05, color = "grey30") +
      ggplot2::geom_sf(data = ne_countries, color = "grey60", size = 0.25) +
      ggplot2::geom_sf(data = ne_states, color = "grey60", size = 0.05) +
      viridis::scale_fill_viridis(discrete = TRUE) +
      ggplot2::coord_sf(crs = crs, xlim = xlims, ylim = ylims) +
      ggthemes::theme_map() +
      ggplot2::labs(title = sprintf("%s", common_name),
                    
                    fill = "Season") +
      ggplot2::theme(legend.position = "bottom",
                     legend.key.width = ggplot2::unit(2, "cm"))
    
    if(save_plot) {
      ggplot2::ggsave(p1, sprintf("%s_strata-map.png", stock_name), type = "cairo")
    }
    return(p1)
  } else {
   # return(strata_grid)
  }
  
} 
