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
#  data <- data %>%
#    dplyr::filter(stock_season == "spring" | stock_season == "fall")

  stock_season <- data$stock_season %>% unique()

#  if (length(stock_season) > 1) {
#    stock_season <- "all"
#  }

  NEesp::map_strata(
    common_name = species_name,
    strata = data,
    stock_season = stock_season
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
#' Derived from `NOAA-EDAB/ECSA`.
#'
#' @param common_name
#' @param spring_strata
#' @param fall_strata
#' @param overwrite
#' @param save_plot
#'
#' @param stock_season A vector of season names (fall, winter, and/or spring)
#'
#' @return a ggplot2 object
#'
#' @importFrom magrittr %>%
#' @export

map_strata <- function(common_name, stock_season, strata) {

  ## General mapping parameters
  xmin <- -77
  xmax <- -65
  ymin <- 35
  ymax <- 45

  xlims <- c(xmin, xmax)
  ylims <- c(ymin, ymax)
  crs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

  ## Download data layers

  ## 2) North America layer
  ne_countries <- rnaturalearth::ne_countries(
    scale = 10,
    continent = "North America",
    returnclass = "sf"
  ) %>%
    sf::st_transform()

  #  sf::st_crs(ne_countries) <- crs

  #  ne_countries <- ne_countries %>%
  #    sf::st_transform(crs = crs)

  ## 3) State layer
  ne_states <- rnaturalearth::ne_states(
    country = "united states of america",
    returnclass = "sf"
  ) %>%
    sf::st_transform()

  #  sf::st_crs(ne_states) <- crs

  #  ne_states <- ne_states %>%
  #    sf::st_transform(crs = crs)

  # strata
  strata_spring <- strata %>%
    dplyr::filter(stock_season == "spring") %>%
    dplyr::select(strata, stock_season) %>%
    dplyr::rename(spring = stock_season)
  
  strata_fall <- strata %>%
    dplyr::filter(stock_season == "fall") %>%
    dplyr::select(strata, stock_season) %>%
    dplyr::rename(fall = stock_season)
  
  strata_winter <- strata %>%
    dplyr::filter(stock_season == "winter") %>%
    dplyr::select(strata, stock_season) %>%
    dplyr::rename(winter = stock_season)

  # overlapping strata
  all_season <- dplyr::full_join(strata_spring, strata_fall,
    by = "strata"
  )
  
  all_season <- dplyr::full_join(all_season, strata_winter, by = "strata")

  all_season <- all_season[, colSums(is.na(all_season)) < nrow(all_season)] # Remove rows with NA only

  label <- all_season %>%
    dplyr::select(-strata)

  labelv <- c()
  for (i in 1:nrow(label)) {
    labelv[i] <- paste(label[i, ], collapse = ", ")
  }

  all_season$label <- labelv
  
  all_season <- all_season %>%
    dplyr::select(strata, label) %>%
    dplyr::rename(STRATA = strata) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(label = label %>%
                    stringr::str_replace_all(", NA", "") %>%
                    stringr::str_replace_all("NA, ", ""))
  
  # For plotting
  new_shape <- NEesp::shape %>%
    dplyr::select(STRATA, geometry) %>%
    sf::st_transform()
  
  sf::st_crs(new_shape) <- crs
  
#  sf::st_crs(new_shape) <- crs

#  print(new_shape)
  
#  print(all_season)
  
  strata_plot <- dplyr::full_join(new_shape, all_season, by = "STRATA") %>%
    dplyr::rename(SEASON = label) %>%
 #   dplyr::select(SEASON, geometry) %>%
    dplyr::filter(!is.na(SEASON))

  p1 <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = new_shape, # all trawl shape files (light outlines)
      fill = "white",
      alpha = 0.9,
      size = 0.01,
      color = "grey30"
    ) +
    ggplot2::geom_sf(
      data = strata_plot, # occupied trawl shape files (filled with color)
      ggplot2::aes(fill = SEASON),
      size = 0.05,
      color = "grey30"
    ) +
    ggplot2::geom_sf(
      data = ne_countries,
      color = "grey60",
      size = 0.25
    ) +
    ggplot2::geom_sf(
      data = ne_states,
      color = "grey60",
      size = 0.05
    ) +
    viridis::scale_fill_viridis(discrete = TRUE) +
    ggplot2::coord_sf(
      crs = crs,
      xlim = xlims,
      ylim = ylims
    ) +
    ggthemes::theme_map() +
    ggplot2::labs(
      title = common_name,
      fill = "Season"
    ) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.key.width = ggplot2::unit(2, "cm")
    )

  return(p1)
}
