## Create Ecodata Indicator Functions

#' Create Cold Pool Index
#'
#' This function generates a cold pool index indicator from ecodata R package to an R object
#' @param data Filepath to the indicator data
#' @param return Boolean. Whether to return the indicator as an object in the global environment
#' @return Saves R object `coldpool_index`, returns cold pool index indicator
#' @importFrom magrittr `%>%`
#' @export
#'
#'Cold pool index: quantifies the interannual strength of the cold pool
#'Positive values == colder 
#'Negative values == warmer
#'
create_coldpool_index <- function(data){
  coldpool_index <- data |>
    dplyr::filter(Var == 'cold_pool_index') %>%
    dplyr::rename(Year = Time)
  
  return(coldpool_index)
}
cp_index <- create_coldpool_index(ecodata::cold_pool)


#' Create Cold Pool Extent
#'
#' This function generates a cold pool extent indicator from ecodata R package to an R object
#' @param data Filepath to the indicator data
#' @param return Boolean. Whether to return the indicator as an object in the global environment
#' @return Saves R object `coldpool_extent`, returns cold pool extent indicator
#' @importFrom magrittr `%>%`
#' @export
#'
#'Cold pool extent:  total area where bottom temperatures remain below 10 °C for at least 2 months between June and September
#'Positive values == Larger
#'Negative values == Smaller
#'
create_coldpool_extent <- function(data){
  coldpool_extent <- data |>
    dplyr::filter(Var == 'extent_index') %>%
    dplyr::rename(Year = Time)
  
  return(coldpool_extent)
}
cp_extent <- create_coldpool_extent(ecodata::cold_pool)


#' Create Cold Pool Persistence
#'
#' This function generates a cold pool persistence indicator from ecodata R package to an R object
#' @param data Filepath to the indicator data
#' @param return Boolean. Whether to return the indicator as an object in the global environment
#' @return Saves R object `coldpool_persistence`, returns cold pool persistence indicator
#' @importFrom magrittr `%>%`
#' @export
#'
#'Cold pool persistence: duration of the cold pool, which ends when bottom temperature rises above 10 °C after it is formed each year
#'Positive values == Longer
#'Negative values == Shorter
#'
create_coldpool_persistence <- function(data){
  coldpool_persistence <- data |>
    dplyr::filter(Var == 'persistence_index') %>%
    dplyr::rename(Year = Time)
  
  return(coldpool_persistence)
}
cp_persistence <- create_coldpool_persistence(ecodata::cold_pool)


#' Create Gulf stream Index
#'
#' This function generates a Gulf Stream index indicator from ecodata R package to an R object
#' @param data Filepath to the indicator data
#' @param return Boolean. Whether to return the indicator as an object in the global environment
#' @return Saves R object `gsi`, returns Gulf Stream Index indicator
#' @importFrom magrittr `%>%`
#' @export
#'
#'Gulf Stream Index: measure of the Gulf Stream position relative to the mean position
#'Positive values == More northerly
#'Negative values == More southerly
create_gsi <- function(data){
  gsi <- data |>
    tidyr::separate(Time, c("Year", "Month"), sep = "\\.")
  
  return(gsi)
}
gsi <- create_gsi(ecodata::gsi)


#' Create Warm Core Rings
#'
#' This function generates a warm core rings indicator from ecodata R package to an R object
#' @param data Filepath to the indicator data
#' @param return Boolean. Whether to return the indicator as an object in the global environment
#' @return Saves R object `wcr`, returns warm core rings indicator
#' @importFrom magrittr `%>%`
#' @export
#'
#'Warm Core Rings: number of warm core ring formations
create_wcr <- function(data){
  wcr <- data |>
    dplyr::rename(Year = Time)
  
  return(wcr)
}
wcr <- create_wcr(ecodata::wcr)
