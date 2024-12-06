## Create Spatial Indicator Functions

#' Create Bottom Temperature Indicator
#'
#' This function generates a bottom temperature indicator from an input data frame with GLORYS data to an R object
#' Prior to running function, should convert GLORYS netCDF files to data frames using 'convert_netcdf_raster_df.R' in this package.
#' @param data Dataframe name or filepath to the indicator data
#' @param shape Spatial shape file input
#' @param columns Number of columns of bottom temperature data. Column titles will be in the format of "X1990.01.01.12.00.00" with date and time corresponding to each month. 
#' @param return Boolean. Whether to return the indicator as an object in the global environment\
#' @return Saves R object `bottomT`, returns bottom temperature indicator
#' @importFrom magrittr `%>%`
#' @export
#'
`%>%` <- magrittr::`%>%`

create_bottomT <- function(data, columns, shape, return = TRUE){
  bottomT <- data %>%
    tidyr::pivot_longer(cols=c(columns),
                        names_to='date',
                        values_to='bottomT') %>%
    dplyr::mutate(year = stringr::str_sub(date, 2, 5), month = stringr::str_sub(date, 7, 8) |> as.numeric()) %>%
    subset(select = -c(date) ) %>%
    dplyr::rename(Longitude = 'x') %>%
    dplyr::rename(Latitude = 'y') %>%
    dplyr::mutate(Units = c ('degC')) %>%
  
  return(bottomT)
}

#bottomT <- create_bottomT(JanFeb, 1:4) 

#' Create Sea Surface Temperature Indicator
#'
#' This function generates a sea surface temperature indicator from an input data frame with ERDDAP data to an R object
#' Prior to running function, should convert ERDDAP netCDF files to data frames using 'convert_netcdf_raster_df.R' in this package.
#' @param data Dataframe name or filepath to the indicator data
#' @param shape Spatial shape file input
#' @param year Data year(s)
#' @param return Boolean. Whether to return the indicator as an object in the global environment\
#' @return Saves R object `seasurfacetemp`, returns sea surface temperature indicator
#' @importFrom magrittr `%>%`
#' @export
#'
`%>%` <- magrittr::`%>%`

create_sst <- function(data, year, shape, return = TRUE){
  seasurfacetemp <- data %>%
    dplyr::rename(Longitude = 'x') %>%
    dplyr::rename(Latitude = 'y') %>%
    dplyr::rename(January = 1, February = 2, March = 3, April = 4,
                  May = 5, June = 6, July = 7, August = 8,
                  September = 9, October = 10, November = 11, December = 12) %>%
    dplyr::mutate(Year = c (year)) %>%
    dplyr::mutate(Units = c ('degC')) %>%
  
  return(seasurfacetemp)
}

#sst <- create_sst(sst_1989, 1989)

#' Create Salinity Indicator
#'
#' This function generates a salinity indicator from an input data frame with GLORYS data to an R object
#' Prior to running function, should convert GLORYS netCDF files to data frames using 'convert_netcdf_raster_df.R' in this package.
#' Column names are depth contours in meters.
#' @param data Dataframe name or filepath to the indicator data
#' @param shape Spatial shape file input
#' @param year Data year(s)
#' @param month Data month(s)
#' @param return Boolean. Whether to return the indicator as an object in the global environment\
#' @return Saves R object `sal`, returns salinity indicator
#' @importFrom magrittr `%>%`
#' @export
#'
`%>%` <- magrittr::`%>%`

create_sal <- function(data, year, month, shape, return = TRUE){
  sal <- data %>%
    dplyr::rename(Longitude = 'x') %>%
    dplyr::rename(Latitude = 'y') %>%
    dplyr::mutate(Year = c (year)) %>%
    dplyr::mutate(Month = c(month)) %>%
    dplyr::mutate(Units = c ('psu')) %>%
    purrr::discard(~all(is.na(.)))
    
    return(sal)
}

#sal <- create_sal(sal_1993, 1993, 'January') 

#' Create Chlorophyll-a Indicator
#'
#' This function generates a chlorophyll-a indicator from an input data frame with OCCCI ERDDAP data to an R object
#' Prior to running function, should convert ERDDAP netCDF files to data frames using 'convert_netcdf_raster_df.R' in this package.
#' @param data Dataframe name or filepath to the indicator data
#' @param shape Spatial shape file input
#' @param year Data year(s)
#' @param return Boolean. Whether to return the indicator as an object in the global environment
#' @return Saves R object `chl`, returns chlorophyll-a indicator
#' @importFrom magrittr `%>%`
#' @export
#'
`%>%` <- magrittr::`%>%`

create_chl <- function(data, year, shape, return = TRUE){
  chl <- data %>%
    dplyr::rename(Longitude = 'x') %>%
    dplyr::rename(Latitude = 'y') %>%
    dplyr::rename(January = 1, February = 2, March = 3, April = 4,
                  May = 5, June = 6, July = 7, August = 8,
                  September = 9, October = 10, November = 11) %>%
    dplyr::mutate(Year = c (year)) %>%
    dplyr::mutate(Units = c ('mg m^-3')) %>%
    
    return(chl)
}

#chl <- create_chl(chl_2017, 2017)

#' Create Primary Production (mean) Indicator
#'
#' This function generates a primary production indicator from an input data frame with OCCCI ERDDAP data to an R object
#' Prior to running function, should convert ERDDAP netCDF files to data frames using 'convert_netcdf_raster_df.R' in this package.
#' @param data Dataframe name or filepath to the indicator data
#' @param shape Spatial shape file input
#' @param year Data year(s)
#' @param return Boolean. Whether to return the indicator as an object in the global environment
#' @return Saves R object `pp`, returns primary production indicator
#' @importFrom magrittr `%>%`
#' @export
#'
`%>%` <- magrittr::`%>%`

create_pp <- function(data, year, shape, return = TRUE){
  pp <- data %>%
    dplyr::rename(Longitude = 'x') %>%
    dplyr::rename(Latitude = 'y') %>%
    dplyr::mutate(Units = c ('mg m^-3')) %>%
    
    return(pp)
}