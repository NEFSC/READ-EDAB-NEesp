## Create Spatial Indicator Functions

#' Create Bottom Temperature Indicator
#'
#' This function generates a bottom temperature indicator from a GLORYS netCDF file. 
#' The function passes data through EDABUtilities::make_2d_summary_ts, which provides summary statistics of 2d gridded data as time series by area.
#' Converts .nc files to data frame.
#' @param data.in Either a character vector of full input file names for a list of spatRasters
#' @param output.files character vector of full output file names corresponding to each input file
#' @param shp.file  string. Shape file you wish to crop each input file to
#' @param var.name string. Variable name you wish to extract = 'bottomT'
#' @param area.names character vector. Names of shape file areas you want to summarize. 
#' @param statistic string. Which statistic to calculate = 'mean'
#' @param agg.time character. Time scale to calculate over (days, doy, months, season, or years)
#' @param tz string. Time zone to convert. No correction if NA
#' @param touches logical. If TRUE, all cells touched by lines or polygons will be masked, not just those on the line render path, or whose center point is within the polygon
#' @param write.out logical. If TRUE, will write a netCDF file with output.files. If FALSE will return a list of spatRasters
#' @return Saves R object `bottomT`, returns bottom temp indicator in a data frame summarized by timestep for each area.names
#' @importFrom magrittr "%>%"
#' @export
#'
`%>%` <- magrittr::`%>%`

create_bottomT <- function(...) { 
  make_2d_summary_output <- EDABUtilities::make_2d_summary_ts(...) 
  df <- make_2d_summary_output %>%
    terra::as.data.frame(na.rm = FALSE) 
  bottomT <- df %>%
    dplyr::mutate(time = as.Date(time),
                  day = lubridate::day(time), month = lubridate::month(time), year = lubridate::year(time)) %>%
    dplyr::mutate(Units = c ('degC')) %>%
    subset(select = -c(agg.time, time) ) 
  
  return(bottomT)
}

#' Create Sea Surface Temperature Indicator
#'
#' This function generates a sea surface temperature indicator from an OISST ERDDAP netCDF file. 
#' The function passes data through EDABUtilities::make_2d_summary_ts, which provides summary statistics of 2d gridded data as time series by area.
#' Converts .nc files to data frame.
#' @param data.in Either a character vector of full input file names for a list of spatRasters
#' @param output.files character vector of full output file names corresponding to each input file
#' @param shp.file  string. Shape file you wish to crop each input file to
#' @param var.name string. Variable name you wish to extract = 'sst'
#' @param area.names character vector. Names of shape file areas you want to summarize. 
#' @param statistic string. Which statistic to calculate = 'mean'
#' @param agg.time character. Time scale to calculate over (days, doy, months, season, or years)
#' @param tz string. Time zone to convert. No correction if NA
#' @param touches logical. If TRUE, all cells touched by lines or polygons will be masked, not just those on the line render path, or whose center point is within the polygon
#' @param write.out logical. If TRUE, will write a netCDF file with output.files. If FALSE will return a list of spatRasters
#' @return Saves R object `sst`, returns sea surface temp indicator in a data frame summarized by timestep for each area.names
#' @importFrom magrittr "%>%"
#' @export
#'
`%>%` <- magrittr::`%>%`

create_sst <- function(...) { 
  make_2d_summary_output <- EDABUtilities::make_2d_summary_ts(...) 
  df <- make_2d_summary_output %>%
    terra::as.data.frame(na.rm = FALSE) 
  sst <- df %>%
    dplyr::mutate(time = as.Date(time),
                  day = lubridate::day(time), month = lubridate::month(time), year = lubridate::year(time)) %>%
    dplyr::mutate(Units = c ('degC')) %>%
    subset(select = -c(agg.time, time) ) 
  
  return(sst)
}

#' Create Salinity Indicator
#'
#' This function generates a salinity indicator from a GLORYS netCDF file. 
#' The function passes data through EDABUtilities::make_2d_summary_ts, which provides summary statistics of 2d gridded data as time series by area.
#' Converts .nc files to data frame.
#' @param data.in Either a character vector of full input file names for a list of spatRasters
#' @param output.files character vector of full output file names corresponding to each input file
#' @param shp.file  string. Shape file you wish to crop each input file to
#' @param var.name string. Variable name you wish to extract = 'sal'
#' @param area.names character vector. Names of shape file areas you want to summarize. 
#' @param statistic string. Which statistic to calculate = 'mean'
#' @param agg.time character. Time scale to calculate over (days, doy, months, season, or years)
#' @param tz string. Time zone to convert. No correction if NA
#' @param touches logical. If TRUE, all cells touched by lines or polygons will be masked, not just those on the line render path, or whose center point is within the polygon
#' @param write.out logical. If TRUE, will write a netCDF file with output.files. If FALSE will return a list of spatRasters
#' @return Saves R object `sal`, returns salinity indicator in a data frame summarized by timestep for each area.names
#' @importFrom magrittr "%>%"
#' @export
#'
`%>%` <- magrittr::`%>%`

create_sal <- function(...) { 
  make_2d_summary_output <- EDABUtilities::make_2d_summary_ts(...) 
  df <- make_2d_summary_output %>%
    terra::as.data.frame(na.rm = FALSE) 
  sal <- df %>%
    dplyr::mutate(time = as.Date(time),
                  day = lubridate::day(time), month = lubridate::month(time), year = lubridate::year(time)) %>%
    subset(select = -c(agg.time, time))  %>%
    purrr::discard(~all(is.na(.)))
  
  return(sal)
}

#' Create Chlorophyll-a Indicator
#'
#' This function generates a chlorophyll-a indicator from an OCCCI ERDDAP netCDF file. 
#' The function passes data through EDABUtilities::make_2d_summary_ts, which provides summary statistics of 2d gridded data as time series by area.
#' Converts .nc files to data frame.
#' @param data.in Either a character vector of full input file names for a list of spatRasters
#' @param output.files character vector of full output file names corresponding to each input file
#' @param shp.file  string. Shape file you wish to crop each input file to
#' @param var.name string. Variable name you wish to extract = 'chl'
#' @param area.names character vector. Names of shape file areas you want to summarize. 
#' @param statistic string. Which statistic to calculate = 'mean'
#' @param agg.time character. Time scale to calculate over (days, doy, months, season, or years)
#' @param tz string. Time zone to convert. No correction if NA
#' @param touches logical. If TRUE, all cells touched by lines or polygons will be masked, not just those on the line render path, or whose center point is within the polygon
#' @param write.out logical. If TRUE, will write a netCDF file with output.files. If FALSE will return a list of spatRasters
#' @return Saves R object `chl`, returns chlorophyll-a indicator in a data frame summarized by timestep for each area.names
#' @importFrom magrittr "%>%"
#' @export
#'
`%>%` <- magrittr::`%>%`

create_chl <- function(...) { 
  make_2d_summary_output <- EDABUtilities::make_2d_summary_ts(...) 
  df <- make_2d_summary_output %>%
    terra::as.data.frame(na.rm = FALSE) 
  chl <- df %>%
    dplyr::mutate(time = as.Date(time),
                  day = lubridate::day(time), month = lubridate::month(time), year = lubridate::year(time)) %>%
    dplyr::mutate(Units = c ('mg m^-3')) %>%
    subset(select = -c(agg.time, time) ) 
  
  return(chl)
}


#' Create Primary Production Indicator
#'
#' This function generates a primary productivity indicator from an ERDDAP netCDF file. 
#' The function passes data through EDABUtilities::make_2d_summary_ts, which provides summary statistics of 2d gridded data as time series by area.
#' Converts .nc files to data frame.
#' @param data.in Either a character vector of full input file names for a list of spatRasters
#' @param output.files character vector of full output file names corresponding to each input file
#' @param shp.file  string. Shape file you wish to crop each input file to
#' @param var.name string. Variable name you wish to extract = 'pp'
#' @param area.names character vector. Names of shape file areas you want to summarize. 
#' @param statistic string. Which statistic to calculate = 'mean'
#' @param agg.time character. Time scale to calculate over (days, doy, months, season, or years)
#' @param tz string. Time zone to convert. No correction if NA
#' @param touches logical. If TRUE, all cells touched by lines or polygons will be masked, not just those on the line render path, or whose center point is within the polygon
#' @param write.out logical. If TRUE, will write a netCDF file with output.files. If FALSE will return a list of spatRasters
#' @return Saves R object `pp`, returns primary production indicator in a data frame summarized by timestep for each area.names
#' @importFrom magrittr "%>%"
#' @export
#'
`%>%` <- magrittr::`%>%`

create_pp <- function(...) { 
  make_2d_summary_output <- EDABUtilities::make_2d_summary_ts(...) 
  df <- make_2d_summary_output %>%
    terra::as.data.frame(na.rm = FALSE) 
  pp <- df %>%
    dplyr::mutate(time = as.Date(time),
                  day = lubridate::day(time), month = lubridate::month(time), year = lubridate::year(time)) %>%
    dplyr::mutate(Units = c ('mg m^-3')) %>%
    subset(select = -c(agg.time, time) ) 
  
  return(pp)
}