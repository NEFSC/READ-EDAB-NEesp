#' Format `survdat` data
#'
#' This function formats `survdat` data for subsequent plotting and analysis. Data from unique observations of a stock (species, region) are averaged by year.
#' Can probably be deprecated in favor of get_var_data2
#' 
#' @param x A `survdat` data frame or tibble, containing data on only a single species.
#' @param variable The `survdat` measurement of interest. Must be a column name of the `survdat` data set.
#' @return A tibble
#' @importFrom magrittr %>%
#' @export

get_var_data <- function(x, variable){
  # remove NA, zero abundance, length 
  y <- x %>% 
    dplyr::filter(get(variable) > 0, ABUNDANCE > 0) %>%
    dplyr::select(YEAR, SEASON, Region, fish_id, date, variable) %>%
    dplyr::distinct() # remove repeated row info
  
  # mean by year
  if(variable == "BIOMASS" | variable == "ABUNDANCE"){
    y <- y %>% 
      dplyr::group_by(YEAR, SEASON, Region) %>% 
      dplyr::summarise(variable2 = sum(get(variable))) %>%
      dplyr::select(YEAR, SEASON, Region, variable2)
  } else {
    y <- y %>% 
      dplyr::group_by(YEAR, SEASON, Region, fish_id, date) %>%
      dplyr::summarise(variable2 = mean(get(variable))) %>% # mean by day
      dplyr::ungroup() %>%
      dplyr::group_by(YEAR, SEASON, Region) %>%
      dplyr::summarise(variable3 = mean(variable2)) %>% # mean by season-year
      dplyr::select(YEAR, SEASON, Region, variable3)
  }
  
  colnames(y) <- c("YEAR", "SEASON", "Region", "variable")
  
  return(y)
}

#' Format `survdat` data
#'
#' This function formats `survdat` data for subsequent plotting and analysis. Data from unique observations of a stock (species, region) are averaged by year.
#' 
#' @param x A `survdat` data frame or tibble, containing data on one or more species.
#' @param variable The `survdat` measurement of interest. Must be a column name of the `survdat` data set.
#' @return A tibble
#' @importFrom magrittr %>%
#' @export

get_var_data2 <- function(x, variable){
  # remove NA, zero abundance, length 
  y <- x %>%
    dplyr::filter(get(variable) > 0, ABUNDANCE > 0) %>%
    dplyr::select(Species, YEAR, SEASON, Region, fish_id, date, variable) %>%
    dplyr::distinct() # remove repeated row info
  
  # mean by year
  if(variable == "BIOMASS" | variable == "ABUNDANCE"){
    y <- y %>% 
      dplyr::group_by(Species, YEAR, SEASON, Region) %>% 
      dplyr::summarise(variable2 = sum(get(variable))) %>%
      dplyr::select(YEAR, SEASON, Species, Region, variable2)
  } else {
    y <- y %>% 
      dplyr::group_by(Species, YEAR, SEASON, Region, fish_id, date) %>%
      dplyr::summarise(variable2 = mean(get(variable))) %>% # mean by day
      dplyr::ungroup() %>%
      dplyr::group_by(Species, YEAR, SEASON, Region) %>%
      dplyr::summarise(variable3 = mean(variable2)) %>% # mean by season-year
      dplyr::select(YEAR, SEASON, Species, Region, variable3)
  }
  
  colnames(y) <- c("YEAR", "SEASON","Species",  "Region", "variable")
  
  return(y)
}

#' Plot `survdat` data
#'
#' This function plots `survdat` data faceted by region. Data must be pre-processed with `get_var_data`.
#' 
#' @param x A data frame or tibble, containing data on one species. The output of `get_var_data`.
#' @param ytitle The title of the y-axis. Defaults to "".
#' @return A ggplot
#' @importFrom magrittr %>%
#' @export

plot_variable <- function(x, ytitle = "") {

  fig <- ggplot2::ggplot(x,
                         ggplot2::aes(x = as.numeric(YEAR),
                    y = variable,
                    color = SEASON))+
    ggplot2::geom_point(cex = 2)+
    ggplot2::geom_line()+
    ggplot2::facet_grid(rows = vars(Region))+
    nmfspalette::scale_color_nmfs("regional web")+
    ggplot2::scale_y_continuous(labels = scales::comma)+
    ggplot2::theme_bw()+
    ggplot2::xlab("Year")+
    ggplot2::ylab(ytitle)+
    ggplot2::theme(legend.position = "bottom")
  
  ecodat <- x %>%
    dplyr::filter(YEAR > 0) %>%
    dplyr::group_by(SEASON, Region) %>%
    dplyr::mutate(num = length(variable)) %>%
    dplyr::filter(num > 30)
  
  if (length(ecodat$num) > 1) {
    lines <- c(1:4)
    names(lines) <- c("FALL", "SPRING", "WINTER", "SUMMER")
    
    # override situations where geom_gls doesn't converge
    res <- try({ecodata::geom_gls(inherit.aes = FALSE,
                                  data = ecodat,
                                  mapping = aes(x = as.numeric(YEAR),
                                                y = variable,
                                                group = SEASON,
                                                lty = SEASON))+
        ggplot2::scale_linetype_manual(values = lines)}, silent = TRUE)
    
    if(class(res) != "try-error"){
      fig <- fig + res
      }
    }

  return(fig)
}

#' Summarise `survdat` data
#'
#' This function summarises `survdat` data, grouping by by region. Data must be pre-processed with `get_var_data`.
#' 
#' @param x A data frame or tibble, containing data on one species. The output of `get_var_data`.
#' @return A `DT::datatable`
#' @importFrom magrittr %>%
#' @export

data_summary <- function(x){
  table <- x %>% dplyr::group_by(SEASON, Region) %>%
    dplyr::filter(variable > 0) %>%
    dplyr::summarise(total_years = length(variable),
                     mean_value = mean(variable),
                     sd_value = sd(variable),
                     min_value = min(variable),
                     max_value = max(variable))
  
  return(table)
  
}

#' Summarise the past 5 years of `survdat` data
#'
#' This function summarises the past five years of `survdat` data, grouping by by region. Data must be pre-processed with `get_var_data`.
#' 
#' @param x A data frame or tibble, containing data on one species. The output of `get_var_data`.
#' @return A `DT::datatable`
#' @importFrom magrittr %>%
#' @export

data_summary_5yr <- function(x){
  x$YEAR <- as.numeric(x$YEAR)

  table <- x %>%  dplyr::group_by(SEASON, Region) %>%
    dplyr::mutate(max_year = max(YEAR)) %>%
    dplyr::filter(YEAR > max_year - 5,
                  variable > 0) %>%
    dplyr::summarise(mean_value5 = mean(variable),
                     sd_value5 = sd(variable),
                     min_value5 = min(variable),
                     max_value5 = max(variable))
  
  return(table)
  
}

#' A wrapper function to plot `survdat` data
#'
#' This function is a wrapper for `get_var_data` and `plot_variable`.
#' 
#' @param x A `survdat` data frame or tibble, containing data on one species. 
#' @param ytitle The title of the y-axis. Defaults to "".
#' @param variable The `survdat` measurement of interest. Must be a column name of the `survdat` data set.
#' @return A ggplot
#' @importFrom magrittr %>%
#' @export

generate_plot <- function(x, ytitle = "", variable){
  
  data <- get_var_data(x, variable = variable)
  
  if(nrow(data) > 0){
    fig <- plot_variable(data, ytitle = ytitle)
    
    return(fig)
  }
  
  else{print("NO DATA")}
  
}

#' A wrapper function to summarise `survdat` data
#'
#' This function is a wrapper for `get_var_data`, `data_summary`, and `data_summary_5yr`.
#' 
#' @param x A `survdat` data frame or tibble, containing data on one species. 
#' @param variable The `survdat` measurement of interest. Must be a column name of the `survdat` data set.
#' @param cap A caption for the table. Defaults to "".
#' @return A `DT::datatable`
#' @importFrom magrittr %>%
#' @export

generate_table <- function(x, variable, cap = ""){
  
  data <- get_var_data(x, variable = variable)
  
  if(nrow(data) > 0){
    table <- data_summary(data)
    table[ , 4:7] <- table[ , 4:7] %>%
      round(digits = 2)
    
    table_5yr <- data_summary_5yr(data)
    table_5yr[ , 3:6] <- table_5yr[ , 3:6] %>%
      round(digits = 2)
    
    total_table <- cbind(table,
                         table_5yr[ , -(1:2)]) %>%
      DT::datatable(rownames = FALSE,
                    colnames = c("Season", "Region", "Total years", "Mean", 
                                 "Standard deviation", "Minimum", "Maximum",
                                 "Mean (past 5 years)", 
                                 "Standard deviation (past 5 years)", 
                                 "Minimum (past 5 years)", 
                                 "Maximum (past 5 years)"),
                    filter = list(position = 'top', 
                                  clear = FALSE),
                    extensions = 'Scroller',
                    caption = cap,
                    options = list(search = list(regex = TRUE),
                                   deferRender = TRUE,
                                   scrollY = 200,
                                   scrollX = TRUE,
                                   scroller = TRUE,
                                   language = list(thousands = ",")))
    
    return(total_table)
  }
  
  else{print("NO DATA")}
  
}
