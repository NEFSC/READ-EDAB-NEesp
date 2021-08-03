#' Replace risk rating of 0.5 with NA
#'
#' This function replace a risk rating of 0.5 that was assigned due to missing data with "NA".
#' A helper function.
#'
#' @param x A data frame or tibble of risk data.
#' @return A data frame or tibble
#' @importFrom magrittr %>%
#' @export

missing_na <- function(data) {
  if (nrow(data) > 0) {
    for (i in seq_len(nrow(data))) {
      if (data$Value[i] %>% is.na()) {
        data$norm_rank[i] <- NA
      }
    }
  }
  return(data)
}

#' Rank an indicator within a species
#'
#' This function ranks all measurements of a single indicator throughout the time series.
#' Measurements are smoothed with a running mean.
#'
#' @param data A data frame or tibble of indicator data on a single stock.
#' @param year_source The name of the column with year data.
#' @param value_source The name of the column with indicator data.
#' @param high Whether a high value should be assessed as high risk ("high_risk") or low risk ("low_risk")
#' @param indicator_name The name the indicator should be assigned in the output.
#' @param n_run How many years to calculate the running mean over. Defaults to 5.
#' @return A tibble
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export

get_species_risk <- function(data, year_source, value_source,
                             high, indicator_name, n_run = 5) {
  data <- data %>%
    dplyr::rename("Value" = value_source, "Year" = year_source)

  # select assessmentdata just from most recent assessment for each species
  if (sum(data %>%
    colnames() %>%
    stringr::str_detect("AssessmentYear")) > 0) {
    data <- data %>%
      dplyr::group_by(.data$Species) %>%
      dplyr::mutate(most_recent_asmt = max(.data$AssessmentYear)) %>%
      dplyr::filter(.data$AssessmentYear == .data$most_recent_asmt)
  }

  # sum state data for commercial info
  if (sum(data %>%
    colnames() %>%
    stringr::str_detect("State")) > 0) {
    data <- data %>%
      dplyr::group_by(.data$Species, .data$Year) %>%
      dplyr::mutate(Value = sum(.data$Value))
  }

  data <- data %>%
    dplyr::select(.data$Species, .data$Region, .data$Value, .data$Year) %>%
    dplyr::mutate(
      ne_stock = (.data$Species %in% NEesp::species_key$Species),
      Year = as.numeric(.data$Year)
    ) %>%
    dplyr::filter(
      .data$ne_stock == "TRUE",
      is.na(.data$Value) == FALSE,
      is.na(.data$Year) == FALSE
    ) %>%
    dplyr::group_by(.data$Species, .data$Region) %>%
    dplyr::distinct(.data$Year, .keep_all = TRUE) %>% # some years have repeats - keep first value only for now
    dplyr::arrange(.data$Year)

  years <- data$Year %>%
    unique()

  results <- c()
  for (i in 1:((years %>% length()) - n_run)) {
    new_data <- data %>%
      dplyr::filter(
        .data$Year <= years[i + n_run],
        is.na(.data$Value) == FALSE,
        is.na(.data$Year) == FALSE
      ) %>%
      dplyr::mutate(recent = .data$Year > years[i + n_run] - n_run) %>%
      dplyr::group_by(.data$Species, .data$Region, .data$recent) %>%
      dplyr::mutate(
        mean_value = mean(.data$Value),
        Indicator = .data$indicator_name,
        Year = paste(years[i + 1], "-", years[i + n_run], " mean",
          sep = ""
        )
      ) %>%
      dplyr::select(.data$Species, .data$Region, .data$recent, .data$mean_value, .data$Indicator, .data$Year) %>%
      dplyr::distinct() %>%
      dplyr::filter(
        .data$recent == TRUE,
        is.na(.data$mean_value) == FALSE
      ) %>% # remove missing values
      dplyr::ungroup()

    results <- rbind(results, new_data)
  }

  if (high == "low_risk") {
    results <- results %>%
      dplyr::group_by(.data$Species, .data$Region) %>%
      dplyr::mutate(
        rank = rank(-.data$mean_value),
        norm_rank = .data$rank / max(.data$rank)
      )
  }

  if (high == "high_risk") {
    results <- results %>%
      dplyr::group_by(.data$Species, .data$Region) %>%
      dplyr::mutate(
        rank = rank(.data$mean_value),
        norm_rank = .data$rank / max(.data$rank)
      )
  }

  results <- results %>%
    dplyr::ungroup() %>%
    dplyr::select(.data$Species, .data$Region, .data$Indicator, .data$Year, .data$mean_value, .data$rank, .data$norm_rank) %>%
    dplyr::rename("Value" = "mean_value")

  return(results)
}

#' Rank change compared to historical value
#'
#' This function calculates the proportional change in an indicator by comparing the current value to the historical value.
#' Measurements are smoothed with a running mean.
#' Stocks are ranked against each other in each year.
#'
#' @param data A data frame or tibble of indicator data on multiple stocks.
#' @param year_source The name of the column with year data.
#' @param value_source The name of the column with indicator data.
#' @param high Whether a high value should be assessed as high risk ("high_risk") or low risk ("low_risk")
#' @param indicator_name The name the indicator should be assigned in the output.
#' @param n_run How many years to calculate the running mean over. Defaults to 5.
#' @return A tibble
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export

get_running_risk <- function(data, year_source, value_source,
                             high, indicator_name, n_run = 5) {
  data <- data %>%
    dplyr::rename("Value" = value_source, "Year" = year_source)

  # select assessmentdata just from most recent assessment for each species
  if (sum(data %>% colnames() %>% stringr::str_detect("AssessmentYear")) > 0) {
    data <- data %>%
      dplyr::group_by(.data$Species) %>%
      dplyr::mutate(most_recent_asmt = max(.data$AssessmentYear)) %>%
      dplyr::filter(AssessmentYear == .data$most_recent_asmt)
  }

  # sum state data for commercial info
  if (sum(data %>% colnames() %>% stringr::str_detect("State")) > 0) {
    data <- data %>%
      dplyr::group_by(.data$Species, .data$Year) %>%
      dplyr::mutate(Value = sum(.data$Value))
  }

  data <- data %>%
    dplyr::select(.data$Species, .data$Region, .data$Value, .data$Year) %>%
    dplyr::mutate(ne_stock = (.data$Species %in% NEesp::species_key$Species)) %>%
    dplyr::filter(
      .data$ne_stock == "TRUE",
      is.na(.data$Value) == FALSE,
      is.na(.data$Year) == FALSE
    )

  data <- data %>%
    dplyr::arrange(.data$Year)

  years <- data$Year %>%
    unique()

  results <- c()
  for (i in 1:((years %>% length()) - n_run)) {
    new_data <- data %>%
      dplyr::filter(
        .data$Year <= years[i + n_run],
        is.na(.data$Value) == FALSE,
        is.na(.data$Year) == FALSE
      ) %>%
      dplyr::mutate(recent = .data$Year > years[i + n_run] - n_run) %>%
      dplyr::group_by(.data$Species, .data$Region, .data$recent) %>%
      dplyr::mutate(mean_value = mean(.data$Value)) %>%
      dplyr::select(.data$Species, .data$Region, .data$recent, .data$mean_value) %>%
      dplyr::distinct() %>%
      tidyr::pivot_wider(
        names_from = .data$recent,
        values_from = .data$mean_value,
        names_prefix = "recent_"
      ) %>%
      dplyr::mutate(score = .data$recent_TRUE / .data$recent_FALSE) %>%
      dplyr::filter(is.na(.data$score) == FALSE) %>% # remove missing values
      dplyr::ungroup()

    if (high == "low_risk") {
      new_data <- new_data %>%
        dplyr::mutate(
          Indicator = .data$indicator_name,
          Year = paste(years[i + 1], "-", years[i + n_run], " vs historic mean",
            sep = ""
          ),
          rank = rank(-.data$score),
          norm_rank = .data$rank / max(.data$rank)
        )
    }

    if (high == "high_risk") {
      new_data <- new_data %>%
        dplyr::mutate(
          Indicator = .data$indicator_name,
          Year = paste(years[i + 1], "-", years[i + n_run], " vs historic mean",
            sep = ""
          ),
          rank = rank(.data$score),
          norm_rank = .data$rank / max(.data$rank)
        )
    }

    new_data <- new_data %>%
      dplyr::group_by(.data$Year) %>%
      dplyr::mutate(n_stocks = length(.data$rank)) %>%
      dplyr::filter(.data$n_stocks >= 5) %>% # only include years with 5+ stocks
      dplyr::ungroup() %>%
      dplyr::select(.data$Species, .data$Region, .data$Indicator, .data$Year, .data$score, .data$rank, .data$norm_rank) %>%
      dplyr::rename("Value" = "score")

    results <- rbind(results, new_data)
  }

  return(results)
}


#' Rank value compared to other species
#'
#' This function ranks all stocks against each other in each year.
#' Stocks are ranked by the value of the indicator.
#' Measurements are smoothed with a running mean.
#'
#' @param data A data frame or tibble of indicator data on multiple stocks.
#' @param year_source The name of the column with year data.
#' @param value_source The name of the column with indicator data.
#' @param high Whether a high value should be assessed as high risk ("high_risk") or low risk ("low_risk")
#' @param indicator_name The name the indicator should be assigned in the output.
#' @param n_run How many years to calculate the running mean over. Defaults to 5.
#' @return A tibble
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export

get_running_value_risk <- function(data, year_source, value_source,
                                   high, indicator_name, n_run = 5) {
  data <- data %>%
    dplyr::rename("Value" = value_source, "Year" = year_source)

  # select assessmentdata just from most recent assessment for each species
  if (sum(data %>% colnames() %>% stringr::str_detect("AssessmentYear")) > 0) {
    data <- data %>%
      dplyr::group_by(.data$Species) %>%
      dplyr::mutate(most_recent_asmt = max(.data$AssessmentYear)) %>%
      dplyr::filter(.data$AssessmentYear == .data$most_recent_asmt)
  }

  # sum state data for commercial info
  if (sum(data %>% colnames() %>% stringr::str_detect("State")) > 0) {
    data <- data %>%
      dplyr::group_by(.data$Species, .data$Year) %>%
      dplyr::mutate(Value = sum(.data$Value))
  }

  data <- data %>%
    dplyr::select(.data$Species, .data$Region, .data$Value, .data$Year) %>%
    dplyr::filter(
      .data$Species %in% NEesp::species_key$Species,
      is.na(.data$Value) == FALSE,
      is.na(.data$Year) == FALSE
    )

  data <- data %>%
    dplyr::arrange(.data$Year)

  years <- data$Year %>%
    unique()

  results <- c()
  for (i in 1:((years %>% length()) - n_run)) {
    new_data <- data %>%
      dplyr::filter(
        .data$Year <= years[i + n_run],
        is.na(.data$Value) == FALSE,
        is.na(.data$Year) == FALSE
      ) %>%
      dplyr::mutate(recent = .data$Year > years[i + n_run] - n_run) %>%
      dplyr::group_by(.data$Species, .data$Region, .data$recent) %>%
      dplyr::mutate(score = mean(.data$Value)) %>%
      dplyr::select(.data$Species, .data$Region, .data$recent, .data$score) %>%
      dplyr::distinct() %>%
      dplyr::filter(
        .data$recent == TRUE,
        is.na(.data$score) == FALSE
      ) %>% # remove missing values
      dplyr::ungroup()

    if (high == "low_risk") {
      new_data <- new_data %>%
        dplyr::mutate(
          Indicator = .data$indicator_name,
          Year = paste(years[i + 1], "-", years[i + n_run], " mean",
            sep = ""
          ),
          rank = rank(-.data$score),
          norm_rank = .data$rank / max(.data$rank)
        )
    }

    if (high == "high_risk") {
      new_data <- new_data %>%
        dplyr::mutate(
          Indicator = .data$indicator_name,
          Year = paste(years[i + 1], "-", years[i + n_run], " mean",
            sep = ""
          ),
          rank = rank(.data$score),
          norm_rank = .data$rank / max(.data$rank)
        )
    }

    new_data <- new_data %>%
      dplyr::group_by(.data$Year) %>%
      dplyr::mutate(n_stocks = length(.data$rank)) %>%
      dplyr::filter(.data$n_stocks >= 5) %>% # only include years with 5+ stocks
      dplyr::ungroup() %>%
      dplyr::select(.data$Species, .data$Region, .data$Indicator, .data$Year, .data$score, .data$rank, .data$norm_rank) %>%
      dplyr::rename("Value" = "score")

    results <- rbind(results, new_data)
  }

  return(results)
}

#' Plot risk over time
#'
#' This function plots indicator risk for one species over time.
#'
#' @param data A data frame or tibble of data. The output of `get_running_risk` or `get_running_value_risk`.
#' @param indicator_name The name of the indicator to plot.
#' @param title The title of the plot. Defaults to "".
#' @param include_legend Whether the plot should have a legend. If blank, legend will be included. If "no", legend will not be included.
#' @return A ggplot or multiple ggplots
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export

plot_risk_by_year <- function(data, indicator, title = "", include_legend) {

  # filter data
  if (indicator[1] != "all") {
    data <- data %>%
      dplyr::filter(.data$Indicator %in% indicator)
  }

  if (nrow(data) > 0) {
    # format year
    year <- data$Year %>%
      stringr::str_trunc(9, "right", ellipsis = "") %>%
      stringr::str_split_fixed("-", n = 2)
    data$new_year <- year[, 2]

    # plot
    for (i in unique(data$Region)) {
      new_data <- data %>%
        dplyr::filter(.data$Region == i)

      if (sum(new_data$norm_rank %>% is.na()) != nrow(new_data)) {
        fig <- ggplot2::ggplot(
          new_data,
          ggplot2::aes(
            x = .data$new_year %>% as.numeric(),
            y = .data$Indicator %>%
              stringr::str_replace_all("_", " "),
            fill = .data$norm_rank
          )
        ) +
          ggplot2::geom_raster(stat = "identity") +
          ggplot2::theme_bw() +
          viridis::scale_fill_viridis(
            limits = c(0, 1),
            breaks = c(0, 0.5, 1),
            direction = -1,
            na.value = "gray90",
            name = "Normalized rank"
          ) +
          ggplot2::theme(legend.position = "top") +
          ggplot2::ylab("Indicator") +
          ggplot2::xlab("Year") +
          ggplot2::labs(
            title = title,
            subtitle = i
          )

        n_category <- data$category %>%
          unique() %>%
          length()

        if (n_category > 1) {
          fig <- fig +
            ggplot2::facet_grid(
              rows = ggplot2::vars(.data$category),
              scales = "free_y",
              space = "free_y"
            )
        }

        if (include_legend == "no") {
          fig <- fig +
            ggplot2::theme(legend.position = "none")
        }

        print(fig)
      } else {
        print(paste("No", i, "data"))
      }
    }
  } else {
    print("NO DATA")
  }
}
