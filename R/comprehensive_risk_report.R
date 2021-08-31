#' Calculate risk for comprehensive risk report
#'
#' This function calculates risk for the comprehensive risk report by ranking species.
#'
#' @param data A data frame or tibble, containing data on multiple species.
#' @param year_source The name of the column with year information.
#' @param value_source The name of the column with the value that should be ranked.
#' @param analysis The type of analysis to do. Choose from `c("most_recent", "past5", "max_alltime", "past10hist", "diet")`.
#' @param high Whether a high value is high risk (`"high_risk"`) or low risk (`"low_risk"`).
#' @param indicator_name The name the indicator should have in the output.
#' @return A tibble
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export

get_risk <- function(data, year_source, value_source,
                     analysis, high, indicator_name) {

  # select assessmentdata just from most recent assessment for each species
  if (sum(data %>% colnames() %>% stringr::str_detect("AssessmentYear")) > 0) {
    data <- data %>%
      dplyr::group_by(.data$Species) %>%
      dplyr::mutate(most_recent_asmt = max(.data$AssessmentYear)) %>%
      dplyr::filter(.data$AssessmentYear == .data$most_recent_asmt)
  }

  data <- data %>%
    dplyr::select(.data$Species, .data$Region, .data$value_source, .data$year_source) %>%
    dplyr::rename("Value" = value_source, "Year" = year_source) %>%
    dplyr::filter(
      .data$Species %in% NEesp::species_key$Species,
      is.na(.data$Value) == FALSE,
      is.na(.data$Year) == FALSE
    )

  if (analysis == "most_recent") {
    data <- data %>%
      dplyr::group_by(.data$Species, .data$Region) %>%
      dplyr::mutate(most_recent_year = max(.data$Year)) %>%
      dplyr::ungroup() %>%
      dplyr::distinct() %>%
      dplyr::filter(.data$Year == .data$most_recent_year) %>%
      dplyr::select(.data$Species, .data$Region, .data$Value, .data$Year) %>%
      dplyr::ungroup() %>%

      # mean value because yellowtail flounder has two ffmsys

      dplyr::group_by(.data$Species, .data$Region, .data$Year) %>%
      dplyr::summarise(Value2 = mean(.data$Value, na.rm = TRUE)) %>%
      dplyr::ungroup() %>%
      dplyr::rename("Value" = "Value2")
  }

  if (analysis == "past5") {
    max_yr <- max(data$Year)

    data <- data %>%
      dplyr::filter(.data$Year > max_yr - 5) %>%
      dplyr::group_by(.data$Species, .data$Region) %>%
      dplyr::summarise(Value2 = mean(.data$Value, na.rm = TRUE)) %>%
      dplyr::rename("Value" = "Value2") %>%
      dplyr::mutate(Year = paste("mean of", max_yr - 5, "-", max_yr)) %>%
      dplyr::ungroup() %>%
      dplyr::select(.data$Species, .data$Region, .data$Year, .data$Value) %>%
      dplyr::distinct()
  }

  if (analysis == "max_alltime") {
    data <- data %>%
      dplyr::group_by(.data$Species, .data$Region) %>%
      dplyr::mutate(max_value = max(.data$Value)) %>%
      dplyr::filter(.data$Value == .data$max_value) %>%
      dplyr::select(.data$Species, .data$Region, .data$Year, .data$Value) %>%
      dplyr::ungroup()
  }

  if (analysis == "past10_hist") {
    max_yr <- max(data$Year)

    data <- data %>%
      dplyr::mutate(recent = .data$Year > max_yr - 10) %>%
      dplyr::group_by(.data$Species, .data$Region, .data$recent) %>%
      dplyr::mutate(mean_abun = mean(.data$Value, na.rm = TRUE)) %>%
      dplyr::select(.data$Species, .data$Region, .data$recent, .data$mean_abun) %>%
      dplyr::distinct() %>%
      tidyr::pivot_wider(
        names_from = .data$recent,
        values_from = .data$mean_abun,
        names_prefix = "recent_"
      ) %>%
      dplyr::mutate(Value = abs(.data$recent_TRUE - .data$recent_FALSE) / .data$recent_FALSE) %>% # magnitude of % change
      dplyr::select(.data$Species, .data$Region, .data$Value) %>%
      dplyr::mutate(Year = paste("mean of", max_yr - 10, "-", max_yr)) %>%
      dplyr::ungroup()
  }

  if (analysis == "diet") {
    data <- data %>%
      dplyr::group_by(.data$Species, .data$Region) %>%
      dplyr::mutate(
        Value = length(unique(.data$Value)),
        Year = "all time"
      ) %>%
      dplyr::select(.data$Species, .data$Region, .data$Value, .data$Year) %>%
      dplyr::distinct() %>%
      dplyr::ungroup()
  }

  if (high == "low_risk" & analysis != "diet") {
    data <- data %>%
      dplyr::mutate(
        Indicator = .data$indicator_name,
        rank = rank(-.data$Value),
        norm_rank = .data$rank / max(.data$rank)
      )
  }

  if (high == "high_risk" & analysis != "diet") {
    data <- data %>%
      dplyr::mutate(
        Indicator = .data$indicator_name,
        rank = rank(.data$Value),
        norm_rank = .data$rank / max(.data$rank)
      )
  }

  if (high == "low_risk" & analysis == "diet") {
    data <- data %>%
      dplyr::mutate(
        Indicator = .data$indicator_name,
        norm_diversity = .data$Value - min(.data$Value) + 1,
        rank = max(.data$Value) - .data$Value + 1,
        norm_rank = .data$rank / max(.data$rank)
      )
  }

  if (high == "high_risk" & analysis == "diet") {
    data <- data %>%
      dplyr::mutate(
        Indicator = .data$indicator_name,
        norm_diversity = .data$diet_diversity - min(.data$diet_diversity) + 1,
        rank = .data$norm_diversity,
        norm_rank = .data$rank / max(.data$rank)
      )
  }


  data <- data %>%
    dplyr::select(.data$Species, .data$Region, .data$Indicator, .data$Year, .data$Value, .data$rank, .data$norm_rank)

  return(data)
}

#' Render the comprehensive risk report
#'
#' This function the comprehensive risk report
#'
#' @param folder The folder where the output file should be saved.
#' @return An html
#' @export

render_comp_risk <- function(folder) {
  rmarkdown::render(list.files(system.file(package = "NEesp"),
    pattern = "comprehensive_risk_assessment.Rmd",
    full.names = TRUE
  ),
  output_dir = folder,
  output_file = "comprehensive_risk_report.html"
  )
}
