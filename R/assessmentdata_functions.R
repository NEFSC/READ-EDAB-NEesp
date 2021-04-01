#' Plot `assessmentdata::stockAssessmentData` data
#'
#' This function plots `assessmentdata::stockAssessmentData` data.
#'
#' @param x A data frame or tibble, containing data on one species. The output of `assessmentdata::stockAssessmentData`.
#' @param metric The metric to plot (Recruitment, Abundance, Catch, or F)
#' @param ytitle The title of the y-axis. Defaults to "".
#' @param lin The linetypes to use for each region (a named vector)
#' @param col The colors to use for each region (a named vector)
#' @return A ggplot
#' @importFrom magrittr %>%
#' @export

plot_asmt <- function(x, metric, ytitle = "", lin = lines, col = colors) {
  x <- x %>%
    dplyr::filter(Metric == metric)

  if (nrow(x) > 0) {
    x$facet_var <- paste(x$Description, "\n", x$Units, " (", x$AssessmentYear,
      ")",
      sep = ""
    )

    # mean by year because some years have two measurements?
    x <- x %>%
      dplyr::group_by(
        Year, Description, Units, AssessmentYear, Region,
        Age, facet_var, Category
      ) %>%
      dplyr::summarise(Value = mean(Value))

    fig <- ggplot2::ggplot(
      x,
      ggplot2::aes(
        x = Year,
        y = Value,
        lty = Region,
        color = Region,
        shape = facet_var
      )
    ) +
      ggplot2::geom_point(cex = 2) +
      ggplot2::geom_line() +
      ggplot2::theme_bw() +
      ggplot2::scale_y_continuous(label = scales::comma) +
      ggplot2::scale_linetype_manual(values = lin) +
      ggplot2::scale_color_manual(values = col) +
      ggplot2::scale_shape_manual(values = c(15:18, 0:14)) +
      ggplot2::theme(
        legend.position = "bottom",
        legend.direction = "vertical",
        # label.hjust = 1 # try next time
      ) +
      ggplot2::guides(
        shape = ggplot2::guide_legend(
          ncol = 2,
          title = "Description, units (assessment year)"
        ),
        fill = FALSE
      ) + # fill is a dummy aes for geom_gls
      ggplot2::ylab(ytitle)

    if (metric != "Catch" &
      x$Category %>%
        unique() %>%
        length() <= 2) {
      fig <- fig +
        ggplot2::facet_grid(
          rows = ggplot2::vars(Age),
          scales = "free_y"
        )
    }

    if (metric != "Catch" &
      x$Category %>%
        unique() %>%
        length() > 2) {
      fig <- fig +
        ggplot2::facet_grid(
          rows = ggplot2::vars(Category),
          scales = "free_y"
        )
    }

    # Category - SSB, mature biomass, etc

    ecodat <- x %>%
      dplyr::filter(Year > 0, Value > 0) %>%
      dplyr::group_by(Region, facet_var) %>%
      dplyr::mutate(num = length(Value)) %>%
      dplyr::filter(num > 30)

    if (nrow(ecodat) > 0) {
      fig <- fig +
        ecodata::geom_gls(
          inherit.aes = FALSE,
          data = ecodat,
          mapping = ggplot2::aes(
            x = Year,
            y = Value,
            lty = Region,
            fill = facet_var
          )
        )
      # geom_gls doesn't like group aesthetic, use fill instead
      # fill doesn't affect geom_gls
      # error but still showing output
      # could wrap the if loop with a try() statement if the error breaks some graphs
    }
    return(fig)
  } else {
    print("NO DATA")
  }
}

#' Plot B/Bmsy or F/Fmsy from `assessmentdata::stockAssessmentSummary`
#'
#' This function plots B/Bmsy or F/Fmsy from `assessmentdata::stockAssessmentSummary`. You will get better results from a saved data pull. The most recent version of `assessmentdata` does not contain multiple years of msy data.
#'
#' @param x A data frame or tibble, containing data on one species. The output of `assessmentdata::stockAssessmentSummary`.
#' @param ytitle The title of the y-axis. Defaults to "".
#' @param lin The linetypes to use for each region (a named vector)
#' @param col The colors to use for each region (a named vector)
#' @param type "b" for B/Bmsy, "f" for F/Fmsy
#' @return A ggplot
#' @importFrom magrittr %>%
#' @export

plot_msy <- function(x, ytitle = "", lin = lines, col = colors, type) {

  # fix colnames
  x$B.Bmsy <- x$`B/Bmsy`
  x$B.Year <- x$`B Year`

  # get data
  year <- x$`Assessment Year`
  Region <- x$Region
  if (type == "b") {
    value <- x$`B/Bmsy`
  } else if (type == "f") {
    value <- x$`F/Fmsy`
  }

  data <- data.frame(
    year = year,
    value = value,
    Region = Region
  )

  fig <- ggplot2::ggplot(
    data,
    ggplot2::aes(
      x = year,
      y = value,
      lty = Region,
      color = Region
    )
  ) +
    ggplot2::geom_point(cex = 2) +
    ggplot2::geom_line() +
    ggplot2::theme_bw() +
    ggplot2::xlab("Year") +
    ggplot2::ylab(ytitle) +
    ggplot2::theme(legend.position = "bottom") +
    ggplot2::scale_linetype_manual(values = lin) +
    ggplot2::scale_color_manual(values = col)

  ecodat <- data %>%
    dplyr::filter(value > 0, year > 0) %>%
    dplyr::group_by(Region) %>%
    dplyr::mutate(num = length(value)) %>%
    dplyr::filter(num > 30)

  if (length(ecodat$value) > 1) {
    fig <- fig +
      ecodata::geom_gls(
        inherit.aes = FALSE,
        data = ecodat,
        mapping = ggplot2::aes(
          x = year,
          y = value,
          lty = Region
        )
      )
  }

  return(fig)
}

#' Assess the status of B/Bmsy or F/Fmsy from `assessmentdata::stockAssessmentSummary`
#'
#' This function assesses the status of the most recent B/Bmsy or F/Fmsy from `assessmentdata::stockAssessmentSummary`.
#'
#' @param x A data frame or tibble, containing data on one species. The output of `assessmentdata::stockAssessmentSummary`.
#' @param region The region to assess.
#' @param metric "bbmsy" for B/Bmsy, "ffmsy" for F/Fmsy
#' @return A character
#' @importFrom magrittr %>%
#' @export

status <- function(data, regions, metric) {
  data <- dplyr::filter(data, Region == regions)

  if (metric == "bbmsy") {
    data <- data$`B/Bmsy`
  }
  if (metric == "ffmsy") {
    data <- data$`F/Fmsy`
  }

  if (sum(is.na(data)) == length(data)) {
    data_info <- "MISSING"
  } else {
    data_info <- "PRESENT"
  }

  data <- data[is.na(data) == FALSE]

  if (data_info == "PRESENT") {
    if (metric == "bbmsy") {
      data_status <- if (tail(data, n = 1) > 1) {
        "GOOD"
      } else
      if (tail(data, n = 1) > 0.5) {
        "CAUTION"
      } else {
        "DANGER"
      }
    }
    if (metric == "ffmsy") {
      data_status <- if (tail(data, n = 1) > 1) "DANGER" else "GOOD"
    }
  } else {
    data_status <- "UNKNOWN"
  }

  return(data_status)
}
