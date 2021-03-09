#' Prepares data for correlation analysis
#'
#' This function prepares stock data and `ecodata` data for correlation analysis and returns linear correlation p-values. A helper function for `plot_correlation`, `correlation_data`, and `correlation_summary`.
#'
#' @param stock_data Data about a single stock (one species, one region) subsetted from `assessmentdata::stockAssessmentData`
#' @param eco_data A data table from `ecodata`. May require pre-processing to standardize format.
#' @param lag_data The number of years to lag the correlation by. Defaults to 0.
#' @return A tibble
#' @importFrom magrittr %>%
#' @export

data_prep <- function(stock_data, eco_data, lag_data = 0) {
  stock2 <- stock_data %>%
    dplyr::mutate(
      Time = as.numeric(Time) - lag_data,
      facet = paste(Metric, Description, Units, sep = "\n")
    ) %>%
    dplyr::ungroup()

  eco_data$Time <- as.numeric(eco_data$Time)

  data <- dplyr::full_join(stock2, eco_data,
    by = "Time"
  ) %>%
    dplyr::filter(
      Var %>% stringr::str_detect(Metric) == FALSE, # remove self-correlations
      is.na(Var) == FALSE,
      is.na(Metric) == FALSE,
      is.na(Value) == FALSE,
      is.na(Val) == FALSE
    ) %>%
    dplyr::ungroup()

  data2 <- data %>%
    dplyr::group_by(Metric, Var) %>%
    dplyr::mutate(n_data_points = length(Time))

  data_model <- data2 %>%
    dplyr::filter(n_data_points >= 3) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(Metric, Var) %>%
    dplyr::mutate(pval = summary(lm(Value ~ Val))$coefficients[2, 4]) %>%
    dplyr::mutate(sig = pval < 0.05)

  data_no_model <- data2 %>%
    dplyr::filter(n_data_points < 3) %>%
    dplyr::mutate(
      pval = NA,
      sig = NA
    )

  data <- rbind(data_model, data_no_model)

  return(data)
}

#' Plots correlations between stock data and indicators
#'
#' This function plots correlations between stock data and `ecodata` data.
#'
#' @param stock Data about a single stock (one species, one region) subsetted from `assessmentdata::stockAssessmentData`
#' @param eco A data table from `ecodata`. May require pre-processing to standardize format.
#' @param lag The number of years to lag the correlation by. Defaults to 0.
#' @return A ggplot
#' @export

plot_correlation <- function(stock, eco, lag = 0) {
  # both data sets must have a column called "Time"
  # the stock data should be from assessmentdata::stockAssessmentData
  # the eco data numeric values should be in a column called "Val"
  # the eco data category values should be in a column called "Var"

  data <- NEesp::data_prep(
    stock_data = stock,
    eco_data = eco,
    lag_data = lag
  )

  if (nrow(data) > 0) {
    my_colors <- c("black", "#B2292E", "gray")
    names(my_colors) <- c("FALSE", "TRUE", "NA")

    fig <- ggplot2::ggplot(
      data,
      ggplot2::aes(
        x = Val,
        y = Value,
        color = sig
      )
    ) +
      ggplot2::geom_line(lty = 2) +
      ggplot2::geom_point() +
      ggplot2::stat_smooth(method = "lm") +
      ggplot2::facet_grid(
        rows = ggplot2::vars(facet),
        cols = ggplot2::vars(Var),
        scales = "free"
      ) +
      ggplot2::scale_color_manual(
        values = my_colors,
        name = "Statistically significant\n(p < 0.05)"
      ) +
      ggplot2::scale_y_continuous(labels = scales::comma) +
      ggplot2::scale_x_continuous(labels = scales::comma) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        axis.title = ggplot2::element_blank(),
        legend.position = "bottom"
      )

    return(fig)
  } else {
    print("No data under conditions selected")
  }
}

#' Produces summary tables of correlations
#'
#' This function creates summary tables of correlations between stock data and `ecodata` data. Designed for use within a RMarkdown document.
#'
#' @param stock Data about a single stock (one species, one region) subsetted from `assessmentdata::stockAssessmentData`
#' @param eco A data table from `ecodata`. May require pre-processing to standardize format.
#' @param lag The number of years to lag the correlation by. Defaults to 0.
#' @return A data frame
#' @importFrom magrittr %>%
#' @export

correlation_data <- function(stock, eco, lag = 0) {
  data <- NEesp::data_prep(
    stock_data = stock,
    eco_data = eco,
    lag_data = lag
  ) %>%
    dplyr::filter(sig == TRUE) # only statistically significant data

  # test correlations

  if (nrow(data) > 0) {
    for (i in unique(data$Metric)) {
      for (j in unique(data$Var)) {
        dat <- data %>%
          dplyr::filter(Metric == i, Var == j)

        if (nrow(dat) > 0) {
          results <- lm(Value ~ Val,
            data = dat
          ) %>%
            summary()

          cat("\n\n<!-- -->\n\n")

          knitr::kable(
            list(
              results$coefficients %>%
                round(digits = 2),
              data.frame(
                Name = c("F-statistic", "df", "R2", "R2-adj"),
                Value = c(
                  results$fstatistic[1] %>%
                    round(digits = 2),
                  paste(results$fstatistic[2:3],
                    collapse = ", "
                  ),
                  results$r.squared %>%
                    round(digits = 2),
                  results$adj.r.squared %>%
                    round(digits = 2)
                )
              )
            ),
            caption = paste(i, j, sep = " vs "),
            booktabs = TRUE
          ) %>% print()

          cat("\n\n<!-- -->\n\n")
        }
      }
    }
  } else {
    print("No statistically significant data")
  }
}

#' Produces a summary table of correlations
#'
#' This function creates summary table of a correlation between stock data and `ecodata` data. Suggest to use with multiple `ecodata` indicators, and create a master data set by appending results.
#'
#' @param stock Data about a single stock (one species, one region) subsetted from `assessmentdata::stockAssessmentData`
#' @param eco A data table from `ecodata`. May require pre-processing to standardize format.
#' @param lag The number of years to lag the correlation by. Defaults to 0.
#' @return A tibble
#' @importFrom magrittr %>%
#' @export

correlation_summary <- function(stock, eco, lag = 0) {
  data <- NEesp::data_prep(
    stock_data = stock,
    eco_data = eco,
    lag_data = lag
  ) %>%
    dplyr::filter(sig == TRUE) # only statistically significant data

  # test correlations

  if (nrow(data) > 0) {
    output <- c()

    for (i in unique(data$Metric)) {
      for (j in unique(data$Var)) {
        dat <- data %>%
          dplyr::filter(Metric == i, Var == j)

        if (nrow(dat) > 0) {
          results <- lm(Value ~ Val,
            data = dat
          ) %>%
            summary()

          output <- rbind(output, c(
            i,
            j,
            length(dat$Metric), # number of points
            results$coefficients[2, 1] %>%
              signif(digits = 2), # slope
            results$coefficients[2, 4] %>%
              signif(digits = 2), # p-value
            results$adj.r.squared %>%
              round(digits = 2)
          )) # adjusted r2
        }
      }
    }

    return(output)
  }
}

#' Renders an RMarkdown section
#'
#' This function renders an RMarkdown section, including plots and data tables, for stock data and an `ecodata` indicator. Designed for use in an RMarkdown document. Stock data must be pre-defined.
#'
#' @param test A data table from `ecodata` that has been pre-processed to a standardized format.
#' @return An RMarkdown section
#' @importFrom magrittr %>%
#' @export

render_indicator <- function(test) {
  res <- knitr::knit_child(system.file("correlation_bookdown_template/_general-child-doc.Rmd", package = "NEesp"),
    quiet = TRUE
  )
  cat(res, sep = "\n")
}
