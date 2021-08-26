#' Prepares data for correlation analysis
#'
#' This function prepares stock data and `ecodata` data for correlation analysis and returns linear correlation p-values and slopes. A helper function for `plot_correlation`, `correlation_data`, and `correlation_summary`.
#'
#' @param stock_data Data about a single stock (one species, one region) subsetted from `assessmentdata::stockAssessmentData`
#' @param eco_data A data table from `ecodata`. May require pre-processing to standardize format.
#' @param lag_data The number of years to lag the correlation by. Defaults to 0.
#' @return A tibble
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export

data_prep <- function(stock_data, eco_data, lag_data = 0) {
  stock2 <- stock_data %>%
    dplyr::mutate(
      Time = as.numeric(.data$Time) - lag_data,
      facet = paste(.data$Metric, .data$Description, .data$Units, sep = "\n")
    ) %>%
    dplyr::ungroup()

  eco_data$Time <- as.numeric(eco_data$Time)

  data <- dplyr::full_join(stock2, eco_data,
    by = "Time"
  ) %>%
    dplyr::filter(
      stringr::str_detect(.data$Var, .data$Metric, negate = TRUE) | # remove self-correlations
        is.na(.data$Metric) |
        is.na(.data$Var)
    ) %>%
    dplyr::ungroup()

  data2 <- data %>%
    dplyr::mutate(missing = (is.na(.data$Value) | is.na(.data$Val))) %>%
    dplyr::group_by(.data$Metric, .data$Var) %>%
    dplyr::mutate(n_data_points = length(.data$Time) - sum(as.numeric(.data$missing)))

  data_model <- data2 %>%
    dplyr::filter(
      .data$n_data_points >= 3,
      .data$missing == FALSE
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$Metric, .data$Var) %>%
    dplyr::mutate(
      pval = summary(stats::lm(.data$Value ~ .data$Val))$coefficients[2, 4],
      slope = stats::coef(stats::lm(.data$Value ~ .data$Val))[2]
    ) %>%
    dplyr::mutate(sig = .data$pval < 0.05)

  data_no_model <- data2 %>%
    dplyr::filter(.data$n_data_points < 3 | .data$missing == TRUE) %>%
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
#' @param data The output of `data_prep()`.
#' @param lag The number of years to lag the correlation by. Defaults to 0.
#' @return A ggplot
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export

plot_correlation <- function(data,
                             lag = 0) {
  # both data sets must have a column called "Time"
  # the stock data should be from assessmentdata::stockAssessmentData
  # the eco data numeric values should be in a column called "Val"
  # the eco data category values should be in a column called "Var"

  data <- data %>%
    tibble::as_tibble()

  if (nrow(data) > 0) {
    my_colors <- c("black", "#B2292E", "gray")
    names(my_colors) <- c("FALSE", "TRUE", "NA")

    fig <- ggplot2::ggplot(
      data,
      ggplot2::aes(
        x = .data$Val,
        y = .data$Value
      )
    ) +
      viridis::scale_color_viridis(
        breaks = seq(1950, 2020, by = 10)
      ) +
      ggplot2::geom_path(ggplot2::aes(color = .data$Time)) +
      ggplot2::geom_point(ggplot2::aes(color = .data$Time)) +
      ggnewscale::new_scale_color() +
      ggplot2::scale_color_manual(
        values = my_colors,
        name = "Statistically significant\n(p < 0.05)"
      ) +
      ggplot2::stat_smooth(ggplot2::aes(color = .data$sig),
        method = "lm"
      ) +
      ggplot2::facet_grid(
        rows = ggplot2::vars(.data$facet),
        cols = ggplot2::vars(.data$Var),
        scales = "free"
      ) +
      ggplot2::scale_y_continuous(labels = scales::comma) +
      ggplot2::scale_x_continuous(labels = scales::comma) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        axis.title = ggplot2::element_blank(),
        legend.position = "bottom"
      )

    print(fig)
  } else {
    print("No data under conditions selected")
  }
}

#' Produces summary tables of correlations
#'
#' This function creates summary tables of correlations between stock data and `ecodata` data. Designed for use within a RMarkdown document.
#'
#' @param data The output of `data_prep()`.
#' @param lag The number of years to lag the correlation by. Defaults to 0.
#' @return A data frame
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export

correlation_data <- function(data, lag = 0) {
  data <- data %>%
    dplyr::filter(.data$sig == TRUE) # only statistically significant data

  # test correlations

  if (nrow(data) > 0) {
    for (i in unique(data$Metric)) {
      for (j in unique(data$Var)) {
        dat <- data %>%
          dplyr::filter(.data$Metric == i, .data$Var == j)

        if (nrow(dat) > 0) {
          results <- stats::lm(.data$Value ~ .data$Val,
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
          ) %>%
            print()

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
#' @param data The output of `data_prep()`.
#' @param lag The number of years to lag the correlation by. Defaults to 0.
#' @return A tibble
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export

correlation_summary <- function(data, lag = 0) {
  data <- data %>%
    dplyr::filter(.data$sig == TRUE) # only statistically significant data

  # test correlations

  if (nrow(data) > 0) {
    output <- c()

    for (i in unique(data$Metric)) {
      for (j in unique(data$Var)) {
        dat <- data %>%
          dplyr::filter(.data$Metric == i, .data$Var == j)

        if (nrow(dat) > 0) {
          results <- stats::lm(.data$Value ~ .data$Val,
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
#' @param lab A name for the figure file
#' @param file The path to the document to knit. Not suggested to change from the default.
#' @return An RMarkdown section
#' @importFrom magrittr %>%
#' @export

render_indicator <- function(test,
                             lab = "no-name",
                             file = system.file("correlation_bookdown_template/_general-child-doc.Rmd",
                               package = "NEesp"
                             )) {
  res <- knitr::knit_child(
    text = knitr::knit_expand(
      file,
      label = lab
    ),
    quiet = TRUE
  )
  cat(res, sep = "\n")
}

#' Calculate a pseudo-R2 for Poisson regressions
#'
#' This function calculates a pseudo-R2 for Poisson regressions.
#'
#' @param model A Poisson GLM model fit to the data
#' @param data The data used to fit the model
#' @param respv_colname The response variable column name
#' @return A pseudo-R2 value
#' @importFrom magrittr %>%
#' @export

pseudoR2 <- function(model, data, respv_colname) {
  data <- data %>%
    dplyr::rename(Measured = respv_colname)

  data$Predicted <- stats::predict(model, newdata = data) %>% exp()

  pseudoR2 <- 1 -
    sum((data$Measured - data$Predicted)^2) /
      sum((data$Measured - mean(data$Measured))^2)

  return(pseudoR2)
}
