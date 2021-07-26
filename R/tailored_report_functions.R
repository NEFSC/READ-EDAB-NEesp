
#' Plot stock-time, indicator-time, and stock-indicator correlations
#'
#' This function plots stock-time, indicator-time, and stock-indicator correlations.
#'
#' @param data A data frame containing stock and indicator time series. Data from a spreadsheet outputted by a `NEespShiny` or `NEesp` regression report. Must pre-process with `NEesp::prep_data`, or do your own pre-processing.
#' @param title Optional. Title for the suite of plots. Defaults to blank.
#' @param lag The number of years by which the stock-indicator correlation was lagged. Required to correct the stock time series. Defaults to 0.
#' @param species The species name to add to plots. Defaults to "species".
#' @param mode If set to "shiny", plots will be displayed but no other functionality will be triggered (ex, saving figures or showing a report card)
#' @return 3 ggplots arranged with `ggpubr::ggarrange`
#' @importFrom magrittr %>%
#' @export

plot_corr_only <- function(data, title = "", lag = 0, species = "species", mode = "") {
  if (nrow(data) > 0) {
    if (mode != "shiny") {
      dir.create("figures")
    }

    my_colors <- c("black", "#B2292E")
    names(my_colors) <- c("FALSE", "TRUE")

    data$sig <- factor(data$sig, levels = c("TRUE", "FALSE"))
    
    # stock-indicator correlation ----
    
    data2 <- data %>%
      tidyr::drop_na()

    fig <- ggplot2::ggplot(data2, ggplot2::aes(
      x = Val,
      y = Value
    )) +
      ggplot2::geom_path(ggplot2::aes(color = Time)) +
      ggplot2::geom_point(ggplot2::aes(color = Time)) +
      viridis::scale_color_viridis(
        breaks = scales::breaks_extended(n = 4),
        name = "Year",
        guide = ggplot2::guide_colorbar(order = 2)
      ) +
      ggnewscale::new_scale_color() +
      ggplot2::stat_smooth(ggplot2::aes(color = sig),
        method = "lm"
      ) +
      ggplot2::scale_color_manual(
        values = my_colors,
        name = "Statistically significant\n(p < 0.05)",
        drop = FALSE,
        guide = ggplot2::guide_legend(order = 1)
      ) +
      ggplot2::scale_y_continuous(labels = scales::comma) +
      ggplot2::scale_x_continuous(labels = scales::comma) +
      ggplot2::theme_bw() +
      ggplot2::labs(title = paste("Correlation between", species, "and indicator")) +
      ggplot2::ylab(unique(data2$Metric)) +
      ggplot2::xlab(unique(data2$Var))

    # stock over time ----
    
    # test if bsb is sig over time - overwrite sig
    dat <- data %>%
      dplyr::select(Value, Time) %>%
      dplyr::distinct() %>%
      dplyr::mutate(Time = Time + lag)
    model <- lm(Value ~ Time, data = dat)
    pval <- summary(model)$coefficients[2, 4]
    data$sig <- (pval < 0.05)

    bsb_fig <- ggplot2::ggplot(
      data,
      ggplot2::aes(
        x = Time + lag,
        y = Value
      )
    ) +
      ggplot2::geom_path(ggplot2::aes(color = Time)) +
      ggplot2::geom_point(ggplot2::aes(color = Time)) +
      viridis::scale_color_viridis(
        breaks = scales::breaks_extended(n = 4),
        name = "Year"
      ) +
      ggnewscale::new_scale_color() +
      ggplot2::stat_smooth(ggplot2::aes(color = sig),
        method = "lm"
      ) +
      ggplot2::scale_color_manual(
        values = my_colors,
        name = "Statistically significant\n(p < 0.05)",
        drop = FALSE
      ) +
      ggplot2::scale_y_continuous(labels = scales::comma) +
      ggplot2::theme_bw() +
      ggplot2::labs(title = stringr::str_to_sentence(species)) +
      ggplot2::xlab("Year") +
      ggplot2::ylab(unique(data$Metric[!is.na(data$Metric)]))+
      ggplot2::xlim(c(min(data$Time), max(data$Time)))

    # indicator over time ----
    
    # test if indicator is sig over time - overwrite sig
    dat <- data %>%
      dplyr::select(Val, Time) %>%
      dplyr::distinct()
    model <- lm(Val ~ Time, data = dat)
    pval <- summary(model)$coefficients[2, 4]
    data$sig <- (pval < 0.05)

    # reformat Var for y-label
    data$Var <- data$Var %>%
      stringr::str_replace("\n", " ") %>%
      stringr::str_wrap(width = 30)
    
    plt <- data %>%
      tidyr::drop_na(Val)

    ind_fig <- ggplot2::ggplot(
      plt,
      ggplot2::aes(
        x = Time,
        y = Val
      )
    ) +
      ggplot2::geom_path(ggplot2::aes(color = Time)) +
      ggplot2::geom_point(ggplot2::aes(color = Time)) +
      viridis::scale_color_viridis(
        breaks = scales::breaks_extended(n = 4),
        name = "Year"
      ) +
      ggnewscale::new_scale_color() +
      ggplot2::stat_smooth(ggplot2::aes(color = sig),
        method = "lm"
      ) +
      ggplot2::scale_color_manual(
        values = my_colors,
        name = "Statistically significant\n(p < 0.05)",
        drop = FALSE
      ) +
      ggplot2::scale_y_continuous(labels = scales::comma) +
      ggplot2::theme_bw() +
      ggplot2::labs(title = "Indicator") +
      ggplot2::xlab("Year") +
      ggplot2::ylab(unique(plt$Var)) +
      ggplot2::xlim(c(min(data$Time), max(data$Time)))

    # change font size for shiny
    if (mode == "shiny") {
      fig <- fig + ggplot2::theme(
        text = ggplot2::element_text(size = 20),
        legend.text = ggplot2::element_text(size = 10)
      )
      bsb_fig <- bsb_fig + ggplot2::theme(
        text = ggplot2::element_text(size = 20),
        legend.text = ggplot2::element_text(size = 10)
      )
      ind_fig <- ind_fig + ggplot2::theme(
        text = ggplot2::element_text(size = 20),
        legend.text = ggplot2::element_text(size = 10)
      )
    }

    big_fig <- ggpubr::ggarrange(
      ggpubr::ggarrange(bsb_fig, ind_fig,
        ncol = 2,
        legend = "none",
        labels = c("A", "B"),
        font.label = list(size = 20, color = "black", face = "bold", family = NULL)
      ),
      fig,
      labels = c(NA, "C"),
      common.legend = TRUE,
      legend = "top",
      nrow = 2,
      font.label = list(size = 20, color = "black", face = "bold", family = NULL)
    )

    big_fig <- big_fig +
      ggplot2::theme(
        plot.background = ggplot2::element_rect(color = "black", size = 1),
        plot.margin = ggplot2::unit(c(0.1, 0.1, 0.1, 0.1), "cm")
      )

    # save
    if (mode != "shiny") {
      file <- paste0(
        unique(data$Metric)[!is.na(unique(data$Metric))],
        "_",
        unique(data$Var),
        ".png"
      ) %>%
        stringr::str_replace_all(" ", "_") %>%
        stringr::str_replace_all("\n", "_") %>%
        stringr::str_remove_all("/") %>%
        stringr::str_remove_all("\\\\")


      print(file)

      ggplot2::ggsave(
        filename = file,
        path = "figures",
        width = 6.5,
        height = 6.5,
        units = "in",
        device = "png"
      )
    }

    return(big_fig)
  }
  else {
    print("No data under conditions selected")
  }
}

#' Prepare data for plotting with `plot_corr_only`, within shiny app
#'
#' This function prepares data for plotting with `plot_corr_only`.
#'
#' @param file_path The file path to the data. Data from a spreadsheet outputted by a `NEespShiny` or `NEesp` regression report.
#' @param metric The stock assessment metric to assess - c("Recruitment", "Abundance", "Catch", "Fmort")
#' @param var Which level of the `Var` column to plot.
#' @return A tibble
#' @importFrom magrittr %>%
#' @export
#'
prep_si_data <- function(file_path,
                         metric = "Recruitment",
                         var) {
  this_data <- NEesp::read_file(file_path) %>%
    dplyr::filter(
      Var == var | is.na(Var),
      Metric == metric | is.na(Metric)
    ) %>%
    dplyr::mutate(Time = as.numeric(Time),
      Var = Var %>%
      stringr::str_replace_all("\n", " ")) %>%
    dplyr::mutate(Var = Var %>%
      stringr::str_wrap(40)) %>%
    dplyr::arrange(Time)

  return(this_data)
}

#' Create an indicator report card rating for one indicator
#'
#' This function creates an indicator report card. The report card assesses ecosystem/socioeconomic favorability for the stock, NOT overall ecosystem/socioeconomic favorability.
#'
#' @param data A data frame containing stock and indicator time series. Data from a spreadsheet outputted by a `NEespShiny` or `NEesp` regression report. Must pre-process with `NEesp::prep_data`, or do your own pre-processing.
#' @param out_name The name that the indicator column should have in the output.
#' @param min_year The minimum year to consider for the recent time-series average. Defaults to 2016.
#' @return A tibble
#' @importFrom magrittr %>%
#' @export

time_rpt <- function(data, out_name = "unnamed", min_year = 2016) {
  if ("TRUE" %in% data$sig) {
    data <- data %>%
      dplyr::select(Time, Val) %>%
      dplyr::distinct() %>%
      dplyr::group_by(Time) %>%
      dplyr::mutate(avg_value = mean(Val, na.rm = TRUE)) %>% # average by year
      dplyr::select(-Val) %>%
      dplyr::distinct()

    analysis <- data %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        long_avg = mean(avg_value) %>%
          round(digits = 2),
        long_sd = sd(avg_value) %>%
          round(digits = 2)
      ) %>%
      dplyr::filter(Time >= min_year) %>%
      dplyr::mutate(
        short_avg = mean(avg_value) %>%
          round(digits = 2),
        short_sd = sd(avg_value) %>%
          round(digits = 2),
        avg_value = round(avg_value, digits = 2)
      )

    status <- c()
    for (i in seq_len(nrow(analysis))) {
      if (analysis$avg_value[i] > (analysis$long_avg[1] + analysis$long_sd[1])) {
        status[i] <- "high"
      } else if (analysis$avg_value[i] < (analysis$long_avg[1] - analysis$long_sd[1])) {
        status[i] <- "low"
      } else {
        status[i] <- "neutral"
      }
    }

    analysis$avg_value <- paste(analysis$avg_value,
      status,
      sep = ", "
    )

    short_status <- c()
    if (analysis$short_avg[1] > (analysis$long_avg[1] + analysis$long_sd[1])) {
      short_status <- "high"
    } else if (analysis$short_avg[1] < (analysis$long_avg[1] - analysis$long_sd[1])) {
      short_status <- "low"
    } else {
      short_status <- "neutral"
    }

    output <- rbind(
      cbind(analysis$Time, analysis$avg_value),
      c(
        "recent mean",
        paste(analysis$short_avg[1],
          " ± ", analysis$short_sd[1],
          ", ",
          short_status,
          sep = ""
        )
      ),
      c(
        "long-term mean",
        paste(analysis$long_avg[1], "±", analysis$long_sd[1])
      )
    ) %>%
      tibble::as_tibble()

    colnames(output) <- c("Time", out_name)
  } else {
    output <- tibble::tibble(Time = NA)
  }

  return(output)
}

#' Read in csv or rds data
#'
#' This function reads in csv or rds data
#'
#' @return A datatable
#' @export

read_file <- function(file_pat) {
  if (stringr::str_detect(file_pat, ".csv$")) {
    data <- read.csv(file_pat)
  } else {
    data <- readRDS(file_pat)
  }
  return(data)
}

#' Create indicator report card output
#'
#' This function creates the report card output for an indicator. Variables are passed from `wrap_analysis`
#'
#' @param metric The stock metric to assess. Passed to `prep_data`.
#' @param data The data to use.
#' @param var Which level of `Var` to plot.
#' @return A tibble
#' @importFrom magrittr %>%
#' @export
ind_rpt <- function(var, metric, data) {
  var <- var %>%
    stringr::str_replace_all("\n", " ")

  rname <- paste("Trend with", metric %>% stringr::str_to_lower())

  # indicator trend
  lil_dat <- data %>%
    dplyr::filter(!is.na(sig)) %>%
    dplyr::select(sig, slope)
  if (lil_dat$sig[1] == "TRUE") {
    trend <- "Yes,"
  } else {
    trend <- "No"
  }
  if (lil_dat$sig[1] == "TRUE") {
    if (lil_dat$slope[1] > 0) {
      dir <- "positive"
    } else {
      dir <- "negative"
    }
  } else {
    dir <- ""
  }

  # time trend
  model <- lm(Val ~ Time, data = data)

  sig <- summary(model)$coefficients[2, 4] < 0.05
  slope <- coef(model)[2]


  if (sig == TRUE) {
    trend2 <- "Yes,"
  } else {
    trend2 <- "No"
  }
  if (sig == TRUE) {
    if (slope > 0) {
      dir2 <- "positive"
    } else {
      dir2 <- "negative"
    }
  } else {
    dir2 <- ""
  }

  # make data frame
  tib <- rbind(
    c(var, "Trend with time", paste(trend2, dir2)),
    c(var, rname, paste(trend, dir))
  )

  return(tib)
}

#' Create time report card
#'
#' This function creates a time series report card for select indicators.
#'
#' @param data A summary of indicator(s) of interest.
#' @return A flextable
#' @importFrom magrittr %>%
#' @export

make_time_rpt <- function(data) {
  # format colnames
  colnames(data) <- sapply(colnames(data),
    stringr::str_replace_all,
    pattern = "_",
    replacement = "\n"
  )

  orig <- data

  # remove words
  for (i in seq_len(nrow(data))) {
    for (j in seq_len(ncol(data))) {
      data[i, j] <- stringr::str_remove_all(data[i, j], pattern = ", high")
      data[i, j] <- stringr::str_remove_all(data[i, j], pattern = ", neutral")
      data[i, j] <- stringr::str_remove_all(data[i, j], pattern = ", low")
    }
  }

  ft <- flextable::flextable(data)

  for (i in seq_len(nrow(data))) {
    for (j in seq_len(ncol(data))) {

      # color based on ecosystem favorability
      if (stringr::str_detect(toString(orig[i, j]), ", high")) {
        ft <- flextable::bg(ft, i = i, j = j, bg = "darkolivegreen1")
      }

      if (stringr::str_detect(toString(orig[i, j]), ", low")) {
        ft <- flextable::bg(ft, i = i, j = j, bg = "red")
      }
    }
  }

  return(ft)
}

#' Create indicator report card
#'
#' This function creates an indicator report card for select indicators.
#'
#' @param data A summary of indicator(s) of interest.
#' @return A flextable
#' @importFrom magrittr %>%
#' @export

make_ind_rpt <- function(data) {
  colnames(data) <- c("Indicator", "Trend_with", "Pattern")
  data <- data %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      Pattern = Pattern %>%
        stringr::str_replace("No ", "No"),
      Indicator = Indicator %>%
        stringr::str_replace_all("\n", " ") %>%
        stringr::str_wrap(20)
    ) %>%
    dplyr::distinct() %>%
    tidyr::pivot_wider(
      names_from = "Trend_with",
      values_from = "Pattern",
      values_fill = "Not tested"
    )

  ft <- flextable::flextable(data)

  for (i in seq_len(nrow(data))) {
    for (j in seq_len(ncol(data))) {

      # make gray if not correlated
      if (stringr::str_detect(toString(data[i, j]), "^No$")) {
        ft <- flextable::bg(ft, i = i, j = j, bg = "gray90")
      }

      # make yellow if pos correlated
      if (stringr::str_detect(toString(data[i, j]), "Yes, positive")) {
        ft <- flextable::bg(ft, i = i, j = j, bg = "lightgoldenrod1")
      }

      # make purple if neg correlated
      if (stringr::str_detect(toString(data[i, j]), "Yes, negative")) {
        ft <- flextable::bg(ft, i = i, j = j, bg = "plum2")
      }
    }
  }

  return(ft)
}
