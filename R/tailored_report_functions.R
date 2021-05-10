
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

    fig <- ggplot2::ggplot(data, ggplot2::aes(
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
      ggplot2::ylab(unique(data$Metric[!is.na(data$Metric)])) +
      ggplot2::xlab(unique(data$Var))

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
      ggplot2::ylab(unique(data$Metric[!is.na(data$Metric)]))

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

    ind_fig <- ggplot2::ggplot(
      data,
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
      ggplot2::ylab(unique(data$Var))

    # version with summary table
    # tbl <- ggpubr::ggtexttable(tab_data, theme = ggpubr::ttheme("blank"), rows = NULL)  %>%
    #  ggpubr::tbody_add_border() %>%
    #  ggpubr::thead_add_border()

    # big_fig <- ggpubr::ggarrange(
    #  ggpubr::ggarrange(bsb_fig, ind_fig, tbl,
    #    ncol = 3,
    #    legend = "none",
    #    labels = c("A", "B", "C")
    #  ),
    #  fig,
    #  labels = c(NA, "D"),
    #  common.legend = TRUE,
    #  legend = "top",
    #  nrow = 2
    # )

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
      file <- paste("figures/",
        unique(data$Metric)[!is.na(unique(data$Metric))],
        "_",
        unique(data$Var),
        ".png",
        sep = ""
      ) %>%
        stringr::str_replace_all(" ", "_") %>%
        stringr::str_replace_all("\n", "_")

      ggplot2::ggsave(
        filename = file,
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

#' Prepare data for plotting with `plot_corr_only`
#'
#' This function prepares data for plotting with `plot_corr_only`.
#'
#' @param data A data frame containing stock and indicator time series, or a file path to a .csv containg the data. Data from a spreadsheet outputted by a `NEespShiny` or `NEesp` regression report.
#' @param metric The stock assessment metric to assess - c("Recruitmetn", "Abundance", "Catch", "Fmort")
#' @param pattern Optional. A pattern to detect in the `Var` row, for example if you do not want to plot all levels of `Var`. Can be a vector.
#' @param remove Optional. If the pattern should be removed (`TRUE`) or retained (`FALSE`). Can be a vector.
#' @return A tibble
#' @importFrom magrittr %>%
#' @export

prep_data <- function(data,
                      metric = "Recruitment",
                      pattern = NULL,
                      remove = NULL) {
  if (class(data) == "character") {
    data <- data %>%
      read.csv()
  }

  data <- data %>%
    dplyr::filter(
      Metric == metric | is.na(Metric)
    ) %>%
    dplyr::mutate(Var = Var %>% stringr::str_replace_all("\n", " "))

  if (length(pattern) > 0) {
    for (i in 1:length(pattern)) {
      data <- data %>%
        dplyr::filter(stringr::str_detect(Var, pattern[i], negate = remove[i]))
    }
  }

  data <- data %>%
    dplyr::mutate(Var = Var %>% stringr::str_wrap(40)) %>%
    dplyr::arrange(Time)

  return(data)
}

#' Create an indicator report card
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
  data <- data %>%
    dplyr::select(Time, Val) %>%
    dplyr::distinct()

  data <- data %>%
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
  for (i in 1:nrow(analysis)) {
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

  return(output)
}

#' Wrapper function to prep data, plot data, and create report card output
#'
#' This is a wrapper function of `prep_data`, `plot_corr_only`, and `time_rpt`
#'
#' @param file_path The file path to a spreadsheet outputted by a `NEespShiny` or `NEesp` regression report.
#' @param metric The stock metric to assess. Passed to `prep_data`.
#' @param pattern  Optional. Passed to `prep_data`. A pattern to detect in the `Var` row, for example if you do not want to plot all levels of `Var`. Can be a vector.
#' @param remove Optional. Passed to `prep_data`. If the pattern should be removed (`TRUE`) or retained (`FALSE`). Can be a vector.
#' @param lag Passed to `plot_corr_only`. The number of years by which the stock-indicator correlation was lagged. Required to correct the stock time series. Defaults to 0.
#' @param min_year Passed to `time_rpt`. The minimum year to consider for the recent time-series average. Defaults to 2016.
#' @param species Passed to `plot_corr_only`. The species name to add to plots. Defaults to "species".
#' @param mode If set to "shiny", plots will be displayed but no other functionality will be triggered (ex, saving figures or showing a report card). Also passed to `plot_corr_only`.
#' @return A tibble
#' @importFrom magrittr %>%
#' @export

wrap_analysis <- function(file_path,
                          metric = "Recruitment",
                          pattern = NULL,
                          remove = NULL,
                          lag = 0,
                          min_year = 2016,
                          species = "species",
                          mode = "download") {
  data <- read.csv(file_path)

  data$Time <- as.numeric(data$Time)

  for (i in unique(data$Var)) {
    this_data <- data %>%
      dplyr::filter(Var == i)

    this_data <- prep_data(
      data = this_data,
      metric = metric,
      pattern = pattern,
      remove = remove
    )

    if (nrow(this_data) > 0) {
      eval <- ((this_data %>%
        dplyr::select(sig) %>%
        tidyr::drop_na() %>%
        dplyr::distinct()) == "TRUE")

      plt <- plot_corr_only(this_data, lag = lag, species = species, mode = mode)
      print(plt)
      cat("\n\n")

      # time series report card
      if (mode != "shiny" &
        eval # only add to report card if relationship is sig
      ) {
        i <- i %>%
          stringr::str_replace_all("\n", " ")

        if (exists("rpt_card_time")) {
          test <- stringr::str_detect(colnames(rpt_card_time), i) # is the var already in the rpt card
          dont_eval <- "TRUE" %in% test
          if (dont_eval == FALSE # only add if not in rpt card already
          ) {
            rpt_card_time <<- dplyr::full_join(rpt_card_time,
              time_rpt(this_data, out_name = i, min_year = min_year),
              by = "Time"
            )
          }
        } else {
          rpt_card_time <<- time_rpt(this_data, out_name = i, min_year = min_year)
        }
      }
      
      # indicator regression report card
      if (mode != "shiny") {
        i <- i %>%
          stringr::str_replace_all("\n", " ")
        
        rname <- paste("Trend with", metric %>% stringr::str_to_lower())
        
        # indicator trend
        lil_dat <- this_data %>%
          dplyr::filter(!is.na(sig)) %>%
          dplyr::select(sig, slope)
        if(lil_dat$sig[1] == "TRUE") {trend <- "Yes,"} else {trend <- "No"}
        if(lil_dat$sig[1] == "TRUE") {
          if(lil_dat$slope[1] > 0) {dir <- "positive"} else {dir <- "negative"}
        } else {dir <- ""}
        
        
        # time trend
        model <- lm(Val ~ Time, data = this_data)
        
        sig <- summary(model)$coefficients[2, 4] < 0.05
        slope <- coef(model)[2]
        
        
        if(sig == TRUE) {trend2 <- "Yes,"} else {trend2 <- "No"}
        if(sig == TRUE) {
          if(slope > 0) {dir2 <- "positive"} else {dir2 <- "negative"}
        } else {dir2 <- ""}

        
        # make data frame
        tib <- rbind(c(i, "Trend with time", paste(trend2, dir2)),
                     c(i, rname, paste(trend, dir)))
        
        # make empty vector if needed
        if (!exists("rpt_card_ind")) {
          rpt_card_ind <<- c()
        }

        rpt_card_ind <<- rbind(rpt_card_ind, tib)
        colnames(rpt_card_ind) <- c("Indicator", "Trend_with", "Pattern")
      }
    }
  }
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
  for (i in 1:nrow(data)) {
    for (j in 1:ncol(data)) {
      data[i, j] <- stringr::str_remove_all(data[i, j], pattern = ", high")
      data[i, j] <- stringr::str_remove_all(data[i, j], pattern = ", neutral")
      data[i, j] <- stringr::str_remove_all(data[i, j], pattern = ", low")
    }
  }

  ft <- flextable::flextable(data)

  for (i in 1:nrow(data)) {
    for (j in 1:ncol(data)) {

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
  data <- data %>%
    tibble::as_tibble() %>%
    dplyr::mutate(Pattern = Pattern %>%
                    stringr::str_replace("No ", "No"),
                  Indicator = Indicator %>%
                    stringr::str_replace_all("\n", " ") %>%
                    stringr::str_wrap(20)) %>%
    dplyr::distinct() %>%
    tidyr::pivot_wider(names_from = "Trend_with", 
                       values_from = "Pattern",
                       values_fill = "Not tested")
  
  ft <- flextable::flextable(data)
  
  for (i in 1:nrow(data)) {
    for (j in 1:ncol(data)) {
      
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
