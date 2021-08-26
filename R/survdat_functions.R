#' Format `survdat` data
#'
#' This function formats `survdat` data for subsequent plotting and analysis. Data from unique observations of a stock (species, region) are averaged by year.
#'
#' @param x A `survdat` data frame or tibble, containing data on one or more species.
#' @param variable The `survdat` measurement of interest. Must be a column name of the `survdat` data set.
#' @return A tibble
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export

get_var_data <- function(x, variable) {
  # remove NA, zero abundance, length
  y <- x %>%
    dplyr::filter(get(variable) > 0, .data$ABUNDANCE > 0) %>%
    dplyr::select(.data$Species, .data$YEAR, .data$SEASON, .data$Region, .data$fish_id, .data$date, variable) %>%
    dplyr::distinct() # remove repeated row info

  # mean by year
  if (variable == "BIOMASS" | variable == "ABUNDANCE") {
    y <- y %>%
      dplyr::group_by(.data$Species, .data$YEAR, .data$SEASON, .data$Region) %>%
      dplyr::summarise(variable2 = sum(get(variable))) %>%
      dplyr::select(.data$YEAR, .data$SEASON, .data$Species, .data$Region, .data$variable2)
  } else {
    y <- y %>%
      dplyr::group_by(.data$Species, .data$YEAR, .data$SEASON, .data$Region, .data$fish_id, .data$date) %>%
      dplyr::summarise(variable2 = mean(get(variable))) %>% # mean by day
      dplyr::ungroup() %>%
      dplyr::group_by(.data$Species, .data$YEAR, .data$SEASON, .data$Region) %>%
      dplyr::summarise(variable3 = mean(.data$variable2)) %>% # mean by season-year
      dplyr::select(.data$YEAR, .data$SEASON, .data$Species, .data$Region, .data$variable3)
  }

  colnames(y) <- c("YEAR", "SEASON", "Species", "Region", "variable")

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
#' @importFrom rlang .data
#' @export

plot_variable <- function(x, ytitle = "") {
  fig <- ggplot2::ggplot(
    x,
    ggplot2::aes(
      x = as.numeric(.data$YEAR),
      y = .data$variable,
      color = .data$SEASON
    )
  ) +
    ggplot2::geom_point(cex = 2) +
    ggplot2::geom_line() +
    ggplot2::facet_grid(rows = ggplot2::vars(.data$Region)) +
    nmfspalette::scale_color_nmfs("regional web") +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::theme_bw() +
    ggplot2::xlab("Year") +
    ggplot2::ylab(ytitle) +
    ggplot2::theme(legend.position = "bottom")

  ecodat <- x %>%
    dplyr::filter(.data$YEAR > 0) %>%
    dplyr::group_by(.data$SEASON, .data$Region) %>%
    dplyr::mutate(num = length(.data$variable)) %>%
    dplyr::filter(.data$num > 30)

  if (length(ecodat$num) > 1) {
    lines <- c(1:4)
    names(lines) <- c("FALL", "SPRING", "WINTER", "SUMMER")

    # override situations where geom_gls doesn't converge
    res <- try(
      {
        ecodata::geom_gls(
          inherit.aes = FALSE,
          data = ecodat,
          mapping = ggplot2::aes(
            x = as.numeric(.data$YEAR),
            y = .data$variable,
            group = .data$SEASON,
            lty = .data$SEASON
          )
        ) +
          ggplot2::scale_linetype_manual(values = lines)
      },
      silent = TRUE
    )

    if (class(res) != "try-error") {
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
#' @importFrom rlang .data
#' @export

data_summary <- function(x) {
  table <- x %>%
    dplyr::group_by(.data$SEASON, .data$Region) %>%
    dplyr::filter(.data$variable > 0) %>%
    dplyr::summarise(
      total_years = length(.data$variable),
      mean_value = mean(.data$variable),
      sd_value = stats::sd(.data$variable),
      min_value = min(.data$variable),
      max_value = max(.data$variable)
    )

  return(table)
}

#' Summarise the past 5 years of `survdat` data
#'
#' This function summarises the past five years of `survdat` data, grouping by by region. Data must be pre-processed with `get_var_data`.
#'
#' @param x A data frame or tibble, containing data on one species. The output of `get_var_data`.
#' @return A `DT::datatable`
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export

data_summary_5yr <- function(x) {
  x$YEAR <- as.numeric(x$YEAR)

  table <- x %>%
    dplyr::group_by(.data$SEASON, .data$Region) %>%
    dplyr::mutate(max_year = max(.data$YEAR, na.rm = TRUE)) %>%
    dplyr::filter(
      .data$YEAR > .data$max_year - 5,
      .data$variable > 0
    ) %>%
    dplyr::summarise(
      mean_value5 = mean(.data$variable),
      sd_value5 = stats::sd(.data$variable),
      min_value5 = min(.data$variable),
      max_value5 = max(.data$variable)
    )

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
#' @export

generate_plot <- function(x, ytitle = "", variable) {
  data <- NEesp::get_var_data(x, variable = variable)

  if (nrow(data) > 0) {
    fig <- NEesp::plot_variable(data, ytitle = ytitle)

    return(fig)
  }

  else {
    print("NO DATA")
  }
}

#' A wrapper function to summarise `survdat` data
#'
#' This function is a wrapper for `get_var_data`, `data_summary`, and `data_summary_5yr`.
#'
#' @param x A `survdat` data frame or tibble, containing data on one species.
#' @param variable The `survdat` measurement of interest. Must be a column name of the `survdat` data set.
#' @param cap A caption for the table. Defaults to "".
#' @param type The file type of the output. One of c("html", "word")
#' @return A `DT::datatable` or a `knitr::kable`
#' @importFrom magrittr %>%
#' @export

generate_table <- function(x, variable, cap = "", type = "html") {
  data <- NEesp::get_var_data(x, variable = variable)

  if (nrow(data) > 0) {
    table <- NEesp::data_summary(data)
    table[, 4:7] <- table[, 4:7] %>%
      round(digits = 2)

    table_5yr <- NEesp::data_summary_5yr(data)
    table_5yr[, 3:6] <- table_5yr[, 3:6] %>%
      round(digits = 2)

    total_table <- cbind(
      table,
      table_5yr[, -(1:2)]
    )

    cnames <- c(
      "Season", "Region", "Total years", "Mean",
      "Standard deviation", "Minimum", "Maximum",
      "Mean (past 5 years)",
      "Standard deviation (past 5 years)",
      "Minimum (past 5 years)",
      "Maximum (past 5 years)"
    )

    if (type == "html") {
      total_table <- total_table %>%
        DT::datatable(
          rownames = FALSE,
          colnames = cnames,
          filter = list(
            position = "top",
            clear = FALSE
          ),
          extensions = "Scroller",
          caption = cap,
          options = list(
            search = list(regex = TRUE),
            deferRender = TRUE,
            scrollY = 200,
            scrollX = TRUE,
            scroller = TRUE,
            language = list(thousands = ",")
          )
        )
    }

    if (type == "word") {
      total_table <- total_table %>%
        knitr::kable(col.names = cnames)
    }


    return(total_table)
  }

  else {
    print("NO DATA")
  }
}

#' Format `survdat` length data
#'
#' This function formats `survdat` length data for subsequent plotting and analysis. Data from unique observations of a stock (species, region) are averaged by year. Returns lengths in a tidy format.
#'
#' @param x A `survdat` data frame or tibble, containing data on one ospecies.
#' @return A tibble
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export

get_len_data <- function(x) {
  y <- x %>%
    dplyr::filter(.data$LENGTH > 0, .data$ABUNDANCE > 0) %>%
    dplyr::select(.data$YEAR, .data$SEASON, .data$Region, .data$fish_id, .data$LENGTH, .data$NUMLEN) %>%
    dplyr::distinct() %>% # problem with repeat rows
    dplyr::group_by(.data$YEAR, .data$SEASON, .data$Region) %>%
    dplyr::mutate(n_fish = sum(.data$NUMLEN)) %>%
    dplyr::filter(.data$n_fish > 10) # only year-season-region with >10 fish

  y <- y %>%
    dplyr::group_by(.data$YEAR, .data$SEASON, .data$Region) %>%
    dplyr::summarise(
      mean_len = sum(.data$LENGTH * .data$NUMLEN) / sum(.data$NUMLEN),
      min_len = min(.data$LENGTH),
      max_len = max(.data$LENGTH)
    )

  y <- tidyr::pivot_longer(y, cols = c("mean_len", "min_len", "max_len"))
  return(y)
}

#' Plot minimum, mean, and maximum `survdat` length data
#'
#' This function plots `survdat` length data faceted by region. Data must be pre-processed with `get_len_data`.
#'
#' @param x A data frame or tibble, containing data on one species. The output of `get_len_data`.
#' @return A ggplot
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export

plot_len <- function(x) {
  for (i in unique(x$SEASON)) {
    y <- x %>%
      dplyr::filter(.data$SEASON == i)

    fig <- ggplot2::ggplot(
      y,
      ggplot2::aes(
        x = as.numeric(.data$YEAR),
        y = .data$value,
        color = .data$name
      )
    ) +
      ggplot2::geom_line() +
      ggplot2::geom_point(cex = 2) +
      ggplot2::facet_grid(rows = ggplot2::vars(.data$Region)) +
      nmfspalette::scale_color_nmfs("regional web",
        name = "",
        label = c("max", "mean", "min")
      ) +
      ggplot2::theme_bw() +
      ggplot2::xlab("Year") +
      ggplot2::ylab("Length") +
      ggplot2::labs(title = i)

    ecodat <- y %>%
      dplyr::filter(.data$YEAR > 0) %>%
      dplyr::group_by(.data$Region, .data$name) %>%
      dplyr::mutate(num = length(.data$value)) %>%
      dplyr::filter(.data$num > 30)

    if (length(ecodat$num) > 1) {
      fig <- fig + ecodata::geom_gls(
        inherit.aes = FALSE,
        data = ecodat,
        mapping = ggplot2::aes(
          x = as.numeric(.data$YEAR),
          y = .data$value,
          group = .data$name
        )
      )
      # i think this works even if one group's gls doesn't converge
    }

    print(fig)

    cat("\n\n")
  }
}

#' Plot a density curve of `survdat` length data
#'
#' This function plots `survdat` length data faceted by region.
#'
#' @param x A data frame or tibble, containing data on one species. A subset of a `survdat` pull.
#' @return A ggplot
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export

plot_len_hist <- function(x) {
  # x = direct survdat data
  y <- x %>%
    dplyr::filter(.data$LENGTH > 0, .data$ABUNDANCE > 0) %>%
    dplyr::select(.data$YEAR, .data$SEASON, .data$Region, .data$fish_id, .data$LENGTH, .data$NUMLEN) %>%
    dplyr::distinct() %>% # problem with repeat rows
    dplyr::mutate(
      Decade = .data$YEAR %>%
        stringr::str_trunc(
          width = 3,
          side = "right",
          ellipsis = ""
        ) %>%
        paste("0", sep = ""),
      LENGTH = .data$LENGTH %>%
        round(digits = 0)
    ) %>%
    dplyr::group_by(.data$Decade, .data$SEASON, .data$Region, .data$LENGTH) %>%
    dplyr::summarise(Count = sum(.data$NUMLEN)) %>%
    dplyr::group_by(.data$Decade, .data$SEASON, .data$Region) %>%
    dplyr::mutate(Proportion = .data$Count / sum(.data$Count))

  mycolors <- nmfspalette::nmfs_palette("regional web")(7)
  mycolors <- mycolors[c(4, 7, 1, 6, 2, 5, 3)] # reorder for better contrast
  names(mycolors) <- c(1960, 1970, 1980, 1990, 2000, 2010, 2020)

  for (i in unique(y$SEASON)) {
    x <- y %>% dplyr::filter(.data$SEASON == i)

    fig_count <- ggplot2::ggplot(
      x,
      ggplot2::aes(
        x = .data$LENGTH,
        y = .data$Count,
        color = .data$Decade
      )
    ) +
      ggplot2::geom_line(cex = 1.5) +
      ggplot2::facet_grid(rows = ggplot2::vars(.data$Region)) +
      ggplot2::scale_color_manual(values = mycolors) +
      ggplot2::scale_y_continuous(
        labels = scales::comma,
        limits = c(0, max(x$Count, na.rm = TRUE))
      ) +
      ggplot2::theme_bw() +
      ggplot2::xlab("Length (cm)")

    fig_prop <- ggplot2::ggplot(
      x,
      ggplot2::aes(
        x = .data$LENGTH,
        y = .data$Proportion,
        color = .data$Decade
      )
    ) +
      ggplot2::geom_line(cex = 1.5) +
      ggplot2::facet_grid(rows = ggplot2::vars(.data$Region)) +
      ggplot2::scale_color_manual(values = mycolors) +
      ggplot2::scale_y_continuous(limits = c(0, max(x$Proportion, na.rm = TRUE))) +
      ggplot2::theme_bw() +
      ggplot2::xlab("Length (cm)")

    ggpubr::ggarrange(fig_count, fig_prop,
      common.legend = TRUE,
      legend = "bottom"
    ) %>%
      ggpubr::annotate_figure(top = i) %>%
      print()

    cat("\n\n")
  }
}

#' A wrapper function to plot `survdat` length data
#'
#' This function is a wrapper for `get_len_data` and `plot_len`.
#'
#' @param x A `survdat` data frame or tibble, containing data on one species.
#' @return A ggplot
#' @export

generate_len_plot <- function(x) {
  data <- NEesp::get_len_data(x)

  if (nrow(data) > 0) {
    NEesp::plot_len(data)
  } else {
    print("NO DATA")
  }
}

#' Format `survdat` length data for summary data table
#'
#' This function formats `survdat` length data for a summary data table. Data from unique observations of a stock (species, region) are averaged by year.
#'
#' @param x A `survdat` data frame or tibble, containing data on one species.
#' @return A tibble
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export

get_len_data_tbl <- function(x) {
  x <- x %>%
    dplyr::filter(.data$LENGTH > 0, .data$ABUNDANCE > 0) %>%
    dplyr::select(.data$YEAR, .data$SEASON, .data$Region, .data$fish_id, .data$LENGTH, .data$NUMLEN) %>%
    dplyr::distinct() %>% # problem with repeat rows
    dplyr::group_by(.data$YEAR, .data$SEASON, .data$Region) %>%
    dplyr::mutate(n_fish = sum(.data$NUMLEN)) %>%
    dplyr::filter(.data$n_fish > 10) # only year-season-region with >10 fish

  return(x)
}

#' Summarise `survdat` length data
#'
#' This function summarises `survdat` data, grouping by by region and season. Data must be pre-processed with `get_len_data_tbl`.
#'
#' @param x A data frame or tibble, containing data on one species. The output of `get_len_data_tbl`.
#' @return A `DT::datatable`
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export

len_tbl_data <- function(x) {
  x <- x %>%
    dplyr::group_by(.data$SEASON, .data$Region) %>%
    dplyr::summarise(
      mean_len = sum(.data$LENGTH * .data$NUMLEN) / sum(.data$NUMLEN),
      sd_len = sqrt(sum((.data$LENGTH - .data$mean_len)^2 * .data$NUMLEN) /
        sum(.data$NUMLEN)),
      n_len = sum(.data$NUMLEN),
      n_yrs = length(unique(.data$YEAR)),
      min_len = min(.data$LENGTH),
      max_len = max(.data$LENGTH)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$SEASON, .data$Region) %>%
    dplyr::summarise(
      mean_value = paste(.data$mean_len %>%
        round(digits = 2),
      " +- ",
      .data$sd_len %>%
        round(digits = 2),
      " (",
      .data$n_len %>% format(big.mark = ","),
      ", ",
      .data$n_yrs,
      ") ",
      sep = ""
      ),

      range_value = paste(.data$min_len %>%
        round(digits = 2),
      .data$max_len %>%
        round(digits = 2),
      sep = " - "
      )
    )
  return(x)
}

#' Summarise the past 5 years of `survdat` length data
#'
#' This function summarises `survdat` data, grouping by by region and season. Data must be pre-processed with `get_len_data_tbl`.
#'
#' @param x A data frame or tibble, containing data on one species. The output of `get_len_data_tbl`.
#' @return A `DT::datatable`
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export

len_tbl_data_5yr <- function(x) {
  x$YEAR <- as.numeric(x$YEAR)

  x <- x %>%
    dplyr::group_by(.data$SEASON, .data$Region) %>%
    dplyr::mutate(max_year = max(.data$YEAR)) %>%
    dplyr::filter(.data$YEAR > .data$max_year - 5) %>%
    dplyr::summarise(
      mean_len = sum(.data$LENGTH * .data$NUMLEN) / sum(.data$NUMLEN),
      sd_len = sqrt(sum((.data$LENGTH - .data$mean_len)^2 * .data$NUMLEN) /
        sum(.data$NUMLEN)),
      n_len = sum(.data$NUMLEN),
      n_yrs = length(unique(.data$YEAR)),
      min_len = min(.data$LENGTH),
      max_len = max(.data$LENGTH)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(.data$SEASON, .data$Region) %>%
    dplyr::summarise(
      mean_value = paste(.data$mean_len %>%
        round(digits = 2),
      " +- ",
      .data$sd_len %>%
        round(digits = 2),
      " (",
      .data$n_len %>% format(big.mark = ","),
      ", ",
      .data$n_yrs,
      ") ",
      sep = ""
      ),

      range_value = paste(.data$min_len %>%
        round(digits = 2),
      .data$max_len %>%
        round(digits = 2),
      sep = " - "
      )
    )
  return(x)
}

#' A wrapper function to summarise `survdat` length data
#'
#' This function is a wrapper for `get_len_data_tbl`, `len_tbl_data`, and `len_tbl_data_5yr`. Returns a summary table.
#'
#' @param x A `survdat` data frame or tibble, containing data on one species.
#' @return A `knitr::kable`
#' @importFrom magrittr %>%
#' @export

generate_len_table <- function(x) {
  tbl_data <- NEesp::get_len_data_tbl(x)

  table <- NEesp::len_tbl_data(tbl_data)

  table_5yr <- NEesp::len_tbl_data_5yr(tbl_data)

  total_table <- cbind(
    table[, 1:3],
    table_5yr[, 3],
    table[, 4],
    table_5yr[, 4]
  )

  if (nrow(total_table) > 0) {
    return(total_table %>%
      knitr::kable(col.names = c(
        "Season", "Region",
        "Mean value +- SD (n fish, n years)",
        "Mean value +- SD (n fish, past 5 years)",
        "Range (total)",
        "Range (past 5 years)"
      )))
  } else {
    print("NO DATA")
  }
}

#' Format `survdat` length data for risk analysis
#'
#' This function formats `survdat` length data for subsequent risk analysis. Data from unique observations of a stock (species, region) are averaged by year. Returns lengths in a wide format.
#'
#' @param x A `survdat` data frame or tibble, containing data on one or more species.
#' @return A tibble
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export

get_len_data_risk <- function(x) {
  y <- x %>%
    dplyr::filter(.data$LENGTH > 0, .data$ABUNDANCE > 0) %>%
    dplyr::select(.data$Species, .data$YEAR, .data$SEASON, .data$Region, .data$fish_id, .data$LENGTH, .data$NUMLEN) %>%
    dplyr::distinct() %>% # problem with repeat rows
    dplyr::group_by(.data$Species, .data$YEAR, .data$SEASON, .data$Region) %>%
    dplyr::mutate(n_fish = sum(.data$NUMLEN)) %>%
    dplyr::filter(.data$n_fish > 10) # only year-season-region with >10 fish

  y <- y %>%
    dplyr::group_by(.data$Species, .data$YEAR, .data$SEASON, .data$Region) %>%
    dplyr::summarise(
      mean_len = sum(.data$LENGTH * .data$NUMLEN) / sum(.data$NUMLEN),
      min_len = min(.data$LENGTH),
      max_len = max(.data$LENGTH)
    )

  return(y)
}

#' Format `survdat` length data for data table
#'
#' This function formats `survdat` length data for subsequent plotting and analysis. Data from unique observations of a stock (species, region) are averaged by year. Returns lengths in a wide format.
#'
#' @param x A `survdat` data frame or tibble, containing data on one species.
#' @return A tibble
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export

get_len_data2 <- function(x) {
  y <- x %>%
    dplyr::filter(.data$LENGTH > 0, .data$ABUNDANCE > 0) %>%
    dplyr::select(.data$YEAR, .data$SEASON, .data$Region, .data$fish_id, .data$LENGTH, .data$NUMLEN) %>%
    dplyr::distinct() %>% # problem with repeat rows
    dplyr::group_by(.data$YEAR, .data$SEASON, .data$Region) %>%
    dplyr::mutate(n_fish = sum(.data$NUMLEN)) %>%
    dplyr::filter(.data$n_fish > 10) # only year-season-region with >10 fish

  y <- y %>%
    dplyr::group_by(.data$YEAR, .data$SEASON, .data$Region, .data$n_fish) %>%
    dplyr::summarise(
      mean_len = sum(.data$LENGTH * .data$NUMLEN) / sum(.data$NUMLEN),
      min_len = min(.data$LENGTH),
      max_len = max(.data$LENGTH)
    ) %>%
    dplyr::mutate(
      n_fish = .data$n_fish %>%
        format(big.mark = ","),
      mean_len = .data$mean_len %>%
        round(digits = 2)
    )

  return(y)
}

#' Format `survdat` to add common names
#'
#' This function appends `survdat` with common names for easier manipulation. By default returns all survdata that is not constrained by biological information. If provided "bio" will return a limited dataset containing only records with biological information such as age and sex.
#'
#' @param survdat_pull_type A desired dataset either all records or a set containing only records with biological information.
#' @return A tibble
#' @importFrom magrittr %>%
#' @export

common_names_survdat <- function(survdat_pull_type = "all") {
  # returns all survdata by defult, if survdat_pull_type= bio returns bio pulls
  if (survdat_pull_type == "bio") {
    survdata <- NEesp::bio_survey
    sp_key <- NEesp::species_key

    survdata.bio.w.codes <- dplyr::inner_join(survdata, sp_key, by = "SVSPP") %>%
      dplyr::rename("common_name" = "Species")

    # print("bio survdata")
    return(survdata.bio.w.codes)
  } else {
    survdata <- NEesp::survey %>%
      dplyr::rename("common_name" = "Species")
    # print("all survdata")
    return(survdata)
  }
}
