#' Plot `survdat` depth data
#'
#' This function plots `survdat` depth data
#'
#' @param data A `survdat` data frame or tibble, containing data on one species.
#' @param species The species name to print as the subtitle
#' @return A ggplot
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export

plot_depth <- function(data, species) {
  selected.spp.sum <- data %>%
    dplyr::group_by(.data$YEAR, .data$SEASON) %>%
    dplyr::summarise(
      ave.d = mean(.data$DEPTH, na.rm = TRUE),
      sd.d = stats::sd(.data$DEPTH, na.rm = TRUE),
      ave.t = mean(.data$BOTTEMP, na.rm = TRUE),
      sd.t = stats::sd(.data$BOTTEMP, na.rm = TRUE)
    )

  if (nrow(selected.spp.sum) > 0) {
    fig <- selected.spp.sum %>%
      ggplot2::ggplot(ggplot2::aes(
        x = .data$YEAR %>% as.numeric(),
        y = -.data$ave.d,
        color = .data$SEASON,
        group = .data$SEASON
      )) +
      ggplot2::geom_line() +
      ggplot2::geom_point() +
      ggplot2::geom_pointrange(ggplot2::aes(
        ymin = -.data$ave.d - .data$sd.d,
        ymax = -.data$ave.d + .data$sd.d
      )) +
      ggplot2::xlab("Year") +
      ggplot2::ylab("depth of trawl (ft)") +
      ggplot2::ggtitle("Seasonal depth profile of",
        subtitle = species
      ) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

    return(fig)
  } else {
    print("NO DATA")
  }
}

#' Plot `survdat` temperature-at-depth data
#'
#' This function plots `survdat` temperature-at-depth data
#'
#' @param data A `survdat` data frame or tibble, containing data on one species.
#' @param species The species name to print as the subtitle
#' @return A ggplot
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export

plot_temp_depth <- function(data, species) {
  selected.spp.sum <- data %>%
    dplyr::group_by(.data$YEAR, .data$SEASON) %>%
    dplyr::summarise(
      ave.d = mean(.data$DEPTH, na.rm = TRUE),
      sd.d = stats::sd(.data$DEPTH, na.rm = TRUE),
      ave.t = mean(.data$BOTTEMP, na.rm = TRUE),
      sd.t = stats::sd(.data$BOTTEMP, na.rm = TRUE)
    )

  if (nrow(selected.spp.sum) > 0) {
    fig <- selected.spp.sum %>%
      ggplot2::ggplot(ggplot2::aes(
        x = .data$YEAR %>% as.numeric(),
        y = .data$ave.t,
        color = .data$SEASON,
        group = .data$SEASON
      )) +
      ggplot2::geom_line() +
      ggplot2::geom_point() +
      ggplot2::geom_pointrange(ggplot2::aes(
        ymin = .data$ave.t - .data$sd.t,
        ymax = .data$ave.t + .data$sd.t
      )) +
      ggplot2::xlab("Year") +
      ggplot2::ylab("Bottom temperature (C)") +
      ggplot2::ggtitle("Seasonal bottom temperature of tows that contain",
        subtitle = species
      ) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))

    return(fig)
  } else {
    print("NO DATA")
  }
}

#' Plot `survdat` age diversity data
#'
#' This function plots `survdat` age diversity data
#'
#' @param data A `survdat` data frame or tibble, containing data on one species.
#' @param species The species name to print as the subtitle
#' @return A ggplot
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export

plot_age_diversity <- function(data, species) {
  if (data$AGE %>% unique() %>% length() >= 3) {
    selected.age <- data %>%
      dplyr::filter(!is.na(.data$AGE))

    age.freq <- selected.age %>%
      dplyr::group_by(.data$YEAR, .data$AGE) %>%
      dplyr::summarise(age.n = length(.data$AGE))
    age.freq <- age.freq %>%
      dplyr::group_by(.data$YEAR) %>%
      dplyr::mutate(prop = (.data$age.n / sum(.data$age.n))) %>%
      dplyr::mutate(prop.ln = (.data$prop * log(.data$prop)))


    age.freq <- age.freq %>%
      dplyr::group_by(.data$YEAR) %>%
      dplyr::summarise(shanon.h = (-1 * (sum(.data$prop.ln))))
  } else {
    print("NOT ENOUGH DATA TO GENERATE METRIC")
    age.freq <- tibble::tibble() # make empty tibble for next logical test
  }

  if (nrow(age.freq) > 0) {
    fig <- age.freq %>%
      ggplot2::ggplot(ggplot2::aes(
        x = .data$YEAR,
        y = .data$shanon.h
      )) +
      ggplot2::geom_path(
        group = 1,
        size = 1.2,
        color = "blue"
      ) +
      ggplot2::geom_point(
        size = 3,
        shape = 23,
        fill = "Black"
      ) +
      ggplot2::xlab("Year") +
      ggplot2::ylab("Shannon diversity index (H)") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) +
      ggplot2::ggtitle("Age diversity of", subtitle = species)

    return(fig)
  } else {
    print("NO DATA")
  }
}

#' Plot `survdat` age density data
#'
#' This function plots `survdat` age density data
#'
#' @param data A `survdat` data frame or tibble, containing data on one species.
#' @return A ggplot
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export

plot_age_density <- function(data) {
  if (data$AGE %>% unique() %>% length() > 3) {
    fig <- data %>%
      tidyr::drop_na(.data$AGE) %>%
      dplyr::group_by(.data$YEAR) %>%
      ggplot2::ggplot(ggplot2::aes(
        x = .data$AGE,
        y = .data$YEAR %>% as.factor(),
        group = .data$YEAR %>% as.factor(),
        fill = .data$YEAR %>% as.numeric()
      )) +
      ggplot2::scale_fill_gradientn(
        colors = nmfspalette::nmfs_palette("regional web")(4),
        name = "Year"
      ) +
      ggridges::geom_density_ridges2() +
      ggplot2::scale_x_continuous(
        limits = c(0, (max(data$AGE, na.rm = TRUE))),
        breaks = seq(0, max(data$AGE, na.rm = TRUE), by = 5)
      ) +
      ggplot2::xlab("Age") +
      ggplot2::ylab("Year")

    return(fig)
  } else {
    print("NO DATA")
  }
}
