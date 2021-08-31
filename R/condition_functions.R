#' Plot a length-weight curve
#'
#' This function creates a length-weight curve.
#'
#' @param x  A data frame or tibble, containing data on one species. Data from `allfh`.
#' @return A ggplot
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export

plot_lw <- function(x) {
  data <- x %>%
    dplyr::filter(
      is.na(.data$pdlen) == FALSE,
      is.na(.data$pdwgt) == FALSE
    ) %>%
    dplyr::select(.data$pdlen, .data$pdwgt, .data$season, .data$Region, .data$fish_id, .data$year) %>%
    dplyr::distinct() %>% # remove duplicates
    dplyr::group_by(.data$Region, .data$season) %>%
    dplyr::mutate(n_fish = length(.data$pdlen)) %>%
    dplyr::filter(.data$n_fish > 10) # only region-season with >10 fish

  if (nrow(data) > 0) {
    fig <- ggplot2::ggplot(
      data,
      ggplot2::aes(
        x = .data$pdlen,
        y = .data$pdwgt / 1000,
        color = .data$year
      )
    ) +
      ggplot2::geom_jitter(alpha = 0.5) +
      ggplot2::scale_color_gradientn(
        colors = nmfspalette::nmfs_palette("regional web")(6),
        limits = c(1973, 2018)
      ) +
      ggplot2::ylab("Weight (kg)") +
      ggplot2::xlab("Length (cm)") +
      ggplot2::theme_bw()

    if (unique(data$season) %>% length() > 1) {
      fig <- fig +
        ggplot2::facet_grid(
          cols = ggplot2::vars(.data$season),
          rows = ggplot2::vars(.data$Region)
        )
    } else {
      fig <- fig +
        ggplot2::facet_grid(rows = ggplot2::vars(.data$Region))
    }

    return(fig)
  } else {
    print("NO DATA")
  }
}

#' Plot species condition (weight/length^3)
#'
#' This function creates a plot of species condition (weight/length^3).
#'
#' @param x  A data frame or tibble, containing data on one species. Data from `allfh`.
#' @return A ggplot
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export

plot_cond <- function(x) {
  data <- x %>%
    dplyr::filter(
      is.na(.data$pdlen) == FALSE,
      is.na(.data$pdwgt) == FALSE
    ) %>%
    dplyr::select(.data$pdlen, .data$pdwgt, .data$season, .data$Region, .data$fish_id, .data$year) %>%
    dplyr::distinct() %>% # remove duplicates
    dplyr::group_by(.data$Region, .data$season) %>%
    dplyr::mutate(n_fish = length(.data$pdlen)) %>%
    dplyr::filter(.data$n_fish > 10) # only region-season with >10 fish

  if (nrow(data) > 0) {
    fig <- ggplot2::ggplot(
      data,
      ggplot2::aes(
        x = .data$year,
        y = .data$pdwgt / (.data$pdlen^3)
      )
    ) +
      ggplot2::geom_jitter(
        alpha = 0.5,
        color = nmfspalette::nmfs_palette("regional web")(1)
      ) +
      ggplot2::ylab("Weight / Length^3 (g/cm^3)") +
      ggplot2::xlab("Year") +
      ggplot2::labs(title = "Condition factor") +
      ggplot2::theme_bw()

    if (unique(data$season) %>% length() > 1) {
      fig <- fig +
        ggplot2::facet_grid(
          cols = ggplot2::vars(.data$season),
          rows = ggplot2::vars(.data$Region)
        )
    } else {
      fig <- fig +
        ggplot2::facet_grid(rows = ggplot2::vars(.data$Region))
    }

    ecodat <- data %>%
      dplyr::group_by(.data$year, .data$season, .data$Region) %>%
      dplyr::summarise(mean_condition = mean(.data$pdwgt / (.data$pdlen^3))) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(.data$season, .data$Region) %>%
      dplyr::mutate(n_year = length(.data$year)) %>%
      dplyr::filter(.data$n_year > 30)

    if (length(ecodat$year) > 1) {
      fig <- fig +
        ecodata::geom_gls(
          inherit.aes = FALSE,
          data = ecodat,
          mapping = ggplot2::aes(
            x = .data$year,
            y = .data$mean_condition
          )
        )
    }

    return(fig)
  } else {
    print("NO DATA")
  }
}

#' Plot species relative weight
#'
#' This function creates a plot of species relative weight.
#'
#' @param x  A data frame or tibble, containing data on one species. Data from `allfh`.
#' @return A ggplot
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export

plot_relw <- function(x) {
  if (nrow(x) > 0) {
    ecodat <- x %>%
      dplyr::group_by(.data$EPU, .data$sex) %>%
      dplyr::mutate(n_year = length(.data$YEAR)) %>%
      dplyr::filter(.data$n_year > 30)

    fig <- ggplot2::ggplot(
      x,
      ggplot2::aes(
        x = .data$YEAR,
        y = .data$MeanCond,
        color = .data$EPU,
        shape = .data$sex,
        lty = .data$sex
      )
    ) +
      ggplot2::geom_line() +
      ggplot2::geom_point() +
      nmfspalette::scale_color_nmfs("regional web") +
      ggplot2::theme_bw() +
      ggplot2::facet_grid(rows = ggplot2::vars(.data$EPU)) +
      ggplot2::ylab("Mean relative weight") +
      ggplot2::xlab("Year")

    if (length(ecodat$n_year > 1)) {
      fig <- fig +
        ecodata::geom_gls(
          inherit.aes = FALSE,
          data = ecodat,
          ggplot2::aes(
            x = .data$YEAR,
            y = .data$MeanCond,
            lty = .data$sex
          )
        )
    }

    return(fig)
  } else {
    print("NO DATA")
  }
}
