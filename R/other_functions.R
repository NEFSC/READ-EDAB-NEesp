#' Plot diet composition
#'
#' This function plots the proportional composition of a species' diet.
#'
#' @param data A data frame or tibble, containing data on one species. Data from `allfh`.
#' @return A ggplot
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export

get_diet_plot <- function(data) {
  if (nrow(data) > 0) {
    normalized <- data %>%
      dplyr::filter(.data$pyamtw > 0) %>%

      # only look at season/year combinations with >20 predator samples
      dplyr::group_by(.data$year, .data$season, .data$Region) %>%
      dplyr::mutate(n_predators = .data$fish_id %>%
        unique() %>%
        length()) %>%
      dplyr::filter(.data$n_predators > 20)

    if (length(normalized$n_predators) > 1) {
      normalized <- normalized %>%
        dplyr::group_by(.data$year, .data$season, .data$Region, .data$gensci) %>%
        dplyr::summarise(total_weight = sum(.data$pyamtw)) %>%
        dplyr::mutate(proportion = .data$total_weight / sum(.data$total_weight))

      normalized$gensci <- stringr::str_replace(normalized$gensci, " ", "_")

      # group low abundance prey as "other"
      groups <- normalized %>%
        dplyr::group_by(.data$year, .data$season, .data$Region, .data$gensci) %>%
        dplyr::summarise(max_prop = max(.data$proportion)) %>%
        dplyr::filter(.data$max_prop > 0.05)

      groups <- groups$gensci %>% unique()

      rows <- match(normalized$gensci, groups) %>%
        is.na() %>%
        which()

      normalized[rows, "gensci"] <- "OTHER"

      # re-group proportions with new "other" category
      normalized <- normalized %>%
        dplyr::group_by(.data$year, .data$season, .data$Region, .data$gensci) %>%
        dplyr::summarise(prop2 = sum(.data$proportion))

      # add in zeros
      combo <- expand.grid(
        year = min(normalized$year):max(normalized$year),
        season = unique(normalized$season),
        Region = unique(normalized$Region),
        gensci = unique(normalized$gensci)
      )

      new_normalized <- dplyr::full_join(normalized, combo,
        by = c(
          "year" = "year",
          "season" = "season",
          "Region" = "Region",
          "gensci" = "gensci"
        )
      ) %>%
        dplyr::mutate(prop2 = ifelse(is.na(.data$prop2), 0, .data$prop2))

      # get order of most important - least important prey
      prey <- new_normalized %>%
        dplyr::group_by(.data$gensci) %>%
        dplyr::summarise(imp = max(.data$prop2)) %>%
        dplyr::arrange(dplyr::desc(.data$imp))

      new_normalized$gensci <- factor(
        new_normalized$gensci,
        prey$gensci
      )

      # assign colors based on nmfs color palette
      plot_colors <- NEesp::prey_palette$color
      names(plot_colors) <- NEesp::prey_palette$prey_id

      # plot
      fig <- ggplot2::ggplot(
        new_normalized,
        ggplot2::aes(
          x = .data$year,
          y = .data$prop2,
          fill = .data$gensci
        )
      ) +
        ggplot2::geom_bar(color = "black", stat = "identity") +
        ggplot2::scale_fill_manual(
          name = "Prey \ncategory",
          values = plot_colors
        ) +
        ggplot2::theme_classic() +
        ggplot2::ylab("Proportion of gut content") +
        ggplot2::theme(legend.position = "bottom")

      if (length(unique(new_normalized$gensci)) > 5) {
        fig <- fig +
          ggplot2::guides(fill = ggplot2::guide_legend(
            nrow = 2,
            byrow = TRUE,
            title = "Category"
          ))
      } else {
        fig <- fig +
          ggplot2::guides(title = "Category")
      }

      if (length(unique(new_normalized$season)) > 1) {
        fig <- fig +
          ggplot2::facet_grid(
            rows = ggplot2::vars(.data$season),
            cols = ggplot2::vars(.data$Region)
          )
      } else {
        fig <- fig +
          ggplot2::facet_grid(cols = ggplot2::vars(.data$Region))
      }

      print(fig)
    } else {
      print("NOT ENOUGH DATA")
    }
  } else {
    print("NO DATA")
  }
}

#' Create a table of diet data
#'
#' This function creates a table of the proportional composition of a species' diet.
#'
#' @param data A data frame or tibble, containing data on one species. Data from `allfh`.
#' @param type The file type of the output. One of c("html", "word")
#' @return A `DT::datatable` or a `knitr::kable`
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export

get_diet_table <- function(data, type = "html") {
  if (nrow(data) > 0) {
    normalized <- data %>%
      dplyr::filter(.data$pyamtw > 0) %>%

      # only look at season/year combinations with >20 predator samples
      dplyr::group_by(.data$year, .data$season, .data$Region) %>%
      dplyr::mutate(n_predators = .data$fish_id %>%
        unique() %>%
        length()) %>%
      dplyr::filter(.data$n_predators > 20)

    if (length(normalized$n_predators) > 1) {
      normalized <- normalized %>%
        dplyr::group_by(.data$year, .data$season, .data$Region, .data$gensci) %>%
        dplyr::summarise(total_weight = sum(.data$pyamtw)) %>%
        dplyr::mutate(proportion = .data$total_weight / sum(.data$total_weight))

      normalized$gensci <- stringr::str_replace(normalized$gensci, " ", "_")

      # group low abundance prey as "other"
      groups <- normalized %>%
        dplyr::group_by(.data$year, .data$season, .data$Region, .data$gensci) %>%
        dplyr::summarise(max_prop = max(.data$proportion)) %>%
        dplyr::filter(.data$max_prop > 0.05)

      groups <- groups$gensci %>%
        unique()

      rows <- match(normalized$gensci, groups) %>%
        is.na() %>%
        which()

      normalized[rows, "gensci"] <- "OTHER"

      # re-group proportions with new "other" category
      normalized <- normalized %>%
        dplyr::group_by(.data$year, .data$season, .data$Region, .data$gensci) %>%
        dplyr::summarise(prop2 = sum(.data$proportion))

      # summary table
      table <- normalized %>%
        dplyr::group_by(.data$gensci, .data$season, .data$Region, .data$year) %>%
        dplyr::filter(sum(.data$prop2) > 0) %>%
        dplyr::group_by(.data$gensci, .data$season, .data$Region) %>%
        dplyr::summarise(
          mean_proportion = paste(mean(.data$prop2) %>% round(digits = 3),
            " +- ",
            stats::sd(.data$prop2) %>% round(digits = 3),
            " (", length(.data$prop2), ") ",
            sep = ""
          ),

          range_proportion = paste(min(.data$prop2) %>% round(digits = 3),
            max(.data$prop2) %>% round(digits = 3),
            sep = " - "
          )
        )

      make_html_table(table,
        type = type,
        col_names = c(
          "Prey category", "Season", "Region",
          "Mean proportion +- SD (n years)",
          "Range"
        )
      )
    } else {
      print("NOT ENOUGH DATA")
    }
  } else {
    print("NO DATA")
  }
}

#' Plot swept area estimates from `survdat`
#'
#' This function plots `swept area estimates from `survdat` data.
#'
#' @param x A data frame or tibble, containing data on one species. The swept area estimate from `survdat`.
#' @param var The variable to plot ("abundance" or "biomass")
#' @return A ggplot
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export

plot_swept <- function(x, var) {
  if (var == "biomass") {
    x <- x %>%
      dplyr::rename(
        "value" = .data$tot.biomass,
        "error" = .data$tot.bio.SE
      )
    name <- "Survey biomass estimate (kg)"
  }

  if (var == "abundance") {
    x <- x %>%
      dplyr::rename(
        "value" = .data$tot.abundance,
        "error" = .data$tot.abund.SE
      )

    name <- "Survey abundance estimate"
  }

  if (nrow(x) > 0) {
    fig <- ggplot2::ggplot(x) +
      ggplot2::geom_ribbon(ggplot2::aes(
        x = .data$YEAR,
        ymin = .data$value - 2 * .data$error,
        ymax = .data$value + 2 * .data$error,
        fill = .data$Season
      ),
      alpha = 0.5
      ) +
      ggplot2::geom_line(ggplot2::aes(
        x = .data$YEAR,
        y = .data$value,
        color = .data$Season
      ),
      cex = 2
      ) +
      nmfspalette::scale_color_nmfs("regional web") +
      nmfspalette::scale_fill_nmfs("regional web") +
      ggplot2::theme_bw() +
      ggplot2::scale_y_continuous(
        name = name,
        labels = scales::comma
      )

    y <- x %>%
      dplyr::filter(is.na(.data$value) == FALSE) %>%
      dplyr::group_by(.data$Season) %>%
      dplyr::summarise(n_points = length(.data$value)) %>%
      dplyr::filter(.data$n_points >= 30)

    if (nrow(y) > 0) {
      fig <- fig +
        ecodata::geom_gls(ggplot2::aes(
          x = .data$YEAR,
          y = .data$value,
          color = .data$Season
        ))
    }

    return(fig)
  } else {
    print("NO DATA")
  }
}

#' Plot climate vulnerability ratings
#'
#' This function plots climate vulnerability ratings from Hare et al. 2016.
#'
#' @param data A data frame or tibble of climate vulnerability ratings from Hare et al. 2016, containing data on one species.
#' @return A ggplot
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export

plot_climate_vulnerability <- function(data) {
  data <- data %>%
    tidyr::pivot_longer(cols = c("Low", "Moderate", "High", "Very.High"))

  data$name <- factor(data$name,
    levels = c("Low", "Moderate", "High", "Very.High")
  )

  for (i in unique(data$Attribute.Category)) {
    plot_data <- data %>%
      dplyr::filter(.data$Attribute.Category == i)

    fig <- ggplot2::ggplot(
      plot_data,
      ggplot2::aes(
        x = .data$name,
        y = .data$value,
        fill = .data$name
      )
    ) +
      ggplot2::geom_bar(
        stat = "identity",
        color = "black"
      ) +
      ggplot2::facet_wrap(
        facets = ggplot2::vars(.data$Attribute),
        ncol = 2
      ) +
      nmfspalette::scale_fill_nmfs(palette = "regional web") +
      ggplot2::theme_bw() +
      ggplot2::labs(title = i) +
      ggplot2::xlab("Expert rating") +
      ggplot2::ylab("Number of experts") +
      ggplot2::theme(legend.position = "none")

    print(fig)

    cat("\n\n")
  }
}

#' Plot stock-level data from `ecodata`
#'
#' This is a generic function to plot stock-level data from `ecodata`
#'
#' @param data `ecodata` data aggregated at the stock level, filtered to the stock of interest
#' @param ylabel Optional. Label for y-axis. Defaults to blank.
#' @return A ggplot
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export


plot_ecodata <- function(data, ylabel = "") {
  if (nrow(data) > 0) {
    fig <- ggplot2::ggplot(
      data,
      ggplot2::aes(
        x = .data$Time,
        y = .data$Value
      )
    ) +
      ggplot2::geom_point(cex = 2) +
      ggplot2::geom_line() +
      ggplot2::facet_grid(
        rows = ggplot2::vars(.data$Pattern_check %>%
          stringr::str_wrap(20)),
        cols = ggplot2::vars(.data$Var)
      ) +
      ggplot2::theme_bw() +
      ggplot2::xlab("Year") +
      ggplot2::ylab(ylabel)

    ecodat <- data %>%
      dplyr::distinct() %>%
      dplyr::group_by(.data$Pattern_check, .data$Var) %>%
      dplyr::mutate(nyear = length(.data$Time)) %>%
      dplyr::filter(.data$nyear > 30)

    # add if statement to check N > 30
    if (nrow(ecodat) > 0) {
      fig <- fig + ecodata::geom_gls(data = ecodat)
    }

    return(fig)
  } else {
    ("NO DATA")
  }
}
