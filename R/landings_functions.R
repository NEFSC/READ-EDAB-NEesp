#' Plot the proportion of commercial and recreational landings
#'
#' This function plots the proportion of commercial and recreational landings for all years with existing data. Does not contain fine-scale region information.
#'
#' @param rec Recreational landings data for a single species. Subsetted from MRIP.
#' @param com Commercial landings data for a single species. Subsetted from FOSS.
#' @return A ggplot
#' @importFrom magrittr %>%
#' @export

plot_prop_catch <- function(rec, com) {
  if (nrow(rec) > 0 & nrow(com) > 0) {
    rec <- rec %>%
      dplyr::group_by(year) %>%
      dplyr::summarise(tot_catch_rec = sum(lbs_ab1)) %>%
      dplyr::rename("Year" = "year")

    com <- com %>%
      dplyr::group_by(Year) %>%
      dplyr::summarise(tot_catch_com = sum(Pounds))

    data <- dplyr::full_join(rec, com, by = "Year") %>%
      dplyr::select(Year, tot_catch_rec, tot_catch_com) %>%
      dplyr::filter(is.na(tot_catch_rec) == FALSE &
        is.na(tot_catch_com) == FALSE) %>%
      dplyr::mutate(
        total_catch = tot_catch_rec + tot_catch_com,
        prop_rec = tot_catch_rec / total_catch,
        prop_com = tot_catch_com / total_catch
      ) %>%
      dplyr::select(Year, prop_rec, prop_com) %>%
      tidyr::pivot_longer(cols = c("prop_rec", "prop_com"))

    fig <- ggplot2::ggplot(
      data,
      ggplot2::aes(
        x = Year,
        y = value,
        fill = name
      )
    ) +
      ggplot2::geom_bar(color = "black", stat = "identity") +
      ggplot2::theme_bw() +
      nmfspalette::scale_fill_nmfs(
        palette = "regional web",
        name = "Catch",
        labels = c("Commercial", "Recreational")
      ) +
      ggplot2::ylab("Proportion of catch")

    return(fig)
  } else {
    print("NO DATA")
  }
}

#' Returns proportional commercial and recreational landings data
#'
#' This function joins for formats commercial and recreational landings data for all years with existing data. Does not contain fine-scale region information.
#'
#' @param rec Recreational landings data for a single species. Subsetted from MRIP.
#' @param com Commercial landings data for a single species. Subsetted from FOSS.
#' @return A tibble
#' @importFrom magrittr %>%
#' @export

prop_catch_data <- function(rec, com) {
  if (nrow(rec) > 0 & nrow(com) > 0) {
    rec <- rec %>%
      dplyr::group_by(year) %>%
      dplyr::summarise(tot_catch_rec = sum(lbs_ab1)) %>%
      dplyr::rename("Year" = "year")

    com <- com %>%
      dplyr::group_by(Year) %>%
      dplyr::summarise(tot_catch_com = sum(Pounds))

    data <- dplyr::full_join(rec, com, by = "Year") %>%
      dplyr::select(Year, tot_catch_rec, tot_catch_com) %>%
      dplyr::filter(is.na(tot_catch_rec) == FALSE &
        is.na(tot_catch_com) == FALSE) %>%
      dplyr::mutate(
        total_catch = tot_catch_rec + tot_catch_com,
        prop_rec = (tot_catch_rec / total_catch) %>%
          round(digits = 3),
        prop_com = (tot_catch_com / total_catch) %>%
          round(digits = 3)
      ) %>%
      dplyr::mutate(
        tot_catch_rec = tot_catch_rec %>%
          format(big.mark = ","),
        tot_catch_com = tot_catch_com %>%
          format(big.mark = ","),
        total_catch = total_catch %>%
          format(big.mark = ",")
      )
    return(data)
  }
}

#' Plot commercial landings data
#'
#' This function plots commercial landings data. Does not contain fine-scale region information.
#'
#' @param data Commercial landings data for a single species. Subsetted from FOSS.
#' @return A ggplot
#' @importFrom magrittr %>%
#' @export

plot_com <- function(data) {
  if (nrow(data) > 0) {
    # get order of most important - least important state
    cat <- data %>%
      dplyr::group_by(State) %>%
      dplyr::summarise(imp = max(Pounds)) %>%
      dplyr::arrange(dplyr::desc(imp))

    data$State <- factor(
      data$State,
      cat$State
    )

    # assign colors based on nmfs color palette
    plot_colors <- NEesp::com_palette$color
    names(plot_colors) <- NEesp::com_palette$state_id

    # plot
    fig <- ggplot2::ggplot(
      data,
      ggplot2::aes(
        x = Year,
        y = Pounds,
        fill = State
      )
    ) +
      ggplot2::geom_bar(color = "black", stat = "identity") +
      ggplot2::theme_bw() +
      ggplot2::scale_y_continuous(
        name = "Total catch (lb)",
        labels = scales::comma,
        sec.axis = ggplot2::sec_axis(
          trans = ~ . / 2204.6,
          name = "Total catch (metric tons)",
          labels = scales::comma
        )
      ) +
      ggplot2::xlab("Year") +
      ggplot2::scale_fill_manual(
        name = "State",
        values = plot_colors
      ) +
      ggplot2::guides(fill = ggplot2::guide_legend(
        nrow = 2,
        byrow = TRUE,
        title = "State"
      )) +
      ggplot2::theme(legend.position = "bottom")

    return(fig)
  } else {
    print("NO DATA")
  }
}

#' Plot commercial revenue data
#'
#' This function plots commercial revenue data. Does not contain fine-scale region information.
#'
#' @param data Commercial revenue data for a single species. Subsetted from FOSS.
#' @return A ggplot
#' @importFrom magrittr %>%
#' @export

plot_com_money <- function(data) {
  if (nrow(data) > 0) {
    # get order of most important - least important state
    cat <- data %>%
      dplyr::group_by(State) %>%
      dplyr::summarise(imp = max(Dollars_adj)) %>%
      dplyr::arrange(dplyr::desc(imp))

    data$State <- factor(
      data$State,
      cat$State
    )

    # assign colors based on nmfs color palette
    plot_colors <- NEesp::com_palette$color
    names(plot_colors) <- NEesp::com_palette$state_id

    # plot
    fig <- ggplot2::ggplot(
      data,
      ggplot2::aes(
        x = Year,
        y = Dollars_adj,
        fill = State
      )
    ) +
      ggplot2::geom_bar(color = "black", stat = "identity") +
      ggplot2::theme_bw() +
      ggplot2::scale_y_continuous(
        name = "Total revenue (2019 $)",
        labels = scales::comma
      ) +
      ggplot2::xlab("Year") +
      ggplot2::scale_fill_manual(
        name = "State",
        values = plot_colors
      ) +
      ggplot2::guides(fill = ggplot2::guide_legend(
        nrow = 2,
        byrow = TRUE,
        title = "State"
      )) +
      ggplot2::theme(legend.position = "bottom")

    return(fig)
  } else {
    print("NO DATA")
  }
}

#' Prepare recreational data for plotting
#'
#' This function does basic data manipulation on recreational data. 
#'
#' @param data Recreational data for a single species. Subsetted from MRIP
#' @return A tibble
#' @importFrom magrittr %>%
#' @export

rec_data_prep <- function(data, state = FALSE){
  
  # for adding zeros
  combo <- expand.grid(
    year = min(data$year):max(data$year),
    mode_fx_f = unique(data$mode_fx_f)
  )
  
  # group by state if TRUE
  
  if(state){
    data <- data %>%
      dplyr::group_by(st_f)
  }
  
  # northeast data
  
  ne <- data %>%
    dplyr::filter(sub_reg_f == "NORTH ATLANTIC" |
                    sub_reg_f == "MID-ATLANTIC") %>%
    dplyr::group_by(mode_fx_f, year, .add = TRUE) %>%
    dplyr::summarise(total_catch = sum(tot_cat),
                     discards = sum(estrel),
                     landings = sum(lbs_ab1),
                     landings_num = sum(landing)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(total_catch_all_mode = sum(total_catch),
                  prop = discards/total_catch_all_mode,
                  prop_land = landings_num/total_catch_all_mode) %>%
    dplyr::full_join(combo,
                     by = c(
                       "year" = "year",
                       "mode_fx_f" = "mode_fx_f"
                     )) %>%
    dplyr::mutate(total_catch2 = ifelse(is.na(total_catch), 0, total_catch)) %>%
    dplyr::select(-total_catch) %>%
    tidyr::pivot_longer(cols = c("total_catch2", "discards", "prop", "landings", "landings_num", "prop_land")) %>%
    dplyr::mutate(Region = "Northeast")
  
  
  # outside of northeast data
  
  out <- data %>%
    dplyr::filter(sub_reg_f != "NORTH ATLANTIC" &
                    sub_reg_f != "MID-ATLANTIC") %>%
    dplyr::group_by(mode_fx_f, year) %>%
    dplyr::summarise(total_catch = sum(tot_cat),
                     discards = sum(estrel),
                     landings = sum(lbs_ab1),
                     landings_num = sum(landing)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(total_catch_all_mode = sum(total_catch),
                  prop = discards/total_catch_all_mode,
                  prop_land = landings_num/total_catch_all_mode) %>%
    dplyr::full_join(combo,
                     by = c(
                       "year" = "year",
                       "mode_fx_f" = "mode_fx_f"
                     )) %>%
    dplyr::mutate(total_catch2 = ifelse(is.na(total_catch), 0, total_catch)) %>%
    dplyr::select(-total_catch) %>%
    tidyr::pivot_longer(cols = c("total_catch2", "discards", "prop", "landings", "landings_num", "prop_land")) %>%
    dplyr::mutate(Region = "Outside\nNortheast")
  
  # total data
  
  all <- data %>%
    dplyr::group_by(mode_fx_f, year) %>%
    dplyr::summarise(total_catch = sum(tot_cat),
                     discards = sum(estrel),
                     landings = sum(lbs_ab1),
                     landings_num = sum(landing)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(total_catch_all_mode = sum(total_catch),
                  prop = discards/total_catch_all_mode,
                  prop_land = landings_num/total_catch_all_mode) %>%
    dplyr::full_join(combo,
                     by = c(
                       "year" = "year",
                       "mode_fx_f" = "mode_fx_f"
                     )) %>%
    dplyr::mutate(total_catch2 = ifelse(is.na(total_catch), 0, total_catch)) %>%
    dplyr::select(-total_catch) %>%
    tidyr::pivot_longer(cols = c("total_catch2", "discards", "prop", "landings", "landings_num", "prop_land")) %>%
    dplyr::mutate(Region = "All Regions")

  # combine
  
  full_data <- rbind(ne, out, all)
  
  # get order of most important - least important category
  cat <- full_data %>%
    dplyr::filter(name == "landings") %>%
    dplyr::group_by(mode_fx_f) %>%
    dplyr::summarise(imp = max(value)) %>%
    dplyr::arrange(dplyr::desc(imp))
  
  full_data$mode_fx_f <- factor(
    full_data$mode_fx_f,
    cat$mode_fx_f
  )

  return(full_data)
}

#' Plot recreational data
#'
#' This function plots recreational data. 
#'
#' @param data Recreational data for a single species. Subsetted from MRIP
#' @param var What data to plot. One of c("total_catch2", "discards", "prop", "landings", "landings_num", "prop_land")
#' @param title The title for the graph
#' @return A ggplot
#' @importFrom magrittr %>%
#' @export

plot_rec <- function(data, var, title) {
  if (nrow(data) > 0) {
    
    data <- NEesp::rec_data_prep(data)

    # assign colors based on nmfs color palette
    plot_colors <- NEesp::rec_palette$color
    names(plot_colors) <- NEesp::rec_palette$rec_mode 
    
    # plot
    fig <- ggplot2::ggplot(
      data %>%
        dplyr::filter(name == var),
      ggplot2::aes(
        x = year,
        y = value,
        fill = mode_fx_f
      )
    ) +
      ggplot2::geom_bar(color = "black", stat = "identity") +
      ggplot2::theme_bw() +
      ggplot2::scale_y_continuous(
        name = "",
        labels = scales::comma
      ) +
      ggplot2::xlab("Year") +
      ggplot2::scale_fill_manual(
        name = "Category",
        values = plot_colors
      ) +
      ggplot2::guides(fill = ggplot2::guide_legend(
        nrow = 2,
        byrow = TRUE,
        title = "Category"
      )) +
      ggplot2::facet_grid(rows = ggplot2::vars(Region)) +
      ggplot2::theme(legend.position = "bottom") +
      ggplot2::labs(title = title)
    
    return(fig)

  } else {
    print("NO DATA")
  }
}