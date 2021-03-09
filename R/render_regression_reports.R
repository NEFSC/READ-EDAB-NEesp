#' Renders a regression ESP report
#'
#' This function renders a regression ESP report.
#'
#' @param stock_var The name of the species
#' @param epus_var The name of the EPU ("MAB", "GB", or "GOM")
#' @param region_var The name of the region the species is found in (must match an option in `assessmentdata::stockAssessmentData`)
#' @param remove_var Whether or not to remove the most recent 10 years of data. Defaults to FALSE.
#' @param lag_var How many years to lag the correlations by. Defaults to 0.
#' @param parent_folder The name of the folder to put the output in. If it does not exist, it will be created.
#' @param input The folder with the bookdown template. Defaults to "package", which calls the template files saved in the package.
#' @param trouble Whether or not to display verbose output. Defaults to FALSE.
#' @param save_var Whether or not to save the data used in report creation. Defaults to TRUE.
#' @return A bookdown report (html) (saved in a folder called `Regressions` in the root directory)
#' @importFrom magrittr %>%
#' @export

render_reg_report <- function(stock_var, epus_var, region_var, remove_var = FALSE,
                              lag_var = 0, parent_folder, input = "package",
                              trouble = FALSE, save_var = TRUE) {
  starting_dir <- getwd()
  
  new_dir <- here::here(
    "Regressions", parent_folder,
    paste(
      stock_var, region_var
      %>% stringr::str_replace_all("/", "-"),
      epus_var
    )
  ) %>%
    stringr::str_replace_all(" ", "_")
  
  dir.create(new_dir,
    recursive = TRUE
  )

  file.create(here::here(new_dir, ".nojekyll"))

  if(input == "package"){
    file.copy(
      from = list.files(system.file("correlation_bookdown_template", package = "NEesp"),
                        full.names = TRUE
      ),
      to = here::here(new_dir),
      overwrite = TRUE
    ) %>%
      invisible()
  } else {
    file.copy(
      from = list.files(input,
                        full.names = TRUE
      ),
      to = here::here(new_dir),
      overwrite = TRUE
    ) %>%
      invisible()
  }


  setwd(here::here(new_dir))

  if(save_var){
    dir.create("data",
               recursive = TRUE
    )
  }

  if (trouble == FALSE) {
    bookdown::render_book(
      input = ".",
      params = list(
        lag = lag_var,
        stock = stock_var,
        region = region_var,
        epu = c(epus_var, c("All", "all", "NE")),
        path = here::here(new_dir, "figures//"),
        save = save_var,
        remove_recent = remove_var
      ),
      output_dir = new_dir,
      intermediates_dir = new_dir,
      knit_root_dir = new_dir,
      clean = TRUE,
      quiet = TRUE
    ) %>%
      suppressMessages() %>%
      suppressWarnings()
  }

  if (trouble == TRUE) {
    bookdown::render_book(
      input = ".",
      params = list(
        lag = lag_var,
        stock = stock_var,
        region = region_var,
        epu = c(epus_var, c("All", "all", "NE")),
        path = here::here(new_dir, "figures//"),
        save = save_var,
        remove_recent = remove_var
      ),
      output_dir = new_dir,
      intermediates_dir = new_dir,
      knit_root_dir = new_dir,
      clean = TRUE,
      quiet = FALSE
    )
  }

  # clean up files
  list.files(here::here(new_dir),
    full.names = TRUE
  ) %>%
    stringr::str_subset(".Rmd") %>%
    file.remove()

  list.files(here::here(new_dir),
    full.names = TRUE
  ) %>%
    stringr::str_subset(".yml") %>%
    file.remove()
  
  setwd(starting_dir)

  print(paste("Done with", parent_folder, region_var, epus_var, stock_var, "!",
    sep = ": "
  ))
}

#' Renders all regression ESP reports
#'
#' This function renders all regression ESP reports.
#'
#' @param x The folder with the bookdown template. Defaults to "package", which calls the template files saved in the package.
#' @return Multiple bookdown reports (html)
#' @export

# get list of species and regions, manually assign to EPUs
# assessmentdata::stockAssessmentData %>%
#  dplyr::select(Species, Region) %>%
#  dplyr::distinct() %>%
#  dplyr::filter(Species %in%
#                  all_species | Species == "Goosefish") %>%
#  write.csv(here::here("R/regressions", "regression_species_regions.csv"))

# info <- read.csv(here::here("R/regressions", "regression_species_regions.csv"))

render_all_reg <- function(x = "package") {
  info <- NEesp::regression_species_regions

  for (i in 1:nrow(info)
  ) {
    # make 0 lag reports
    NEesp::render_reg_report(
      stock_var = info[i, 1],
      epus_var = info[i, 3],
      region_var = info[i, 2],
      lag_var = 0,
      remove_var = FALSE,
      save_var = TRUE,
      input = x,
      parent_folder = "zero_lag",
      trouble = TRUE
    )

    # make 1 year lag reports
    NEesp::render_reg_report(
      stock_var = info[i, 1],
      epus_var = info[i, 3],
      region_var = info[i, 2],
      lag_var = 1,
      remove_var = FALSE,
      save_var = FALSE,
      input = x,
      parent_folder = "one_year_lag",
      trouble = FALSE
    )

    # make 1 year lag, minus 10 recent years reports
    NEesp::render_reg_report(
      stock_var = info[i, 1],
      epus_var = info[i, 3],
      region_var = info[i, 2],
      lag_var = 1,
      remove_var = TRUE,
      save_var = FALSE,
      input = x,
      parent_folder = "one_year_lag_remove_recent",
      trouble = FALSE
    )
  }
}
