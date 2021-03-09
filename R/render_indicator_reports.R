#' Renders an indicator ESP report
#'
#' This function renders an indicator ESP report.
#'
#' @param x The common name of the species.
#' @param input The folder with the bookdown template. Defaults to "package", which calls the template files saved in the package.
#' @param trouble Whether or not to display verbose output. Defaults to FALSE.
#' @param save_data Whether or not to save the data generated used in report creation. Defaults to TRUE.
#' @return A bookdown report (html) (saved in a folder called `action_reports` in the root directory)
#' @importFrom magrittr %>%
#' @export

render_ind_report <- function(x, trouble = FALSE, save_data = TRUE, input = "package") {
  starting_dir <- getwd()
  
  new_dir <- here::here("action_reports/", x)
  dir.create(new_dir, recursive = TRUE) %>% suppressWarnings()

  file.create(here::here(new_dir, ".nojekyll")) %>%
    invisible()

  if(input == "package"){
    file.copy(
      from = list.files(system.file("indicator_bookdown_template", package = "NEesp"),
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

  if (trouble == FALSE) {
    bookdown::render_book(
      input = ".",
      params = list(
        species_ID = x,
        path = here::here(new_dir, "figures//"),
        ricky_survey_data = bio_survey,
        save = save_data
      ),
      intermediates_dir = new_dir,
      knit_root_dir = new_dir,
      output_dir = new_dir,
      clean = TRUE,
      quiet = TRUE
    ) %>%
      suppressWarnings() %>%
      suppressMessages()
  }

  if (trouble == TRUE) {
    bookdown::render_book(
      input = ".",
      params = list(
        species_ID = x,
        path = here::here(new_dir, "figures//"),
        ricky_survey_data = bio_survey,
        save = save_data
      ),
      intermediates_dir = new_dir,
      knit_root_dir = new_dir,
      output_dir = new_dir,
      clean = TRUE,
      quiet = FALSE
    )
  }

  # clean up files
  clean <- c(
    list.files(here::here(new_dir),
      full.names = TRUE
    ) %>%
      stringr::str_subset(".Rmd"),
    list.files(here::here(new_dir),
      full.names = TRUE
    ) %>%
      stringr::str_subset(".yml")
  )

  file.remove(clean) %>%
    invisible()
  
  setwd(starting_dir)

  print(paste("Done with", x, "!"))
}
