#' Renders an indicator ESP report
#'
#' This function renders an indicator ESP report.
#'
#' @param x The common name of the species
#' @param trouble Whether or not to display verbose output. Defaults to FALSE.
#' @return A bookdown report (html) (saved in a folder called `action_reports` in the root directory)
#' @importFrom magrittr %>%
#' @export

render_ind_report <- function(x, trouble = FALSE) {
  new_dir <- here::here("action_reports/", x)
  dir.create(new_dir, recursive = TRUE)

  file.create(here::here(new_dir, ".nojekyll")) %>%
    invisible()

  bookdown_template <- c(
    list.files(here::here("correlation_bookdown_template"),
      full.names = TRUE
    ) %>%
      stringr::str_subset(".Rmd"),
    list.files(here::here("correlation_bookdown_template"),
      full.names = TRUE
    ) %>%
      stringr::str_subset(".yml")
  )

  file.copy(
    from = bookdown_template,
    to = here::here(new_dir),
    overwrite = TRUE
  ) %>%
    invisible()

  setwd(here::here(new_dir))

  if (trouble == FALSE) {
    bookdown::render_book(
      input = ".",
      params = list(
        species_ID = x,

        path = here::here(new_dir, "figures//"),

        latlong_data = latlong,
        shape = shape,

        asmt_sum_data = asmt_sum,

        survey_data = survey_big,

        ricky_survey_data = ricky_survey,

        diet_data = allfh,

        rec_data = rec,

        asmt_data = asmt,

        cond_data = cond,

        risk_data = risk,

        risk_year_hist_data = risk_year_hist,

        risk_year_value_data = risk_year_value,

        risk_species_data = risk_species,

        com_data = com,

        swept_data = swept
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

        latlong_data = latlong,
        shape = shape,

        asmt_sum_data = asmt_sum,

        survey_data = survey_big,

        ricky_survey_data = ricky_survey,

        diet_data = allfh,

        rec_data = rec,

        asmt_data = asmt,

        cond_data = cond,

        risk_data = risk,

        risk_year_hist_data = risk_year_hist,

        risk_year_value_data = risk_year_value,

        risk_species_data = risk_species,

        com_data = com,

        swept_data = swept
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

  print(paste("Done with", x, "!"))
}
