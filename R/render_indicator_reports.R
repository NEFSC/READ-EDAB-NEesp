#' Renders an indicator ESP report
#'
#' This function renders an indicator ESP report.
#'
#' @param x The common name of the species. `NEesp::species_key$Species` shows all available options.
#' @param input The folder with the bookdown template (include full file path, e.g. use `here::here`). Defaults to "package", which calls the template files saved in the package.
#' @param params_to_use A list of parameters to use in the markdown report. Do not set if using `input = "package"`.
#' @param trouble Whether or not to display verbose output. Defaults to FALSE.
#' @param save_data Whether or not to save the data used in report creation. Only relevant when using `input = "package"`. Defaults to TRUE.
#' @param out Deprecated. The `output_format` to be passed to the `bookdown::render_book` call. Defaults to `bookdown::gitbook(split_by = "section", fig_caption = TRUE)`. For experimental purposes only: most alternative outputs have not been tested.
#' @return A bookdown report (html) (saved in a folder called `action_reports` in the root directory)
#' @importFrom magrittr %>%
#' @export

render_ind_report <- function(x, 
                              input = "package", 
                              params_to_use,
                              trouble = FALSE, 
                              save_data = TRUE, 
                              config = "_bookdown.yml"#,
                              #out = bookdown::gitbook(split_by = "section", fig_caption = TRUE)
                              ) {
  starting_dir <- getwd()
  
  # fix capitalization if necessary
  x <- stringr::str_to_sentence(x)
  
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
    
    params_list <- list(
      species_ID = x,
      path = here::here(new_dir, "figures//"),
      ricky_survey_data = NEesp::bio_survey,
      save = save_data
    )
    
  } else {
    
    if(class(params_to_use)[1] != "list") {
      stop("Please add your parameters as a list!")
    } 
    
    params_list <- params_to_use
    
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
      config_file = config,
      params = params_list,
      intermediates_dir = new_dir,
      knit_root_dir = new_dir,
      output_dir = new_dir,
      #output_format = out,
      clean = TRUE,
      quiet = TRUE
    ) %>%
      suppressWarnings() %>%
      suppressMessages()
  }

  if (trouble == TRUE) {
    bookdown::render_book(
      input = ".",
      config_file = config,
      params = params_list, 
      intermediates_dir = new_dir,
      knit_root_dir = new_dir,
      output_dir = new_dir,
      #output_format = out,
      clean = TRUE,
      quiet = FALSE
    )
  }

  # clean up files
  clean <- c(
    list.files(here::here(new_dir),
               pattern = ".Rmd",
               full.names = TRUE
    ),
    list.files(here::here(new_dir),
               pattern = ".yml",
               full.names = TRUE
    )
  )

  file.remove(clean) %>%
    invisible()
  
  setwd(starting_dir)

  print(paste("Done with ", x, "!", sep = ""))
}
