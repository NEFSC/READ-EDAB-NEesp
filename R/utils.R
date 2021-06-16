#' Change "Goosefish" to "Monkfish" in a designated "Species" column
#'
#' This function changes any instances of "Goosefish" to "Monkfish" in a designated "Species" column
#'
#' @param data A data frame or tibble
#' @param species_col The name of the column with species names
#' @return A tibble
#' @importFrom magrittr %>%
#' @export

update_species_names <- function(data, species_col) {
  col_num <- which(colnames(data) == species_col)

  data <- data %>%
    dplyr::rename(Species = species_col) %>%
    dplyr::mutate(Species = Species %>%
      stringr::str_replace("Goosefish", "Monkfish"))
  # add any other names that have to be changed in mutate() above

  colnames(data)[col_num] <- species_col

  return(data)
}

#' Wrapper function for `DT::datatable`
#'
#' This function is a wrapper for `DT::datatable`.
#'
#' @param x A data frame or tibble
#' @param col_names The column names to display on the output. Defaults to the column names of the data frame.
#' @return An html table
#' @export

make_html_table <- function(x, col_names = colnames(x), type = "html") {
  if (is.null(x) == FALSE) {
    if (nrow(x) > 0) {
      
      if (type == "html") {
        output <- DT::datatable(x,
          rownames = FALSE,
          colnames = col_names,
          filter = list(
            position = "top",
            clear = FALSE
          ),
          extensions = "Scroller",
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
        if(nrow(x) <= 60) {
          output <- knitr::kable(x, col.names = col_names)
        } else {
          output <- "More than 60 rows of data! Please see `data` folder."
        }
        
      }

      return(output)
      
    } else {
      print("NO DATA")
    }
  } else {
    print("NO DATA")
  }
}

#' Wrapper function for `DT::datatable`
#'
#' This function is a wrapper for `DT::datatable`. Pagination is disabled.
#'
#' @param x A data frame or tibble
#' @param col_names The column names to display on the output. Defaults to the column names of the data frame.
#' @return An html table
#' @export

make_html_table_thin <- function(x, col_names) {
  if (is.null(x) == FALSE) {
    if (nrow(x) > 0) {
      output <- DT::datatable(x,
        rownames = FALSE,
        colnames = col_names,
        filter = list(
          position = "top",
          clear = FALSE
        ),
        # extensions = 'Scroller',
        options = list(
          search = list(regex = TRUE),
          deferRender = TRUE,
          scrollY = 200,
          # scrollX = TRUE,
          # scroller = TRUE,
          language = list(thousands = ","),
          paging = FALSE
        )
      )
      return(output)
    } else {
      print("NO DATA")
    }
  } else {
    print("NO DATA")
  }
}

#' Changes the class of `character` columns to `factor`
#'
#' This function changes `character` columns to class `factor` for better rendering as an html table with the `DT` package.
#'
#' @param x A data frame or tibble
#' @return A data frame
#' @export

character_to_factor <- function(x) {
  if (is.null(x) == FALSE) {
    if (nrow(x) > 0) {
      for (i in 1:ncol(x)) {
        x <- as.data.frame(x)
        if (class(x[, i]) == "character") {
          x[, i] <- as.factor(x[, i])
        }
      }
      return(x)
    }
  }
}

#' Change the class of a vector to `numeric`
#'
#' This function changes a vector to class `int` or `dbl`. Used when numerics are not read in properly.
#'
#' @param x A vector
#' @return A vector
#' @export

format_numbers <- function(x) {
  as.numeric(as.character(unlist(x)))
}

#' Save a data set
#'
#' This function saves a data set if it has nrow > 0. Data is saved in a folder called `data` in the working directory.
#'
#' @param x A data table or tibble
#' @return A .csv (small files), or a .RDS (large files)
#' @export

save_data <- function(x) {
  name <- substitute(x)

  if (nrow(x) > 0) {

    # remove column of row indices
    if ("X" %in% colnames(x)) {
      x <- x %>%
        dplyr::select(-X)
    }

    objsize <- object.size(x)

    if (objsize < 10^4 ) {
      filename <- paste("data/", name, ".csv", sep = "")
      write.csv(x, file = filename, row.names = FALSE)
    } else {
      filename <- paste("data/", name, ".RDS", sep = "")
      saveRDS(x, file = filename)
    }
  }
}

#' Locate files containing a text string
#'
#' This function locates all .R and .Rmd files that contain a text string.
#'
#' @param text A text string to search for.
#' @param path The folder to search (searches recursively). Defaults to the project root.
#' @return A vector of names of files that contain the string.
#'
#' @importFrom magrittr %>%
#' @export

find_files <- function(text, path = here::here()) {
  all_files <- c(
    list.files(path, recursive = TRUE, full.names = TRUE) %>%
      stringr::str_subset("\\.R$"),
    list.files(path, recursive = TRUE, full.names = TRUE) %>%
      stringr::str_subset("\\.Rmd$")
  )

  out <- c()

  for (i in 1:length(all_files)) {
    results <- grep(text, readLines(all_files[i]), value = FALSE) %>% suppressWarnings()

    if (length(results) > 0) {
      results <- paste(results, collapse = ", ")

      this_data <- c(all_files[i], results)

      out <- rbind(out, this_data)
    }

    percent <- (i / length(all_files) * 100) %>%
      round(digits = 0)

    if((i %% 10) == 0){
      print(paste(i, " files searched, ", percent, "% done", ".....", sep = ""))
    }
    
  }

  if (is.null(out)) {
    print("Not found")
  } else {
    colnames(out) <- c("file", "line(s)")
    return(out)
  }
}
