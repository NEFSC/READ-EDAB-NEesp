#' Changes "Goosefish" to "Monkfish" in a designated "Species" column
#'
#' This function changes any instances of "Goosefish" to "Monkfish" in a designated "Species" column
#' 
#' @param data A data frame or tibble
#' @param species_col Thename of the column with species names
#' @return The data frame, with any instances of "Goosefish" changed to "Monkfish"

update_species_names <- function(data, species_col){

  col_num <- which(colnames(data) == species_col)
  
  data <- data %>%
    dplyr::rename(Species = species_col) %>%
    dplyr::mutate(Species = Species %>%
                    stringr::str_replace("Goosefish", "Monkfish"))
  # add any other names that have to be changed in mutate() above
  
  colnames(data)[col_num] <- species_col
                    
  return(data)
}