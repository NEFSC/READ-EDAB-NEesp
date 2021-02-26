#' Changes the class of `character` columns to `factor`
#'
#' This function changes `character` columns to class `factor` for better rendering as an html table with the `DT` package.
#' 
#' @param x A data frame or tibble
#' @return The data frame, with `character` columns reclassified as `factor`

character_to_factor <- function(x){
  if(is.null(x) == FALSE){
    if(nrow(x) > 0){
      for(i in 1:ncol(x)){
        x <- as.data.frame(x)
        if(class(x[ , i]) == "character"){
          x[ , i] <- as.factor(x[ , i])
        }
      }
      return(x)
    }
  }
}
