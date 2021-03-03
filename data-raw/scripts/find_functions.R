all_files <- c(list.files(here::here(), recursive = TRUE, full.names = TRUE) %>%
             stringr::str_subset("\\.R$"),
           list.files(here::here(), recursive = TRUE, full.names = TRUE) %>%
             stringr::str_subset("\\.Rmd$"))

find_files(all_files, "Export_All")

# search files with a loop
find_files <- function(x, text){
  out <- c()
  for(i in 1:length(x)){
    
    results <- grep(text, readLines(x[i]), value = FALSE) %>% suppressWarnings()
    
    if(length(results) > 0){
      
      results <- paste(results, collapse = ", ")
      
      this_data <- c(x[i], results)
      
      out <- rbind(out, this_data)
    }
    
    percent <- (i/length(x) * 100) %>%
      round(digits = 0)
    print(paste(i, ", ", percent, "%", ".....", sep = ""))
    
  }
  
  if(is.null(out)) {print("Not found")} else return(out)
}

