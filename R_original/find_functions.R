files <- list.files(here::here(), pattern = ".R", recursive = TRUE, full.names = TRUE)
files2 <- list.files(here::here(), pattern = ".Rmd", recursive = TRUE, full.names = TRUE)

all_files <- c(files, files2) %>% unique()

find_files(all_files, "plot_recruit")

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
    
  }
  return(out)
}

