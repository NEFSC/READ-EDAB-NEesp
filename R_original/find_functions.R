files <- list.files(here::here(), pattern = ".R", recursive = TRUE, full.names = TRUE)
files2 <- list.files(here::here(), pattern = ".Rmd", recursive = TRUE, full.names = TRUE)

all_files <- c(files, files2) %>% unique()

# search files with a loop
out <- c()
for(i in 1:length(all_files)){
  
  results <- grep("plot_recruit", readLines(all_files[i]), value = FALSE)
  
  if(length(results) > 0){
    
    results <- paste(results, collapse = ", ")
    
    this_data <- c(all_files[i], results)
    
    out <- rbind(out, this_data)
  }
  
}
print(out)