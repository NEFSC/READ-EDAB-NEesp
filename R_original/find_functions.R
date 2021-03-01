files <- list.files(here::here(), pattern = ".R", recursive = TRUE, full.names = TRUE)
files2 <- list.files(here::here(), pattern = ".Rmd", recursive = TRUE, full.names = TRUE)

readLines(files) %>% stringr::str_subset("get_var_data2")

# search files with a loop
out <- c()
for(i in 1:length(files2)){
  
  results <- grep("get_var_data2", readLines(files2[i]), value = FALSE)
  
  if(length(results) > 0){
    
    results <- paste(results, collapse = ", ")
    
    this_data <- c(files2[i], results)
    
    out <- rbind(out, this_data)
  }
  
}
print(out)