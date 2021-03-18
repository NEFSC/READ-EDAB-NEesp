library(NEesp)

names <- list("allfh", "asmt", "asmt_sum", "bio_survey", "climate_risk",
           "climate_vulnerability", "com_catch", "com_palette",
           "cond", "guild_info", "guild_risk", "indicator_info",
           "latlong", "nrcc", "prey_palette", "rec_catch", "rec_palette",
           "regression_species_regions", "risk", "risk_species",
           "risk_year_hist", "risk_year_value", "shape", "species_guilds",
           "species_key", "survey", "swept")

my_save <- function(x) {
  data <- get(x)
  sinew::makeOxygen(data)
  
  print(x)
}

sink("R/data.R")
lapply(names, my_save)
sink()
