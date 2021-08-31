library(NEesp)

names <- list("allfh", "asmt", "asmt_sum", "bio_survey", "climate_risk",
           "climate_vulnerability", "com_catch", "com_palette",
           "cond", "guild_info", "guild_risk", "indicator_info",
           "latlong", "nrcc", "prey_palette", "rec_catch", "rec_palette",
           "regression_species_regions", "risk", "risk_species",
           "risk_year_hist", "risk_year_value", "shape", "species_guilds",
           "species_key", "survey", "swept")

names <- list(  'allfh', 'allfh_small', 'asmt_sum', 'asmt_sum_small', 'com_gear',
                'ichthyo', 'rec_catch_small', 'rec_effort', 'sp_group',
                'zoop_stock')

#zoop_stock <- NEesp::zoop_stock

my_save <- function(x) {
  data <- get(x)
  sinew::makeOxygen(data)
  
  print(x)
}

sink("R/data2.R")
lapply(names, my_save)
sink()
