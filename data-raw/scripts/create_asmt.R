`%>%` <- dplyr::`%>%`

# assessmentdata ----
asmt <- assessmentdata::stockAssessmentData %>%
  dplyr::filter(Region == "Gulf of Maine / Georges Bank" |
    Region == "Eastern Georges Bank" |
    Region == "Georges Bank" |
    Region == "Gulf of Maine" |
    Region == "Gulf of Maine / Cape Hatteras" |
    Region == "Georges Bank / Southern New England" |
    Region == "Southern New England / Mid" |
    Region == "Gulf of Maine / Northern Georges Bank" |
    Region == "Southern Georges Bank / Mid" |
    Region == "Cape Cod / Gulf of Maine" |
    Region == "Northwestern Atlantic Coast" |
    Region == "Atlantic" |
    Region == "Western Atlantic" |
    Region == "Atlantic Coast" |
    Region == "Georges Bank / Cape Hatteras" |
    Region == "Northwestern Atlantic" |
    Region == "Mid")
asmt$Region <- asmt$Region %>%
  stringr::str_replace("Mid", "Mid-Atlantic")
asmt$Species <- asmt$Species %>%
  stringr::str_to_sentence()

asmt <- asmt %>%
  update_species_names(species_col = "Species")

for (i in 1:nrow(asmt)) {
  if (asmt$Metric[i] == "Abundance") {
    if (asmt$Units[i] == "Metric Tons" |
      asmt$Units[i] == "Kilograms/Tow" |
      asmt$Units[i] == "Kilograms / Tow" |
      asmt$Units[i] == "Thousand Metric Tons" |
      asmt$Units[i] == "Average Kilograms / Tow" |
      asmt$Units[i] == "mt" |
      asmt$Units[i] == "Average Kilograms/Tow") {
      asmt$Metric[i] <- "Biomass"
    }
  }
}

# standardize units...
for (i in 1:nrow(asmt)) {
  if (asmt$Units[i] == "Number x 1,000" |
    asmt$Units[i] == "Number x 1000" |
    asmt$Units[i] == "Thousand Recruits") {
    asmt$Value[i] <- asmt$Value[i] * 10^3
    asmt$Units[i] <- "Number"
  }
  if (asmt$Units[i] == "Million Recruits" |
    asmt$Units[i] == "Number x 1,000,000") {
    asmt$Value[i] <- asmt$Value[i] * 10^6
    asmt$Units[i] <- "Number"
  }
  if (asmt$Units[i] == "Number x 10,000") {
    asmt$Value[i] <- asmt$Value[i] * 10^4
    asmt$Units[i] <- "Number"
  }
  if (asmt$Units[i] == "Recruits") {
    asmt$Units[i] <- "Number"
  }
  if (asmt$Units[i] == "Thousand Metric Tons") {
    asmt$Value[i] <- asmt$Value[i] * 10^3
    asmt$Units[i] <- "Metric Tons"
  }
  if (asmt$Units[i] == "mt") {
    asmt$Units[i] <- "Metric Tons"
  }
  if (asmt$Units[i] == "Million Metric Tons") {
    asmt$Value[i] <- asmt$Value[i] * 10^6
    asmt$Units[i] <- "Metric Tons"
  }
}

asmt$Description <- asmt$Description %>%
  stringr::str_replace(" - ", ", ") %>%
  stringr::str_replace("e Age", "e, Age")

new_desc <- asmt$Description %>%
  stringr::str_split_fixed(" \\(", 2)

details <- new_desc[, 2] %>%
  stringr::str_replace("\\)", "")

new_desc2 <- new_desc[, 1] %>%
  stringr::str_split_fixed(", ", 2)

basic <- new_desc2[, 1]
detail_or_age <- new_desc2[, 2]

age <- c()
for (i in 1:length(details)) {
  if (detail_or_age[i] %>% stringr::str_detect("Age") == TRUE) {
    age[i] <- detail_or_age[i]
  }
  if (details[i] %>% stringr::str_detect("Age") == TRUE) {
    age[i] <- details[i]
  }
  if (details[i] %>% stringr::str_detect("Age") == FALSE &
    detail_or_age[i] %>% stringr::str_detect("Age") == FALSE) {
    age[i] <- "No age"
  }
}

asmt$Age <- age

category <- c()
for (i in 1:nrow(asmt)) {
  if (asmt$Description[i] %>% stringr::str_detect("Mature") == TRUE) {
    category[i] <- "Mature"
  }
  if (asmt$Description[i] %>% stringr::str_detect("Spawning Stock") == TRUE |
    asmt$Description[i] %>% stringr::str_detect("SSB") == TRUE) {
    category[i] <- "Spawning Stock"
  }
  if (asmt$Description[i] %>% stringr::str_detect("Survey") == TRUE) {
    category[i] <- "Survey"
  }
  if (asmt$Description[i] %>% stringr::str_detect("Survey") == FALSE &
    asmt$Description[i] %>% stringr::str_detect("Mature") == FALSE &
    asmt$Description[i] %>% stringr::str_detect("Spawning Stock") == FALSE &
    asmt$Description[i] %>% stringr::str_detect("SSB") == FALSE
  ) {
    category[i] <- "Other"
  }
}
asmt$Category <- category

usethis::use_data(asmt)

# asmt$Units <- asmt$Units %>%
# stringr::str_replace("Thousand Recruits", "Number x 1,000")
