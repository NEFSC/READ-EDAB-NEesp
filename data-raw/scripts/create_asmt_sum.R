`%>%` <- dplyr::`%>%`

## assessmentdata::stockAssessmentSummary ----
# this is from the OLD version of assessmentdata
assessmentdata::stockAssessmentSummary %>%
  write.csv(here::here("data-raw", "assessmentdata_stockAssessmentSummary.csv"))

# assessmentdata summary ----
# asmt_sum <- assessmentdata::stockAssessmentSummary  # summary data from before 2019 has been removed from package, no new data added though
asmt_sum <- read.csv(here::here("data-raw", "assessmentdata_stockAssessmentSummary.csv"),
  check.names = FALSE,
  header = TRUE
)[, -1] %>%
  tibble::as_tibble()

asmt_sum <- asmt_sum %>%
  dplyr::filter(Jurisdiction == "NEFMC" |
    Jurisdiction == "NEFMC / MAFMC" |
    Jurisdiction == "MAFMC")


split_info <- stringr::str_split_fixed(asmt_sum$`Stock Name`, " - ", n = 2)
asmt_sum$Species <- split_info[, 1]
asmt_sum$Region <- split_info[, 2]

asmt_sum <- asmt_sum %>%
  update_species_names(species_col = "Species")

usethis::use_data(asmt_sum)
