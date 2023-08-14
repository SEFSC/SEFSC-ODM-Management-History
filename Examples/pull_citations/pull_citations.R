# Load packages ####

#install.packages("librarian")
librarian::shelf(here, tidyverse)

mh_data_log <- readRDS(here("ODM-MH-Data_log", "data", "processed", "MH_AL_2023Aug08.RDS"))

  #Define species of interest (create function later, with optional inputs)
  
  # EXAMPLE 1: CARIBBEAN YELLOWTAIL SNAPPER
  # short_name = "CR_YTS"
  # itis = 168907
  # region = "CARIBBEAN"
  
  # EXAMPLE 2: CARIBBEAN STOPLIGHT PARROTFISH
  short_name = "CR_SLP"
  itis = 170867
  region = "CARIBBEAN"

# FILTER TO A LSIT OF UNIQUE FR CITATIONS
list_citations <- mh_data_log |>
  ungroup() |>
  dplyr::filter(SPECIES_ITIS_USE == itis,
                REGION == region) |>
  unite(col = "FR_ACTION",
        ACTION, ACTION_TYPE, AMENDMENT_NUMBER, na.rm=TRUE, sep = ", ") |>
  dplyr::select(FR_CITATION, FR_ACTION) |>
  distinct() |>
  arrange(FR_CITATION)

# FILTER TO A LIST OF UNIQUE REGULATION TYPES AND FR CITATIONS
list_reg_types <- mh_data_log |>
  ungroup() |>
  dplyr::filter(SPECIES_ITIS_USE == itis,
                REGION == region) |>
  unite(col = "FR_ACTION",
        ACTION, ACTION_TYPE, AMENDMENT_NUMBER, na.rm=TRUE, sep = ", ") |>
  dplyr::select(FR_CITATION, FR_ACTION, MANAGEMENT_TYPE_USE, ZONE_USE, SPP_NAME) |>
  distinct() |>
  arrange(FR_CITATION)

# SAVE SPECIFIC LIST OF FR CITATIONS and MANAGMENT TYPES
write.csv(list_reg_types, here("Examples", "pull_citations", paste0(short_name, "_", format(Sys.Date(), "%Y%b%d"), ".csv")), row.names = FALSE)
