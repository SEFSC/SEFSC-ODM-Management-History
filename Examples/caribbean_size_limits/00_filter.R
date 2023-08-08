# Management History Processing ####

# Load packages ####
#install.packages("librarian")
librarian::shelf(here, tidyverse)

mh_analysis_use <- readRDS(here("ODM-MH-Data_log", "data", "processed", "MH_AL_2023Jul28.RDS"))

car_sizelim <- mh_analysis_use |>
  dplyr::filter(REGION == "CARIBBEAN",
         MANAGEMENT_TYPE_USE == "MINIMUM SIZE LIMIT",
         SECTOR_USE == "COMMERCIAL")

test <- mh_data_log |>
  dplyr::filter(REGION == "CARIBBEAN",
                MANAGEMENT_TYPE_USE == "MINIMUM SIZE LIMIT",
                SECTOR_USE == "COMMERCIAL")

FR34850 <- mh_analysis_use |>
  dplyr::filter(FR_CITATION == "50 FR 34850")
nrow(FR34850)

new <- mh_analysis_use |>
  dplyr::filter(REGION == "CARIBBEAN") 
