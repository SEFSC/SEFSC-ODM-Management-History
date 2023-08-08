# Management History Processing ####

# Load packages ####
#install.packages("librarian")
librarian::shelf(here, tidyverse)

mh_analysis_use <- readRDS(here("data", "processed", "MH_AL_2023Jul28.RDS"))

car_sizelim <- mh_analysis_use |>
  dplyr::filter(REGION == "CARIBBEAN",
         MANAGEMENT_TYPE_USE == "MINIMUM SIZE LIMIT",
         SECTOR_USE == "COMMERCIAL")

new <- mh_analysis_use |>
  dplyr::filter(REGION == "CARIBBEAN") 
