# Process all regulations related to closures

# Load packages ####
#install.packages("librarian")
librarian::shelf(here, tidyverse)

# Read in MH Data Log
mh_data_log <- readRDS(here("ODM-MH-Analysis_ready", "results", "MH_DL_wFY_2023Sep14.RDS"))

# Select species and region
spp <- 'PORGY, RED'
region <- 'SOUTH ATLANTIC'

# Process fishing season
source(here("ODM-MH-Analysis_ready", "Closures", "Fishing_season.R"))

# Process prohibited species or prohibited sale

# Process closures and re-openings

