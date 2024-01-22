# Process all regulations related to closures

# Load packages ####
#install.packages("librarian")
librarian::shelf(here, tidyverse)

# Read in MH Data Log
mh_data_log <- readRDS(here("ODM-MH-Analysis_ready", "results", "MH_DL_wFY_2024Jan19.RDS"))

# Select species and region
spp <- 'PORGY, RED'
region <- 'SOUTH ATLANTIC'

# filter for species and region (all closure related records)
mh_spp_closure <- mh_data_log %>%
  filter(COMMON_NAME_USE == spp, REGION == region,
         MANAGEMENT_TYPE_USE %in% c('CLOSURE', 'FISHING SEASON', 
                                    'PROHIBITED SALE AND PURCHASE',
                                    'PROHIBITED SPECIES'))

# Create empty dataframe of dates

# Process fishing season and expand
source(here("ODM-MH-Analysis_ready", "Closures", "Fishing_season.R"))

# Process prohibited species or prohibited sale and expand
# Recode as "CLOSURE" then process like one-time or recurring closure
mh_spp_closure <- mutate(mh_spp_closure, 
                         MANAGEMENT_TYPE_USE = case_when(MANAGEMENT_TYPE %in% c('PROHIBITED SALE AND PURCHASE','PROHIBITED SPECIES') ~ 'CLOSURE',
                                                         T ~ MANAGEMENT_TYPE_USE),
                         VALUE = case_when(MANAGEMENT_TYPE %in% c('PROHIBITED SALE AND PURCHASE','PROHIBITED SPECIES') ~ 'CLOSE',
                                                         T ~ VALUE))

# Process closures and re-openings and expand
source(here("ODM-MH-Analysis_ready", "Closures", "Closure_Reopening.R")) 

# Join everything - overwrite

# Collapse
