# Process all regulations related to closures

# Load packages ####
#install.packages("librarian")
librarian::shelf(here, tidyverse, lubridate, openxlsx, neatRanges, splitstackshape)

# Read in MH Data Log
mh_data_log <- readRDS(here("ODM-MH-Data_log", "data", "results", "MH_DL_2024Feb07.RDS"))

# Select species and region
spp <- 'PORGY, RED'
region <- 'SOUTH ATLANTIC'

# Function to expand dates based on management status
source(here("ODM-MH-Analysis_ready", "func_expand_status.R"))

# filter for species and region (all closure related records)
# Recode management types to have a value
mh_spp_closure <- mh_data_log %>%
  filter(COMMON_NAME_USE == spp & REGION == region &
         (MANAGEMENT_TYPE_USE %in% c('CLOSURE', 'FISHING SEASON', 'FISHING YEAR', 'PROHIBITED SPECIES') |
            # Flag == NO means fishery wide prohibited sale and purchase (ie closure)
            MANAGEMENT_TYPE_USE == 'PROHIBITED SALE AND PURCHASE' & FLAG == 'NO')) %>%
  mutate(VALUE = case_when(MANAGEMENT_TYPE_USE %in% c('FISHING SEASON', 'FISHING YEAR') ~ 'OPEN',
                           MANAGEMENT_TYPE_USE %in% c('PROHIBITED SALE AND PURCHASE', 'PROHIBITED SPECIES') ~ 'CLOSE',
                           TRUE ~ VALUE)) %>%
  arrange(SECTOR_USE, START_DATE2)

unique(mh_spp_closure$MANAGEMENT_TYPE_USE)

# Fishing Year
unique(select(filter(mh_spp_closure, MANAGEMENT_TYPE_USE == "FISHING YEAR"), MANAGEMENT_STATUS_USE))
spp_year <- expand_status(mh_spp_closure, "FISHING YEAR")

# Fishing Season - still needs some work if not entered consistently
unique(select(filter(mh_spp_closure, MANAGEMENT_TYPE_USE == "FISHING SEASON"), MANAGEMENT_STATUS_USE))
spp_season <- expand_status(mh_spp_closure, "FISHING SEASON")

# Closure
unique(select(filter(mh_spp_closure, MANAGEMENT_TYPE_USE == "CLOSURE"), MANAGEMENT_STATUS_USE))
spp_closures <- expand_status(mh_spp_closure, "CLOSURE")

# Prohibited sale and purchase
unique(select(filter(mh_spp_closure, MANAGEMENT_TYPE_USE == "PROHIBITED SALE AND PURCHASE"), MANAGEMENT_STATUS_USE))
spp_prohibited_sale <- expand_status(mh_spp_closure, "PROHIBITED SALE AND PURCHASE")

# Prohibited species
unique(select(filter(mh_spp_closure, MANAGEMENT_TYPE_USE == "PROHIBITED SPECIES"), MANAGEMENT_STATUS_USE))
spp_prohibited_spp <- expand_status(mh_spp_closure, "PROHIBITED SPECIES")

# Combine all management types that refer to closures
spp_closure_story <- spp_year %>%
  rename(FR_CITATION_year = "FR_CITATION",
         VALUE_year = "VALUE") %>%
  select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_year, VALUE_year) %>%
  full_join(spp_season %>%
              rename(FR_CITATION_season = "FR_CITATION",
                     VALUE_season = "VALUE") %>%
              select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_season, VALUE_season),
            by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE,  date_sequence)) %>%
  full_join(spp_closures %>%
              rename(FR_CITATION_close = "FR_CITATION",
                     VALUE_close = "VALUE") %>%
              select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_close, VALUE_close),
            by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE,  date_sequence)) %>%
  full_join(spp_prohibited_sale %>%
             rename(FR_CITATION_sale = "FR_CITATION",
                     VALUE_sale = "VALUE") %>%
              select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_sale, VALUE_sale),
            by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE,  date_sequence)) %>%
  full_join(spp_prohibited_spp %>%
              rename(FR_CITATION_spp = "FR_CITATION",
                     VALUE_spp = "VALUE") %>%
              select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_spp, VALUE_spp),
            by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence)) %>%
  arrange(SECTOR_USE, SUBSECTOR_USE, ZONE_USE, date_sequence) %>%
  # Select the most recent FR CITATION
  mutate(FR_CITATION = pmax(FR_CITATION_year, 
                            FR_CITATION_season, 
                            FR_CITATION_close, 
                            FR_CITATION_sale, 
                            FR_CITATION_spp,
                            na.rm = T)) %>%
  # Select the fishery status (open/closed) that applies to the most recent FR
  mutate(VALUE = case_when(FR_CITATION == FR_CITATION_close ~ VALUE_close,
                           FR_CITATION == FR_CITATION_sale ~ VALUE_sale,
                           FR_CITATION == FR_CITATION_spp ~ VALUE_spp,
                           FR_CITATION == FR_CITATION_season ~ VALUE_season,
                           FR_CITATION == FR_CITATION_year ~ VALUE_year))

summ_spp_closures <- spp_closure_story %>%
  mutate(YEAR = year(date_sequence)) %>%
  group_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR, FR_CITATION, VALUE) %>% 
  summarize(ndays = n(),
            start = min(date_sequence),
            end = max(date_sequence)) %>%
  arrange(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR, start) %>%
  # Add back FR_URL for reference
  left_join(distinct(select(filter(ungroup(mh_data_log), FMP == 'SNAPPER-GROUPER FISHERY OF THE SOUTH ATLANTIC REGION'), FR_CITATION, FR_URL)),
            by = join_by(FR_CITATION))

# Save as Excel to compare to SEDAR MH file
write.xlsx(summ_spp_closures, "C:/Users/sarina.atkinson/Documents/Data/MH_closures/SA_RP_closures_08Feb24.xlsx")

