#### Closures ####
# Code creates a comprehensive, expanded time series of fishery status (OPEN/CLOSE)
# by incorporating all closure-related regulations.
# The first output from this code indicates the days open per year by fishery (commercial/recreational)
# The second output from this code displays the continuous OPEN/CLOSE periods with start/end dates

# Load packages ####
librarian::shelf(here, tidyverse, lubridate, dplyr, tidyr, neatRanges, splitstackshape)

# Read in MH Data Log ####
mh_data_log <- readRDS(here("ODM-MH-Data_log", "data", "results", "MH_DL_2025Sep10.RDS"))

# Select species and region of interest for the closure time series ####
spp <- 'SNAPPER, RED'
region <- 'SOUTH ATLANTIC'

# Function to expand dates based on management status ####
# Function expands records for a given MANAGEMENT_TYPE_USE into a daily date sequence with
# the status of the fishery (OPEN/CLOSED) indicated for each day based on the MANAGEMENT_STATUS_USE 
# (ONCE/SEASONAL/RECURRING) of the applicable regulations
source(here("ODM-MH-Analysis_ready", "Closures", "func_expand_status.R"))

# Filter and reclassify closure-related records ####
# Retain only species and region of interest and management types that impact the status of the fishery
# Provide management types that impact fishery status with a VALUE of OPEN or CLOSE
# FISHING SEASON/FISHING YEAR implies that the fishery is OPEN during the defined window
# PROHIBITED SALE AND PURCHASE 
#    - If FLAG = YES -> OPEN (sale is allowed)
#    - If FLAG = NO -> CLOSE (sale is not allowed)
# PROHIBITED SPECIES implies the fishery is closed as species cannot be retained
# CLOSURE VALUE already indicates fishery status
mh_spp_closure <- mh_data_log %>%
  filter(COMMON_NAME_USE == spp,
         REGION == region,
         MANAGEMENT_TYPE_USE %in% c('CLOSURE', 'FISHING SEASON', 'FISHING YEAR',
                                    'PROHIBITED SALE AND PURCHASE',
                                    'PROHIBITED SPECIES')) %>%
  mutate(VALUE = case_when(MANAGEMENT_TYPE_USE %in% c('FISHING SEASON', 'FISHING YEAR') ~ 'OPEN',
                           MANAGEMENT_TYPE_USE == 'PROHIBITED SALE AND PURCHASE' & FLAG == 'YES' ~ 'OPEN',
                           MANAGEMENT_TYPE_USE == 'PROHIBITED SALE AND PURCHASE' & FLAG == 'NO' ~ 'CLOSE',
                           MANAGEMENT_TYPE_USE == 'PROHIBITED SPECIES' ~ 'CLOSE',
                           TRUE ~ VALUE)) %>%
  arrange(SECTOR_USE, START_DATE2)

# Check to see which MANAGEMENT_TYPE_USE are present after filtering
unique(mh_spp_closure$MANAGEMENT_TYPE_USE)

# Date expansion by MANAGEMENT_TYPE_USE ####
# Each section expands the raw rules for the applicable MANAGEMENT_TYPE_USE into a 
# series by date with the associated status (OPEN/CLOSE) and START/END TIME

# Fishing Year expansion
unique(select(filter(mh_spp_closure, MANAGEMENT_TYPE_USE == "FISHING YEAR"), MANAGEMENT_STATUS_USE))
spp_year <- expand_status(mh_spp_closure, "FISHING YEAR")

# Fishing Season expansion 
# Portion may require some adjustment if fishing year has not been documented consistently in the raw data
unique(select(filter(mh_spp_closure, MANAGEMENT_TYPE_USE == "FISHING SEASON"), MANAGEMENT_STATUS_USE))
spp_season <- expand_status(mh_spp_closure, "FISHING SEASON")

# Closure expansion
unique(select(filter(mh_spp_closure, MANAGEMENT_TYPE_USE == "CLOSURE"), MANAGEMENT_STATUS_USE))
spp_closures <- expand_status(mh_spp_closure, "CLOSURE")

# Prohibited Sale and Purchase expansion
unique(select(filter(mh_spp_closure, MANAGEMENT_TYPE_USE == "PROHIBITED SALE AND PURCHASE"), MANAGEMENT_STATUS_USE))
spp_prohibited_sale <- expand_status(mh_spp_closure, "PROHIBITED SALE AND PURCHASE")

# Prohibited Species expansion
unique(select(filter(mh_spp_closure, MANAGEMENT_TYPE_USE == "PROHIBITED SPECIES"), MANAGEMENT_STATUS_USE))
spp_prohibited_spp <- expand_status(mh_spp_closure, "PROHIBITED SPECIES")

# Join all expanded series into one time series ####
# For days with multiple statuses based on multiple regulations, select the most recent FR Citation
spp_closure_story <- spp_year %>%
  rename(FR_CITATION_year = "FR_CITATION",
         TIME_year = "TIME",
         VALUE_year = "VALUE") %>%
  select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_year, TIME_year, VALUE_year) %>%
  # Comment out MANAGEMENT_TYPE_USE that do not apply to your filter criteria
  full_join(spp_season %>%
            rename(FR_CITATION_season = "FR_CITATION",
                     TIME_season = "TIME",
                    VALUE_season = "VALUE") %>%
              select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_season, TIME_season, VALUE_season),
            by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE,  date_sequence)) %>%
  full_join(spp_closures %>%
              rename(FR_CITATION_close = "FR_CITATION",
                     TIME_close = "TIME",
                     VALUE_close = "VALUE") %>%
              select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_close, TIME_close, VALUE_close),
            by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE,  date_sequence)) %>%
  full_join(spp_prohibited_sale %>%
             rename(FR_CITATION_sale = "FR_CITATION",
                    TIME_sale = "TIME", 
                     VALUE_sale = "VALUE") %>%
              select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_sale, VALUE_sale),
            by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE,  date_sequence)) %>%
  full_join(spp_prohibited_spp %>%
              rename(FR_CITATION_spp = "FR_CITATION",
                     TIME_spp = "TIME",
                     VALUE_spp = "VALUE") %>%
              select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_spp, VALUE_spp),
              by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence)) %>%
  arrange(SECTOR_USE, SUBSECTOR_USE, ZONE_USE, date_sequence) %>%
  # Select the most recent FR CITATION and retain its TIME/VALUE
  mutate(FR_CITATION = pmax(FR_CITATION_year, 
                            FR_CITATION_season, 
                            FR_CITATION_close, 
                            FR_CITATION_sale, 
                            FR_CITATION_spp,
                            na.rm = T),
         TIME = case_when(FR_CITATION == FR_CITATION_close ~ TIME_close,
                          FR_CITATION == FR_CITATION_sale ~ TIME_sale,
                          FR_CITATION == FR_CITATION_spp ~ TIME_spp,
                         FR_CITATION == FR_CITATION_season ~ TIME_season,
                          FR_CITATION == FR_CITATION_year ~ TIME_year)) %>%
  mutate(VALUE = case_when(FR_CITATION == FR_CITATION_close ~ VALUE_close,
                           FR_CITATION == FR_CITATION_sale ~ VALUE_sale,
                           FR_CITATION == FR_CITATION_spp ~ VALUE_spp,
                           FR_CITATION == FR_CITATION_season ~ VALUE_season,
                           FR_CITATION == FR_CITATION_year ~ VALUE_year)) %>%
  # Remove management type-specific columns now that a single FR/TIME/VALUE has been selected for each day
  select(-FR_CITATION_year, -TIME_year, -VALUE_year, -FR_CITATION_close, -TIME_close, -VALUE_close, -FR_CITATION_sale, -VALUE_sale, -TIME_sale, -FR_CITATION_season, -VALUE_season, -TIME_season) %>%
  # How to handle half days
  # Some FR notices take effect midday or later. To correctly count days for annual tallies, treat noon/6pm
  # TIMEs as half-days) (0.5)
  # This ensures that a midday start/end time contributes to both OPEN and CLOSED period calculations
  mutate(day_calc = case_when(TIME == "06:00:00 PM" ~ 0.5,
                              TIME == "12:00:00 PM" ~ 0.5,
                              TRUE ~ 1))

# Summarize the open and closed periods for each year ####
# Fill in any mission VALUE as OPEN (assumes that any window without an applicable rule is open)
summ_spp_closures <- spp_closure_story %>%
  mutate(YEAR = year(date_sequence),
         VALUE = as.character(VALUE),
         VALUE = if_else(is.na(VALUE), "OPEN", VALUE)) %>%
  filter(VALUE %in% c("OPEN", "CLOSE"))

# Create half-day complements ####
# When a day is counted as 0.5 under one status (OPEN/CLOSED), add a complementary 0.5 day to 
# the opposite status so that the calendar day sums to 1 acorss OPEN/CLOSE
summ_spp_closures2 <- summ_spp_closures %>%
  filter(day_calc == 0.5) %>%
  mutate(VALUE = if_else(VALUE == "OPEN", "CLOSE", "OPEN"))

# Combine whole-day and half-day rows for final daily time series
spp_closure_combined <- bind_rows(summ_spp_closures, summ_spp_closures2)

# Collapse individual days into continuous periods within each year ####
# Group consecutive days with the same VALUE into periods and compute total days within the period (ndays)
# and list the start/end as the first/last days within the period
spp_close_sum <- spp_closure_combined %>%
  group_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR) %>% 
  arrange(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, TIME) %>%
  mutate(change = VALUE != lag(VALUE, default = first(VALUE)) |
           YEAR != lag(YEAR, default = first(YEAR))) %>%
  mutate(group = cumsum(change)) %>%
  group_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR, VALUE, group) %>%
  summarize(ndays = sum(day_calc, na.rm = TRUE),
            start = min(date_sequence),
            end = max(date_sequence),
            .groups = 'drop') %>%
  arrange(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR, start)


# Identify full range of years per group ####
# Determine the min/max year of regulations documented and expand to all years between so that even years with zero regulations 
# are accounted for in the expansion
all_years_by_group <- spp_close_sum %>%
  distinct(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR, VALUE) %>%
  group_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE) %>%
  summarize(min_year = min(YEAR), max_year = max(YEAR), .groups = "drop") %>%
  rowwise() %>%
  mutate(YEAR = list(seq(min_year, max_year))) %>%
  unnest(YEAR) %>%
  select(-min_year, -max_year)

# Summarize the number of days open per year ####
# Collapse to annual totals of OPEN days per fishery grouping
summ_days_open <- spp_close_sum %>%
  filter(VALUE == "OPEN") %>%
  group_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR) %>%
  summarize(Days_Open = sum(ndays), .groups = "drop") %>%
  arrange(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR)

# Ensure every year in the full span is represented
# Fill in missing years as zero
open_by_year <- all_years_by_group %>%
  left_join(summ_days_open, by = c("FMP", "COMMON_NAME_USE", "REGION", "ZONE_USE", "SECTOR_USE", "SUBSECTOR_USE", "YEAR")) %>%
  mutate(Days_Open = replace_na(Days_Open, 0)) %>%
  arrange(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR)

# Save annual open days as RDS file
saveRDS(open_by_year, here("ODM-MH-Analysis_ready", "Closures", "Closure_outputs", paste0('Open_', spp, format(Sys.Date(), "%Y%b%d"), '.RDS')))

# Save closure periods output as RDS file
saveRDS(spp_close_sum, here("ODM-MH-Analysis_ready", "Closures", "Closure_outputs", paste0('Close_', spp, format(Sys.Date(), "%Y%b%d"), '.RDS')))
