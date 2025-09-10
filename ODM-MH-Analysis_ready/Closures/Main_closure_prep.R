# Process all regulations related to closures

# Load packages ####
librarian::shelf(here, tidyverse, lubridate, dplyr, tidyr, neatRanges, splitstackshape)

# Read in MH Data Log
mh_data_log <- readRDS(here("ODM-MH-Data_log", "data", "results", "MH_DL_2025Mar28.RDS"))

# Select species and region
spp <- 'SNAPPER, RED'
region <- 'GULF OF MEXICO'

# Function to expand dates based on management status
source(here("ODM-MH-Analysis_ready", "Closures", "func_expand_status.R"))

# filter for species and region (all closure related records)
# Recode management types to have a value
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

unique(mh_spp_closure$MANAGEMENT_TYPE_USE)

# Fishing Year
chk <- filter(mh_spp_closure, MANAGEMENT_TYPE_USE == "FISHING YEAR")
spp_year <- expand_status(mh_spp_closure, "FISHING YEAR")

# Fishing Season - still needs some work if not entered consistently
chk <- filter(mh_spp_closure, MANAGEMENT_TYPE_USE == "FISHING SEASON")
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
# For now just comment out mtypes that do not apply
spp_closure_story <- spp_year %>%
  rename(FR_CITATION_year = "FR_CITATION",
         TIME_year = "TIME",
         VALUE_year = "VALUE") %>%
  select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_year, TIME_year, VALUE_year) %>%
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
 # full_join(spp_prohibited_sale %>%
   #          rename(FR_CITATION_sale = "FR_CITATION",
    #                TIME_sale = "TIME", 
    #                 VALUE_sale = "VALUE") %>%
     #         select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_sale, VALUE_sale),
      #      by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE,  date_sequence)) %>%
 # full_join(spp_prohibited_spp %>%
  #            rename(FR_CITATION_spp = "FR_CITATION",
  #                   TIME_spp = "TIME",
   #                  VALUE_spp = "VALUE") %>%
    #          select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, FR_CITATION_spp, VALUE_spp),
     #         by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence)) %>%
  arrange(SECTOR_USE, SUBSECTOR_USE, ZONE_USE, date_sequence) %>%
  # Select the most recent FR CITATION
  mutate(FR_CITATION = pmax(FR_CITATION_year, 
                            FR_CITATION_season, 
                            FR_CITATION_close, 
                           # FR_CITATION_sale, 
                            #FR_CITATION_spp,
                            na.rm = T),
         TIME = case_when(FR_CITATION == FR_CITATION_close ~ TIME_close,
                          #FR_CITATION == FR_CITATION_sale ~ TIME_sale,
                          #FR_CITATION == FR_CITATION_spp ~ TIME_spp,
                          FR_CITATION == FR_CITATION_season ~ TIME_season,
                          FR_CITATION == FR_CITATION_year ~ TIME_year)) %>%
  # Select the fishery status (open/closed) that applies to the most recent FR
  mutate(VALUE = case_when(FR_CITATION == FR_CITATION_close ~ VALUE_close,
                           #FR_CITATION == FR_CITATION_sale ~ VALUE_sale,
                           #FR_CITATION == FR_CITATION_spp ~ VALUE_spp,
                           FR_CITATION == FR_CITATION_season ~ VALUE_season,
                           FR_CITATION == FR_CITATION_year ~ VALUE_year)) %>%
  select(-FR_CITATION_year, -TIME_year, -VALUE_year, -FR_CITATION_close, -TIME_close, -VALUE_close, -FR_CITATION_season, -TIME_season, -VALUE_season) %>%
  # Create day_calc to indicate cases where the start/end time is midday 
  # midday start/end times should be counted toward both open and closed periods
  mutate(day_calc = case_when(TIME == "06:00:00 PM" ~ 0.5,
                              TIME == "12:00:00 PM" ~ 0.5,
                              TRUE ~ 1))

# Summarize the open and closed periods for each year
summ_spp_closures <- spp_closure_story %>%
  mutate(YEAR = year(date_sequence),
         VALUE = as.character(VALUE),
         VALUE = if_else(is.na(VALUE), "OPEN", VALUE)) %>%
  filter(VALUE %in% c("OPEN", "CLOSE"))

summ_spp_closures2 <- summ_spp_closures %>%
  filter(day_calc == 0.5) %>%
  mutate(VALUE = if_else(VALUE == "OPEN", "CLOSE", "OPEN"))

spp_closure_combined <- bind_rows(summ_spp_closures, summ_spp_closures2)

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


# Summarize the open and closed periods for each year
#summ_spp_closures <- spp_closure_story %>%
 # mutate(YEAR = year(date_sequence)) %>%
  #group_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR) %>% 
  #arrange(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence, TIME) %>%
  #mutate(change = VALUE != lag(VALUE, default = first(VALUE)) |
   #        YEAR != lag(YEAR, default = first(YEAR))) %>%
  #mutate(group = cumsum(change)) %>%
  #group_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR, VALUE, group) %>%
  #summarize(ndays = sum(day_calc, na.rm = TRUE),
   #         start = min(date_sequence),
    #        end = max(date_sequence),
     #       .groups = 'drop') %>%
  #mutate(VALUE = case_when(is.na(VALUE) ~ "OPEN",
   #                        TRUE ~ VALUE)) %>%
  #arrange(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR)

# Identify full range of years per group
all_years_by_group <- spp_close %>%
  distinct(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR, VALUE) %>%
  group_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE) %>%
  summarize(min_year = min(YEAR), max_year = max(YEAR), .groups = "drop") %>%
  rowwise() %>%
  mutate(YEAR = list(seq(min_year, max_year))) %>%
  unnest(YEAR) %>%
  select(-min_year, -max_year)

# Summarize the number of days open per year
summ_days_open <- spp_close %>%
  filter(VALUE == "OPEN") %>%
  group_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR) %>%
  summarize(Days_Open = sum(ndays), .groups = "drop") %>%
  arrange(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR)

open_by_year <- all_years_by_group %>%
  left_join(summ_days_open, by = c("FMP", "COMMON_NAME_USE", "REGION", "ZONE_USE", "SECTOR_USE", "SUBSECTOR_USE", "YEAR")) %>%
  mutate(Days_Open = replace_na(Days_Open, 0)) %>%
  arrange(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR)

# Save closure output as RDS file
saveRDS(open_by_year, here("ODM-MH-Analysis_ready", "Closures", "Closure_outputs", paste0('open_by_year', format(Sys.Date(), "%Y%b%d"), '.RDS')))

