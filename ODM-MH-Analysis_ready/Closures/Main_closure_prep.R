# Process all regulations related to closures

# Load packages ####
#install.packages("librarian")
librarian::shelf(here, tidyverse, lubridate, dplyr, tidyr, neatRanges, splitstackshape)

# Read in MH Data Log
mh_data_log <- readRDS(here("ODM-MH-Data_log", "data", "results", "MH_DL_2024Dec16.RDS"))

# Select species and region
spp <- 'SNAPPER, RED'
region <- 'GULF OF MEXICO'

# Function to expand dates based on management status
source(here("ODM-MH-Analysis_ready", "Closures", "func_expand_status.R"))


# filter for species and region (all closure related records)
# Recode management types to have a value
mh_spp_closure <- mh_data_log %>%
  filter(COMMON_NAME_USE == spp, REGION == region,
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
                           # FR_CITATION_sale, 
                          #  FR_CITATION_spp,
                            na.rm = T)) %>%
  # Select the fishery status (open/closed) that applies to the most recent FR
  mutate(VALUE = case_when(FR_CITATION == FR_CITATION_close ~ VALUE_close,
                           FR_CITATION == FR_CITATION_sale ~ VALUE_sale,
                           FR_CITATION == FR_CITATION_spp ~ VALUE_spp,
                           FR_CITATION == FR_CITATION_season ~ VALUE_season,
                           FR_CITATION == FR_CITATION_year ~ VALUE_year)) #%>%
  select(-FR_CITATION_year, -VALUE_year, -FR_CITATION_season, -VALUE_season, -FR_CITATION_close, -VALUE_close)

summ_spp_closures <- spp_closure_story %>%
  mutate(YEAR = year(date_sequence)) %>%
  group_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR) %>% 
  arrange(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence) %>%
  mutate(change = VALUE != lag(VALUE, default = first(VALUE)) |
           YEAR != lag(YEAR, default = first(YEAR))) %>%
  mutate(group = cumsum(change)) %>%
  group_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR, VALUE, group) %>%
  summarize(ndays = n(),
            start = min(date_sequence),
            end = max(date_sequence),
            .groups = 'drop') %>%
  arrange(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, YEAR, start)

# Save as Excel to compare to SEDAR MH file
library(openxlsx)
write.xlsx(summ_spp_closures, "C:/Users/sarina.atkinson/Documents/Data/MH_closures/GM_RGR_closures.xlsx")

# Check GM RGR comm all closures
chk <- filter(mh_data_log, SECTOR_USE == 'COMMERCIAL', ZONE_USE == 'ALL', FR_CITATION == '55 FR 46955')

# Check GM RGR rec all closures - missing this FR which is in S88 MH file from Council
chk <- filter(mh_data_log, FR_CITATION == '80 FR 59665')

# Check SA speckled hind - some years 366 because of leap year
chk <- filter(spp_closure_story, year(date_sequence) == 2016, SECTOR_USE == 'COMMERCIAL', ZONE_USE == 'ALL')

# Check GM SM comm all closures
chk <- filter(spp_prohibited_sale, SECTOR_USE == 'COMMERCIAL', ZONE_USE == 'ALL', year(date_sequence) == 2001)
# Flag == yes because span mackerel can not be cut-off/damaged
chk2 <- filter(mh_spp_closure, REGULATION_ID == 3018)
# Look into flag==yes for prohibited sale and purchase
chk_sale <- filter(mh_data_log, MANAGEMENT_TYPE_USE == "PROHIBITED SALE AND PURCHASE") %>%
  group_by(REGION, COMMON_NAME_USE, FLAG) %>% summarise(nrecs = n())
# SA wahoo - 1 record flagged NO, 3 flagged YES
chk3 <- filter(mh_data_log, COMMON_NAME_USE == 'WAHOO', REGION == 'SOUTH ATLANTIC', MANAGEMENT_TYPE_USE == "PROHIBITED SALE AND PURCHASE") %>%
  select(REGULATION_ID, FR_CITATION, FR_SECTION, FR_URL, SECTOR_USE, SUBSECTOR_USE, ZONE_USE, FLAG)

# Check SA GTF comm closures
chk <- filter(spp_closures, SECTOR_USE == 'COMMERCIAL', ZONE_USE == 'ALL', year(date_sequence) == 2013)
chk2 <- filter(mh_spp_closure, REGULATION_ID == 167)

# Check SA GTF rec closures
chk <- filter(mh_spp_closure, FR_CITATION == '59 FR 27242')

# Check SA RS REC closures
chk <- filter(spp_closure_story, SECTOR_USE == 'RECREATIONAL', ZONE_USE == 'ALL', year(date_sequence) == 2021)
chk2 <- filter(mh_spp_closure, FR_CITATION == '75 FR 76874')
chk3 <- filter(mh_spp_closure, SECTOR_USE == 'RECREATIONAL') %>% arrange(START_DATE2) %>%
  select(CLUSTER, REGULATION_ID, FR_CITATION, SECTOR_USE, SUBSECTOR_USE, MANAGEMENT_TYPE_USE, VALUE,
         EFFECTIVE_DATE, INEFFECTIVE_DATE, START_DATE2, END_DATE2, 
         START_MONTH, START_DAY_USE, START_YEAR, END_MONTH_USE, END_DAY_USE, END_YEAR_USE, 
         REVERSION, NEVER_IMPLEMENTED) %>%
  filter(!(NEVER_IMPLEMENTED == 1 & REVERSION == TRUE))

# Check on SA RP COM closures
chk <- filter(spp_closure_story, year(date_sequence) ==2000, ZONE_USE == 'ALL', SECTOR_USE == 'COMMERCIAL')
chk2 <- filter(mh_data_log, FR_CITATION == '65 FR 51248')
