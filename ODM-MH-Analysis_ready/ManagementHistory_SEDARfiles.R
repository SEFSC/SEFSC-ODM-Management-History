#### Management History Documentation ####
# This script creates standardized tables of management regulations for use in SEDAR stock assessment processes.
# It compiles data from the Management History Data Log and the closure expansion process into clean formats
# that summarize management values, such as ACLs, size limits, trip limits, bag limits, and closure periods by 
# year for commercial and recreational sectors.
# The first portion of this script creates a table that can be more easily utilized by data analysts
# The second portion of this script creates tables that can be more easily utilized in reports

# Part 1. Data Analyst Tables ####
##### Load packages ####
librarian::shelf(here, tidyverse, gt, flextable, officer, dplyr, gt, officer, lubridate, htmltools, rmarkdown, tidyr, openxlsx, stringr)

#### Load data ####
# Load the MH Data Log
# This includes the full regulatory time series across species and FMPs
mh <- readRDS(here("ODM-MH-Data_log", "data", "results", "MH_DL_2025Sep10.RDS"))
# Correct the order for common name
mh <- mh %>%
  mutate(COMMON_NAME_USE = str_to_title(trimws(gsub("^(.*), (.*)$", "\\2 \\1", COMMON_NAME_USE))))

# Load Closure file for species of interest
closures <- readRDS(here("ODM-MH-Analysis_ready", "Closures", "Closure_outputs", "Closure_SNAPPER, RED22025Sep22.RDS"))

#### Define species and region of interest ####
spp = 'Red Snapper'
region = 'SOUTH ATLANTIC'


#### Commercial sector processing ####
# Retain regulations for species and region of interest
# Remove non-detailed regulations and those that were never implemented
# Retain only full-region (ZONE_USE = ALL) and sector-wide (COMMERCIAL/ALL) regulations
mh_commercial <- mh %>%
  filter(COMMON_NAME_USE == spp,
         REGION == region,
         ZONE_USE == "ALL",
         DETAILED == "YES",
         NEVER_IMPLEMENTED %in% c(0, NA),
         SECTOR_USE == "COMMERCIAL",
         SUBSECTOR_USE == "ALL") %>%
  # Format data for table columns
  # FMP_START = month-day when the FMP began
  # YEAR = extract the year value from the START_DATE2 of the regulation
  # Extract the value, start, end, and FR notice information for all of the MANAGEMENT_TYPE_USE of interest
  mutate(FMP_START = case_when(ACTION_TYPE == "ORIGINAL FMP" ~ format(START_DATE2, "%m-%d"),
                                    TRUE ~ NA),
         YEAR = year(START_DATE2),
         ACL = case_when(MANAGEMENT_TYPE_USE == "ACL" ~ VALUE,
                         TRUE ~ NA),
         acl_value_prep = case_when(MANAGEMENT_TYPE_USE == "ACL" & VALUE_UNITS == "POUNDS" ~ "lbs",
                                    TRUE ~ NA),
         acl_type_prep = case_when(MANAGEMENT_TYPE_USE == "ACL" & VALUE_TYPE == "GUTTED WEIGHT" ~ "gw",
                                   MANAGEMENT_TYPE_USE == "ACL" & VALUE_TYPE == "ROUND (WHOLE) WEIGHT" ~ "ww",
                                   TRUE ~ NA),
         ACL_UNIT = case_when(MANAGEMENT_TYPE_USE == "ACL" ~ paste(acl_value_prep, " ", acl_type_prep),
                              TRUE ~ NA),
         ACL_FR = case_when(MANAGEMENT_TYPE_USE == "ACL" ~ FR_CITATION,
                            TRUE ~ NA),
         MIN_SIZE = case_when(MANAGEMENT_TYPE_USE == "MINIMUM SIZE LIMIT" ~ VALUE,
                              TRUE ~ NA),
         MIN_START = case_when(MANAGEMENT_TYPE_USE == "MINIMUM SIZE LIMIT" ~ START_DATE2,
                               TRUE ~ NA), 
         MIN_END = case_when(MANAGEMENT_TYPE_USE == "MINIMUM SIZE LIMIT" ~ END_DATE2,
                             TRUE ~ NA),
         MIN_FR = case_when(MANAGEMENT_TYPE_USE == "MINIMUM SIZE LIMIT" ~ FR_CITATION,
                            TRUE ~ NA),
         TRIP_LIMIT = case_when(MANAGEMENT_TYPE_USE == "TRIP LIMIT" ~ VALUE,
                                TRUE ~ NA),
         TRIP_START = case_when(MANAGEMENT_TYPE_USE == "TRIP LIMIT" ~ START_DATE2,
                                TRUE ~ NA),
         TRIP_END = case_when(MANAGEMENT_TYPE_USE == "TRIP LIMIT" ~ END_DATE2,
                              TRUE ~ NA),
         TRIP_FR = case_when(MANAGEMENT_TYPE_USE == "TRIP LIMIT" ~ FR_CITATION,
                             TRUE ~ NA))
         
#### Determine Time Range ####
# Define the first year in the time series (min_year), the current year, and the terminal year
# Terminal year definition may need to be adjusted
mh_year_commercial <- mh_commercial %>%
  summarise(min_year = min(YEAR, na.rm = TRUE)) %>%
  mutate(current_year = year(Sys.Date()),
         terminal_year = current_year - 1)

#### Create a complete year sequence ####
# Year sequence should start from first year in time series to terminal year
years <- tibble(YEAR = mh_year_commercial$min_year:mh_year_commercial$terminal_year)

#### Add Closure information ####
# Filter to commercial closures for the entire region 
# Convert CLOSE/OPEN to sentence case
closure_commercial <- closures %>%
  filter(SECTOR_USE == "COMMERCIAL") %>%
  filter(ZONE_USE == "ALL") %>%
  mutate(VALUE_CLOSURE = case_when(VALUE == "CLOSE" ~ "CLOSED",
                                   TRUE ~ VALUE),
         VALUE_CLOSURE = str_to_sentence(VALUE_CLOSURE))

#### Expand regulations across applicable years ####
# Each regulation is expanded across all years between its START and END date information
mh_commercial_expanded <- mh_commercial %>%
  mutate(START_YEAR = year(START_DATE2), 
         END_YEAR = year(END_DATE2)) %>%
  rowwise() %>%
  mutate(YEAR_TEMP = list(START_YEAR:ifelse(is.na(END_YEAR), START_YEAR, END_YEAR))) %>%
  unnest(cols = c(YEAR_TEMP)) %>%  
  ungroup()

#### Ensure all years from min_year to current_year are included in full sequence
mh_commercial_complete <- years %>%
  full_join(mh_commercial_expanded, by = c("YEAR" = "YEAR_TEMP"))

#### Fill missing values for regulations so that each year has regulation values
mh_commercial_complete <- mh_commercial_complete %>%
  arrange(YEAR) %>%
  group_by(YEAR) %>%
  fill(ACL, ACL_UNIT, ACL_FR, MIN_SIZE, MIN_START, MIN_END, MIN_FR,
       TRIP_LIMIT, TRIP_START, TRIP_END, TRIP_FR, .direction = "downup") %>%
  ungroup()

#### Join closure data ####
# Combine closure days with regulation records
# If closure not available, default to full open year (Jan 1-Dec 31)
mh_commercial_closure <- mh_commercial_complete %>%
  left_join(closure_commercial, by = "YEAR") %>%
  mutate(VALUE_CLOSURE = ifelse(is.na(VALUE_CLOSURE), "Open", VALUE_CLOSURE),
    start = case_when(is.na(start) ~ as.Date(paste0(YEAR, "-01-01")),
      TRUE ~ start),
    end = case_when(is.na(end) ~ as.Date(paste0(YEAR, "-12-31")),
      TRUE ~ end),
    ndays = case_when(is.na(ndays) ~ as.numeric(end - start + 1),
      TRUE ~ ndays)) 

#### Collapse duplicate rows ####
# Retain multiple rows per YEAR with if there are distinct combinations of VALUE_CLOSURE, start, and end
mh_commercial_closure <- mh_commercial_closure %>%
  group_by(YEAR, VALUE_CLOSURE) %>%
  arrange(start, end) %>%
  mutate(regulation_change = case_when(lag(MIN_SIZE, default = first(MIN_SIZE)) != MIN_SIZE ~ TRUE,
                                       lag(TRIP_LIMIT, default = first(TRIP_LIMIT)) != TRIP_LIMIT ~ TRUE,
                                       TRUE ~ FALSE),
         period_change = case_when(lag(VALUE_CLOSURE, default = first(VALUE_CLOSURE)) != VALUE_CLOSURE ~ TRUE,
                                   lag(start, default = first(start)) != start ~ TRUE,
                                   TRUE ~ FALSE),
         change_flag = regulation_change | period_change) %>%
  filter(row_number() == 1 | change_flag) %>%
  ungroup()

#### Adjust start/end dates ####
# Align regulation start/end dates to closure periods
mh_commercial_closure <- mh_commercial_closure %>%
    # Adjust START dates for each regulation type based on season open and closed dates
  mutate(FMP_START_date = as.Date(FMP_START, format = "%Y-%m-%d"), 
         MIN_START = case_when(!is.na(MIN_START) & year(MIN_START) == YEAR & month(MIN_START) > month(start) ~ MIN_START,  # Keep MIN_START if same year and regulation month is after start of fishing year
                               !is.na(MIN_START) & year(MIN_START) == YEAR & month(MIN_START) < month(start) ~ start, # Keep start if same year and regulation month is before start of fishing year
                               !is.na(MIN_START) & year(MIN_START) != YEAR ~ start, # Otherwise, use start if year is not the same
                               TRUE ~ MIN_START),
         TRIP_START = case_when(!is.na(TRIP_START) & year(TRIP_START) == YEAR & month(TRIP_START) > month(start) ~ TRIP_START,  # Keep TRIP_START if same year and month is after start of fishing year
                                !is.na(TRIP_START) & year(TRIP_START) == YEAR & month(TRIP_START) < month(start) ~ start, # Keep start if same year and regulation month is before start of fishing year
                                !is.na(TRIP_START) & year(TRIP_START) != YEAR ~ start, # Otherwise, use start if year is not the same
                                TRUE ~ TRIP_START),
    # Adjust END dates for each regulation type based on season open and closed dates
         MIN_END = case_when(!is.na(MIN_END) & year(MIN_END) == YEAR & month(MIN_END) < month(end) ~ MIN_END,  # Keep MIN_END if same year and regulation end is prior to end of fishing season
                             !is.na(MIN_END) & year(MIN_END) == YEAR & month(MIN_END) > month(end) ~ end, # Keep end if same year and regulation ends after end of fishing season
                             !is.na(MIN_END) & year(MIN_END) != YEAR ~ end, # Otherwise, use end if year is not the same
                             TRUE ~ MIN_END),
         TRIP_END = case_when(!is.na(TRIP_END) & year(TRIP_END) == YEAR & month(TRIP_END) >= month(start) & month(TRIP_END) < month(end) ~ TRIP_END,  # Keep TRIP_END if same year and regulation end is prior to end of fishing season
                              !is.na(TRIP_END) & year(TRIP_END) != YEAR & month(TRIP_END) >= month(start)& month(TRIP_END) > month(end) ~ end, # Keep end if same year and regulation end is after end of fishing season 
                              !is.na(TRIP_END) & year(TRIP_END) != YEAR ~ end,
                              TRUE ~ TRIP_END),
    # If FMP_START_date exists, use that. Otherwise, use start
         start = case_when(!is.na(FMP_START_date) ~ FMP_START_date,
                           TRUE ~ as.Date(start))) 
    
# Create final commercial table ####
# Indicate column order, adjust column labels, and date formats
mh_commercial_table <- mh_commercial_closure %>%
  select(YEAR, VALUE_CLOSURE, ndays, start, end, ACL, ACL_UNIT, ACL_FR, MIN_SIZE, MIN_START, MIN_END, MIN_FR,
         TRIP_LIMIT, TRIP_START, TRIP_END, TRIP_FR) %>%
  arrange(YEAR, start, MIN_START, TRIP_START) %>%
  mutate(YEAR = as.character(YEAR),
         MIN_START = case_when(!is.na(MIN_START) ~ format(MIN_START, "%e-%b"),
                               TRUE ~ NA_character_),
         MIN_END = case_when(!is.na(MIN_END) ~ format(MIN_END, "%e-%b"),
                             TRUE ~ NA_character_),
         TRIP_START = case_when(!is.na(TRIP_START) ~ format(TRIP_START, "%e-%b"),
                             TRUE ~ NA_character_),
         TRIP_END = case_when(!is.na(TRIP_END) ~ format(TRIP_END, "%e-%b"),
                              TRUE ~ NA_character_),
         start = case_when(!is.na(start) ~ format(start, "%e-%b"),
                           TRUE ~ NA_character_),
         end = case_when(!is.na(end) ~ format(end, "%e-%b"),
                         TRUE ~ NA_character_),
         # Retain values when fishery is open
         ACL = case_when(VALUE_CLOSURE == "Open" ~ ACL,
                         TRUE ~ NA_character_),
         ACL_UNIT = case_when(VALUE_CLOSURE == "Open" ~ ACL_UNIT,
                              TRUE ~ NA_character_),
         ACL_FR = case_when(VALUE_CLOSURE == "Open" ~ ACL_FR,
                            TRUE ~ NA_character_),
         MIN_SIZE = case_when(VALUE_CLOSURE == "Open" & MIN_START >= start & MIN_START <= end ~ MIN_SIZE,
                              TRUE ~ NA_character_),
         MIN_START = case_when(VALUE_CLOSURE == "Open" & MIN_START >= start & MIN_START <= end ~ MIN_START,
                               TRUE ~ NA_character_),
         MIN_END = case_when(VALUE_CLOSURE == "Open" & MIN_START >= start & MIN_START <= end ~ MIN_END,
                             TRUE ~ NA_character_),
         MIN_FR = case_when(VALUE_CLOSURE == "Open" & MIN_START >= start & MIN_START <= end ~ MIN_FR,
                            TRUE ~ NA_character_),
         TRIP_LIMIT = case_when(VALUE_CLOSURE == "Open" & TRIP_START >= start & TRIP_START <= end ~ TRIP_LIMIT,
                                TRUE ~ NA_character_),
         TRIP_START = case_when(VALUE_CLOSURE == "Open" & TRIP_START >= start & TRIP_START <= end~ TRIP_START,
                                TRUE ~ NA_character_),
         TRIP_END = case_when(VALUE_CLOSURE == "Open" & TRIP_START >= start & TRIP_START <= end ~ TRIP_END,
                              TRUE ~ NA_character_),
         TRIP_FR = case_when(VALUE_CLOSURE == "Open" & TRIP_START >= start & TRIP_START <= end ~ TRIP_FR,
                             TRUE ~ NA_character_)) %>%
  rename("Year" = YEAR,
         "ACL Unit" = ACL_UNIT,
         "ACL FR Citation" = ACL_FR,
         "Number of Days" = ndays,
         "Fishing Season" = VALUE_CLOSURE,
         "Season Start Date (first day implemented)" = start,
         "Season End Date (last day effective)" = end,
         "Minimum Size Limit (inches TL)" = MIN_SIZE,
         "Minimum Size Limit Start Date" = MIN_START,
         "Minimum Size Limit End Date" = MIN_END,
         "Minimum Size Limit FR Citation" = MIN_FR,
         "Retention (Trip) Limit (lbs gw)" = TRIP_LIMIT,
         "Retention Limit Start Date" = TRIP_START, 
         "Retention Limit End Date" = TRIP_END,
         "Retention Limit FR Citation" = TRIP_FR)

#### Recreational sector processing ####
# Retain regulations for species and region of interest
# Remove non-detailed regulations and those that were never implemented
# Retain only full-region (ZONE_USE = ALL) and sector-wide (RECREATIONAL/ALL) regulations
mh_recreational <- mh %>%
  filter(COMMON_NAME_USE == spp,
         REGION == region,
         ZONE_USE == "ALL",
         DETAILED == "YES",
         NEVER_IMPLEMENTED %in% c(0, NA),
         SECTOR_USE == "RECREATIONAL",
         SUBSECTOR_USE == "ALL") %>%
  # Format data for table columns
  # FMP_START = month-day when the FMP began
  # YEAR = extract the year value from the START_DATE2 of the regulation
  # Extract the value, start, end, and FR notice information for all of the MANAGEMENT_TYPE_USE of interest
  mutate(FMP_START = case_when(ACTION_TYPE == "ORIGINAL FMP" ~ format(START_DATE2, "%m-%d"),
                               TRUE ~ NA),
         YEAR = year(START_DATE2),
         ACL = case_when(MANAGEMENT_TYPE_USE == "ACL" ~ VALUE,
                         TRUE ~ NA),
         acl_value_prep = case_when(MANAGEMENT_TYPE_USE == "ACL" & VALUE_UNITS == "POUNDS" ~ "lbs",
                                    TRUE ~ NA),
         acl_type_prep = case_when(MANAGEMENT_TYPE_USE == "ACL" & VALUE_TYPE == "GUTTED WEIGHT" ~ "gw",
                                   MANAGEMENT_TYPE_USE == "ACL" & VALUE_TYPE == "ROUND (WHOLE) WEIGHT" ~ "ww",
                                   TRUE ~ NA),
         ACL_UNIT = case_when(MANAGEMENT_TYPE_USE == "ACL" ~ paste(acl_value_prep, " ", acl_type_prep),
                              TRUE ~ NA),
         ACL_FR = case_when(MANAGEMENT_TYPE_USE == "ACL" ~ FR_CITATION,
                            TRUE ~ NA),
         MIN_SIZE = case_when(MANAGEMENT_TYPE_USE == "MINIMUM SIZE LIMIT" ~ VALUE,
                              TRUE ~ NA),
         MIN_START = case_when(MANAGEMENT_TYPE_USE == "MINIMUM SIZE LIMIT" ~ START_DATE2,
                               TRUE ~ NA), 
         MIN_END = case_when(MANAGEMENT_TYPE_USE == "MINIMUM SIZE LIMIT" ~ END_DATE2,
                             TRUE ~ NA),
         MIN_FR = case_when(MANAGEMENT_TYPE_USE == "MINIMUM SIZE LIMIT" ~ FR_CITATION,
                            TRUE ~ NA),
         BAG_LIMIT = case_when(MANAGEMENT_TYPE_USE == "BAG LIMIT" ~ VALUE,
                                TRUE ~ NA),
         BAG_START = case_when(MANAGEMENT_TYPE_USE == "BAG LIMIT" ~ START_DATE2,
                                TRUE ~ NA),
         BAG_END = case_when(MANAGEMENT_TYPE_USE == "BAG LIMIT" ~ END_DATE2,
                              TRUE ~ NA),
         BAG_FR = case_when(MANAGEMENT_TYPE_USE == "BAG LIMIT" ~ FR_CITATION,
                             TRUE ~ NA))

#### Determine Time Range ####
# Define the first year in the time series (min_year), the current year, and the terminal year
# Terminal year definition may need to be adjusted
mh_year_recreational <- mh_recreational %>%
  summarise(min_year = min(YEAR, na.rm = TRUE)) %>%
  mutate(current_year = year(Sys.Date()),
         terminal_year = current_year - 1)

#### Create a complete year sequence ####
# Year sequence should start from first year in time series to terminal year
years <- tibble(YEAR = mh_year_recreational$min_year:mh_year_recreational$terminal_year)

#### Add Closure information ####
# Filter to recreational closures for the entire region 
# Convert CLOSE/OPEN to sentence case
closure_recreational <- closures %>%
  filter(SECTOR_USE == "RECREATIONAL") %>%
  filter(ZONE_USE == "ALL") %>%
  mutate(VALUE_CLOSURE = case_when(VALUE == "CLOSE" ~ "CLOSED",
                                   TRUE ~ VALUE),
         VALUE_CLOSURE = str_to_sentence(VALUE_CLOSURE))

#### Expand regulations across applicable years ####
# Each regulation is expanded across all years between its START and END date information
mh_recreational_expanded <- mh_recreational %>%
  mutate(START_YEAR = year(START_DATE2), 
         END_YEAR = year(END_DATE2)) %>%
  rowwise() %>%
  mutate(YEAR_TEMP = list(START_YEAR:ifelse(is.na(END_YEAR), START_YEAR, END_YEAR))) %>%
  unnest(cols = c(YEAR_TEMP)) %>%  
  ungroup()

#### Ensure all years from min_year to current_year are included in full sequence
mh_recreational_complete <- years %>%
  full_join(mh_recreational_expanded, by = c("YEAR" = "YEAR_TEMP"))

#### Fill missing values for regulations so that each year has regulation values
mh_recreational_complete <- mh_recreational_complete %>%
  arrange(YEAR) %>%
  group_by(YEAR) %>%
  fill(ACL, ACL_UNIT, ACL_FR, MIN_SIZE, MIN_START, MIN_END, MIN_FR,
       BAG_LIMIT, BAG_START, BAG_END, BAG_FR, .direction = "downup") %>%
  ungroup()

#### Join closure data ####
# Combine closure days with regulation records
# If closure not available, default to full open year (Jan 1-Dec 31)
mh_recreational_closure <- mh_recreational_complete %>%
  left_join(closure_recreational, by = "YEAR") %>%
  mutate(VALUE_CLOSURE = ifelse(is.na(VALUE_CLOSURE), "Open", VALUE_CLOSURE),
         start = case_when(is.na(start) ~ as.Date(paste0(YEAR, "-01-01")),
                           TRUE ~ start),
         end = case_when(is.na(end) ~ as.Date(paste0(YEAR, "-12-31")),
                         TRUE ~ end),
         ndays = case_when(is.na(ndays) ~ as.numeric(end - start + 1),
                           TRUE ~ ndays)) 

#### Collapse duplicate rows ####
# Retain multiple rows per YEAR with if there are distinct combinations of VALUE_CLOSURE, start, and end
mh_recreational_closure <- mh_recreational_closure %>%
  group_by(YEAR, VALUE_CLOSURE) %>%
  arrange(start, end) %>%
  mutate(regulation_change = case_when(lag(MIN_SIZE, default = first(MIN_SIZE)) != MIN_SIZE ~ TRUE,
                                       lag(BAG_LIMIT, default = first(BAG_LIMIT)) != BAG_LIMIT ~ TRUE,
                                       TRUE ~ FALSE),
         period_change = case_when(lag(VALUE_CLOSURE, default = first(VALUE_CLOSURE)) != VALUE_CLOSURE ~ TRUE,
                                   lag(start, default = first(start)) != start ~ TRUE,
                                   TRUE ~ FALSE),
         change_flag = regulation_change | period_change) %>%
  filter(row_number() == 1 | change_flag) %>%
  ungroup()


#### Adjust start/end dates ####
# Align regulation start/end dates to closure periods
mh_recreational_closure <- mh_recreational_closure %>%
  # Adjust START dates for each regulation type based on season open and closed dates
  mutate(FMP_START_date = as.Date(FMP_START, format = "%Y-%m-%d"), 
         MIN_START = case_when(!is.na(MIN_START) & year(MIN_START) == YEAR & month(MIN_START) > month(start) ~ MIN_START,  # Keep MIN_START if same year and regulation month is after start of fishing year
                               !is.na(MIN_START) & year(MIN_START) == YEAR & month(MIN_START) < month(start) ~ start, # Keep start if same year and regulation month is before start of fishing year
                               !is.na(MIN_START) & year(MIN_START) != YEAR ~ start, # Otherwise, use start if year is not the same
                               TRUE ~ MIN_START),
         BAG_START = case_when(!is.na(BAG_START) & year(BAG_START) == YEAR & month(BAG_START) > month(start) ~ BAG_START,  # Keep BAG_START if same year and month is after start of fishing year
                                !is.na(BAG_START) & year(BAG_START) == YEAR & month(BAG_START) < month(start) ~ start, # Keep start if same year and regulation month is before start of fishing year
                                !is.na(BAG_START) & year(BAG_START) != YEAR ~ start, # Otherwise, use start if year is not the same
                                TRUE ~ BAG_START),
         # Adjust END dates for each regulation type based on season open and closed dates
         MIN_END = case_when(!is.na(MIN_END) & year(MIN_END) == YEAR & month(MIN_END) < month(end) ~ MIN_END,  # Keep MIN_END if same year and regulation end is prior to end of fishing season
                             !is.na(MIN_END) & year(MIN_END) == YEAR & month(MIN_END) > month(end) ~ end, # Keep end if same year and regulation ends after end of fishing season
                             !is.na(MIN_END) & year(MIN_END) != YEAR ~ end, # Otherwise, use end if year is not the same
                             TRUE ~ MIN_END),
         BAG_END = case_when(!is.na(BAG_END) & year(BAG_END) == YEAR & month(BAG_END) >= month(start) & month(BAG_END) < month(end) ~ BAG_END,  # Keep BAG_END if same year and regulation end is prior to end of fishing season
                              !is.na(BAG_END) & year(BAG_END) != YEAR & month(BAG_END) >= month(start)& month(BAG_END) > month(end) ~ end, # Keep end if same year and regulation end is after end of fishing season 
                              !is.na(BAG_END) & year(BAG_END) != YEAR ~ end,
                              TRUE ~ BAG_END),
         # If FMP_START_date exists, use that. Otherwise, use start
         start = case_when(!is.na(FMP_START_date) ~ FMP_START_date,
                           TRUE ~ as.Date(start))) 


# Create final recreational table ####
# Indicate column order, adjust column labels, and date formats
mh_recreational_table <- mh_recreational_closure %>%
  select(YEAR, VALUE_CLOSURE, ndays, start, end, ACL, ACL_UNIT, ACL_FR, MIN_SIZE, MIN_START, MIN_END, MIN_FR,
         BAG_LIMIT, BAG_START, BAG_END, BAG_FR) %>%
  arrange(YEAR, start, MIN_START, BAG_START) %>%
  mutate(YEAR = as.character(YEAR),
         MIN_START = case_when(!is.na(MIN_START) ~ format(MIN_START, "%e-%b"),
                               TRUE ~ NA_character_),
         MIN_END = case_when(!is.na(MIN_END) ~ format(MIN_END, "%e-%b"),
                             TRUE ~ NA_character_),
         BAG_START = case_when(!is.na(BAG_START) ~ format(BAG_START, "%e-%b"),
                                TRUE ~ NA_character_),
         BAG_END = case_when(!is.na(BAG_END) ~ format(BAG_END, "%e-%b"),
                              TRUE ~ NA_character_),
         start = case_when(!is.na(start) ~ format(start, "%e-%b"),
                           TRUE ~ NA_character_),
         end = case_when(!is.na(end) ~ format(end, "%e-%b"),
                         TRUE ~ NA_character_),
         # Retain values when fishery is open
         ACL = case_when(VALUE_CLOSURE == "Open" ~ ACL,
                         TRUE ~ NA_character_),
         ACL_UNIT = case_when(VALUE_CLOSURE == "Open" ~ ACL_UNIT,
                              TRUE ~ NA_character_),
         ACL_FR = case_when(VALUE_CLOSURE == "Open" ~ ACL_FR,
                            TRUE ~ NA_character_),
         MIN_SIZE = case_when(VALUE_CLOSURE == "Open" & MIN_START >= start & MIN_START <= end ~ MIN_SIZE,
                              TRUE ~ NA_character_),
         MIN_START = case_when(VALUE_CLOSURE == "Open" & MIN_START >= start & MIN_START <= end ~ MIN_START,
                               TRUE ~ NA_character_),
         MIN_END = case_when(VALUE_CLOSURE == "Open" & MIN_START >= start & MIN_START <= end ~ MIN_END,
                             TRUE ~ NA_character_),
         MIN_FR = case_when(VALUE_CLOSURE == "Open" & MIN_START >= start & MIN_START <= end ~ MIN_FR,
                            TRUE ~ NA_character_),
         BAG_LIMIT = case_when(VALUE_CLOSURE == "Open" & BAG_START >= start & BAG_START <= end ~ BAG_LIMIT,
                                TRUE ~ NA_character_),
         BAG_START = case_when(VALUE_CLOSURE == "Open" & BAG_START >= start & BAG_START <= end ~ BAG_START,
                                TRUE ~ NA_character_),
         BAG_END = case_when(VALUE_CLOSURE == "Open" & BAG_START >= start & BAG_START <= end ~ BAG_END,
                              TRUE ~ NA_character_),
         BAG_FR = case_when(VALUE_CLOSURE == "Open" & BAG_START >= start & BAG_START <= end ~ BAG_FR,
                             TRUE ~ NA_character_)) %>%
  rename("Year" = YEAR,
         "ACL Unit" = ACL_UNIT,
         "ACL FR Citation" = ACL_FR,
         "Number of Days" = ndays,
         "Fishing Season" = VALUE_CLOSURE,
         "Season Start Date (first day implemented)" = start,
         "Season End Date (last day effective)" = end,
         "Minimum Size Limit (inches TL)" = MIN_SIZE,
         "Minimum Size Limit Start Date" = MIN_START,
         "Minimum Size Limit End Date" = MIN_END,
         "Minimum Size Limit FR Citation" = MIN_FR,
         "Retention (Bag) Limit (# fish)" = BAG_LIMIT,
         "Retention Limit Start Date" = BAG_START, 
         "Retention Limit End Date" = BAG_END,
         "Retention Limit FR Citation" = BAG_FR)

#### Add commercial and recreational tables to Excel file ####
# Create workbook
wb <- createWorkbook()

# Add worksheets for each table
addWorksheet(wb, "Commercial")
addWorksheet(wb, "Recreational")

# Add the data frames to their respective sheets
writeData(wb, "Commercial", mh_commercial_table)
writeData(wb, "Recreational", mh_recreational_table)

# Save workbook as Excel
saveWorkbook(wb, here("ODM-MH-Analysis_ready", "Management History Comparison Files", "ManagementHistory_outputs", paste0("Management History_DataAnalyst", spp, "_", region, format(Sys.Date(), "%d%b%Y"),".xlsx")),
             overwrite = TRUE)

# Part 2. Management History Report Tables ####
#### TRIP LIMIT ####
trip_tab <- mh %>%
  filter(COMMON_NAME_USE == spp, REGION == region, MANAGEMENT_TYPE_USE == 'TRIP LIMIT', NEVER_IMPLEMENTED == 0, REG_REMOVED == 0) %>%
  arrange(FR_CITATION) %>%
  ungroup() %>%
  mutate(START_DATE = format(START_DATE2, "%m/%d/%Y"),
         END_DATE = format(END_DATE2, "%m/%d/%Y"),
         SECTOR = str_to_title(paste0(SECTOR_USE, " ", "-", " ", SUBSECTOR_USE)),
         ZONE = str_to_title(paste0(REGION, " ", "-", " ", ZONE_USE)),
         VALUE  = paste0(VALUE, " ", tolower(VALUE_UNITS), " ", tolower(VALUE_TYPE), " ", tolower(VALUE_RATE)),
         ACTION = case_when(is.na(AMENDMENT_NUMBER) & !is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE)),
                            is.na(AMENDMENT_NUMBER) &  is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION)),
                            !is.na(AMENDMENT_NUMBER) & is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", AMENDMENT_NUMBER)),
                            !is.na(AMENDMENT_NUMBER) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE, " ", AMENDMENT_NUMBER))),
         START_YEAR = format(START_DATE2, "%Y"),
         COMMON_NAME_USE      = str_to_title(paste0(COMMON_NAME_USE)),
         MANAGEMENT_TYPE_USE  = str_to_title(paste0(MANAGEMENT_TYPE_USE)),
          Flag = case_when(FLAG == "YES" ~ "Yes",
                           FLAG == "NO" ~ "No",
                           TRUE ~ NA_character_))

trip_tab_export <- trip_tab %>%
  select(COMMON_NAME_USE, START_YEAR, START_DATE, END_DATE, SECTOR, VALUE, ZONE, 
         FR_CITATION, ACTION, FR_URL, Flag) %>%
  rename(`Species Affected` = COMMON_NAME_USE,
         `First Year in Effect`= START_YEAR,
         `Effective Date`= START_DATE,
         `End Date` = END_DATE,
          Fishery = SECTOR,
         `Trip Limit`= VALUE,
         `Region Affected`= ZONE,
         `FR Reference(s)`= FR_CITATION,
         `Amendment Number or Rule Type` = ACTION,
         `Flag` = Flag,
         `FR URL` = FR_URL) %>%
  arrange(Fishery, `First Year in Effect`)


trip_tab2 <- trip_tab %>%
  select(COMMON_NAME_USE, START_YEAR, START_DATE, END_DATE, SECTOR, VALUE, ZONE, 
         FR_CITATION, ACTION, FR_URL, Flag) %>%
  do(tab = flextable(.) %>%
       set_header_labels(MANAGEMENT_TYPE_USE = "Management Type",
                         COMMON_NAME_USE  = "Species Affected",
                         START_YEAR = "First Year in Effect",
                         START_DATE = "Effective Date",
                         END_DATE = "End Date",
                         SECTOR = "Fishery",
                         VALUE  = "Trip Limit",
                         ZONE = "Region Affected",
                         FR_CITATION = "FR Reference(s)",
                         ACTION = "Amendment Number or Rule Type",
                         Flag = "Flag",
                         FR_URL = "FR URL") %>%
       merge_v(j = 1, part = "body") %>%
       merge_v(j = 2, part = "body") %>%
       merge_v(j = 3, part = "body") %>%
       merge_v(j = 4, part = "body") %>%
       theme_box() %>%
       hline_top(part = "header", border = fp_border(color = "black", width = 2)) %>%
       hline_bottom(part = "header", border = fp_border(color = "black", width = 2)) %>%
       fontsize(part = "all", size = 12) %>%
       font(part = "all", fontname = "Times New Roman") %>%
       align(part = "all", align = "center"))

#### BAG LIMIT ####
bag_tab <- mh %>%
  filter(COMMON_NAME_USE == spp, REGION == region, MANAGEMENT_TYPE_USE == 'BAG LIMIT', NEVER_IMPLEMENTED == 0, REG_REMOVED == 0) %>%
  arrange(FR_CITATION) %>%
  ungroup() %>%
  mutate(START_DATE = format(START_DATE2, "%m/%d/%Y"),
         END_DATE = format(END_DATE2, "%m/%d/%Y"),
         SECTOR = str_to_title(paste0(SECTOR_USE, " ", "-", " ", SUBSECTOR_USE)),
         ZONE = str_to_title(paste0(REGION, " ", "-", " ", ZONE_USE)),
         VALUE  = paste0(VALUE, " ", tolower(VALUE_RATE)),
         ACTION = case_when(is.na(AMENDMENT_NUMBER) & !is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE)),
                            is.na(AMENDMENT_NUMBER) &  is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION)),
                            !is.na(AMENDMENT_NUMBER) & is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", AMENDMENT_NUMBER)),
                            !is.na(AMENDMENT_NUMBER) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE, " ", AMENDMENT_NUMBER))),
         START_YEAR = format(START_DATE2, "%Y"),
         COMMON_NAME_USE      = str_to_title(paste0(COMMON_NAME_USE)),
         MANAGEMENT_TYPE_USE  = str_to_title(paste0(MANAGEMENT_TYPE_USE)),
         Flag = case_when(FLAG == "YES" ~ "Yes",
                          FLAG == "NO" ~ "No",
                          TRUE ~ NA_character_))

bag_tab_export <- bag_tab %>%
  select(COMMON_NAME_USE, START_YEAR, START_DATE, END_DATE, SECTOR, VALUE, ZONE,
          FR_CITATION, ACTION, FR_URL, Flag) %>%
  rename(`Species Affected` = COMMON_NAME_USE,
         `First Year in Effect`= START_YEAR,
         `Effective Date`= START_DATE,
         `End Date` = END_DATE,
         Fishery = SECTOR,
         `Bag Limit`= VALUE,
         `Region Affected`= ZONE,
         `FR Reference(s)`= FR_CITATION,
         `Amendment Number or Rule Type` = ACTION,
         `Flag` = Flag,
         `FR URL` = FR_URL) %>%
  arrange(Fishery, `First Year in Effect`)


bag_tab2 <- bag_tab %>%
  select(COMMON_NAME_USE, START_YEAR, START_DATE, END_DATE, SECTOR, VALUE, ZONE,
         FR_CITATION, ACTION, FR_URL, Flag) %>%
  do(tab = flextable(.) %>%
       set_header_labels(MANAGEMENT_TYPE_USE = "Management Type",
                         COMMON_NAME_USE  = "Species Affected",
                         START_YEAR = "First Year in Effect",
                         START_DATE = "Effective Date",
                         END_DATE = "End Date",
                         SECTOR = "Fishery",
                         VALUE  = "Bag Limit",
                         ZONE = "Region Affected",
                         FR_CITATION = "FR Reference(s)",
                         ACTION = "Amendment Number or Rule Type",
                         Flag = "Flag",
                         FR_URL = "FR URL") %>%
       merge_v(j = 1, part = "body") %>%
       merge_v(j = 2, part = "body") %>%
       merge_v(j = 3, part = "body") %>%
       merge_v(j = 4, part = "body") %>%
       theme_box() %>%
       hline_top(part = "header", border = fp_border(color = "black", width = 2)) %>%
       hline_bottom(part = "header", border = fp_border(color = "black", width = 2)) %>%
       fontsize(part = "all", size = 12) %>%
       font(part = "all", fontname = "Times New Roman") %>%
       align(part = "all", align = "center"))

#### MAXIMUM/MINIMUM SIZE LIMIT ####
size_tab <- mh %>%
  filter(COMMON_NAME_USE == spp, REGION == region, MANAGEMENT_CATEGORY == 'SELECTIVITY CONTROLS', NEVER_IMPLEMENTED == 0, REG_REMOVED == 0) %>%
  ungroup() %>%
  mutate(START_DATE = format(START_DATE2, "%m/%d/%Y"),
         END_DATE   = format(END_DATE2, "%m/%d/%Y"),
         SECTOR = str_to_title(paste0(SECTOR_USE, " ", "-", " ", SUBSECTOR_USE)),
         ZONE = str_to_title(paste0(REGION, " ", "-", " ", ZONE_USE)),
         VALUE  = paste0(VALUE, " ", tolower(VALUE_UNITS), " ", tolower(VALUE_TYPE)),
         ACTION = case_when(is.na(AMENDMENT_NUMBER) & !is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE)),
                            is.na(AMENDMENT_NUMBER) &  is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION)),
                            !is.na(AMENDMENT_NUMBER) & is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", AMENDMENT_NUMBER)),
                            !is.na(AMENDMENT_NUMBER) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE, " ", AMENDMENT_NUMBER))),
    START_YEAR = format(START_DATE2, "%Y"),
    VALUE_TYPE = case_when(MANAGEMENT_TYPE_USE == 'MINIMUM SIZE LIMIT' ~ paste0("Minimum ", str_to_title(VALUE_TYPE)),
                           MANAGEMENT_TYPE_USE == 'MAXIMUM SIZE LIMIT' ~ paste0("Maximum ", str_to_title(VALUE_TYPE))),
    COMMON_NAME_USE = str_to_title(paste0(COMMON_NAME_USE)),
    MANAGEMENT_TYPE_USE = str_to_title(paste0(MANAGEMENT_TYPE_USE)),
    Flag = case_when(FLAG == "YES" ~ "Yes",
                     FLAG == "NO" ~ "No",
                     TRUE ~ NA_character_))

size_tab_export <- size_tab %>%
  select(COMMON_NAME_USE, START_YEAR, START_DATE, END_DATE, SECTOR, VALUE, ZONE, FR_CITATION, ACTION, FR_URL, Flag) %>%
  arrange(SECTOR, START_YEAR) %>%
  rename(`Species Affected`= COMMON_NAME_USE,
         `First Year in Effect` = START_YEAR,
         `Effective Date` = START_DATE,
         `End Date` = END_DATE,
          Fishery = SECTOR,
         `Size Limit` = VALUE,
         `Region Affected` = ZONE,
         `FR Reference(s)` = FR_CITATION,
         `Amendment Number or Rule Type` = ACTION,
          Flag = Flag,
         `FR URL` = FR_URL) %>%
  arrange(Fishery, `First Year in Effect`)

size_tab2 <- size_tab %>%
  select(COMMON_NAME_USE, START_YEAR, START_DATE, END_DATE, SECTOR, VALUE, ZONE, FR_CITATION, ACTION, FR_URL, Flag) %>%
  arrange(SECTOR, START_YEAR) %>%
  do(tab = flextable(.) %>%
       set_header_labels(MANAGEMENT_TYPE_USE = "Management Type",
                         COMMON_NAME_USE = "Species Affected",
                         START_YEAR = "Start Year",
                         START_DATE  = "Effective Date",
                         END_DATE = "End Date",
                         SECTOR  = "Fishery",
                         VALUE = "Size Limit",
                         ZONE = "Region Affected",
                         FR_CITATION = "FR Reference(s)",
                         ACTION = "Amendment Number or Rule Type",
                         Flag  = "Flag",
                         FR_URL = "FR URL") %>%
       merge_v(j = 1, part = "body") %>%
       merge_v(j = 2, part = "body") %>%
       merge_v(j = 3, part = "body") %>%
       merge_v(j = 4, part = "body") %>%
       theme_box() %>%
       hline_top(part = "header", border = fp_border(color = "black", width = 2)) %>%
       hline_bottom(part = "header", border = fp_border(color = "black", width = 2)) %>%
       fontsize(part = "all", size = 12) %>%
       font(part = "all", fontname = "Times New Roman") %>%
       align(part = "all", align = "center"))


#### ACL ####
ACL_table <- mh %>%
  filter(COMMON_NAME_USE == spp, REGION == region, MANAGEMENT_TYPE_USE == 'ACL') %>%
  ungroup() %>%
  mutate(START_DATE = format(START_DATE2, "%m/%d/%Y"),
         END_DATE   = format(END_DATE2, "%m/%d/%Y"),
         SECTOR = str_to_title(paste0(SECTOR_USE, " ", "-", " ", SUBSECTOR_USE)),
         ZONE = str_to_title(paste0(REGION, " ", "-", " ", ZONE_USE)),
         VALUE  = paste0(VALUE, " ", tolower(VALUE_UNITS), " ", tolower(VALUE_TYPE)),
         ACTION = case_when(is.na(AMENDMENT_NUMBER) & !is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE)),
                            is.na(AMENDMENT_NUMBER) &  is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION)),
                            !is.na(AMENDMENT_NUMBER) & is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", AMENDMENT_NUMBER)),
                            !is.na(AMENDMENT_NUMBER) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE, " ", AMENDMENT_NUMBER))),
         START_YEAR = format(START_DATE2, "%Y"),
         COMMON_NAME_USE      = str_to_title(paste0(COMMON_NAME_USE)),
         MANAGEMENT_TYPE_USE  = str_to_title(paste0(MANAGEMENT_TYPE_USE)),
         Flag = case_when(FLAG == "YES" ~ "Yes",
                          FLAG == "NO" ~ "No",
                          TRUE ~ NA_character_))

ACL_table_export <- ACL_table %>%
  select(COMMON_NAME_USE, START_YEAR, SECTOR, VALUE, ZONE, FR_CITATION, FR_URL, ACTION, Flag) %>%
  distinct() %>%
  arrange(SECTOR, START_YEAR) %>%
  rename(`Species Affected` = COMMON_NAME_USE,
         `First Year in Effect` = START_YEAR,
          Fishery = SECTOR,
         `ACL` = VALUE,
         `Region Affected`= ZONE,
         `FR Reference(s)`= FR_CITATION,
         `Amendment Number or Rule Type` = ACTION,
         `Flag` = Flag,
         `FR URL` = FR_URL)

ACL_table1 <- ACL_table %>%
  select(COMMON_NAME_USE, START_YEAR, SECTOR, VALUE, ZONE, FR_CITATION, FR_URL, ACTION, Flag) %>%
  distinct() %>%
  arrange(SECTOR, START_YEAR) %>%
  do(tab = flextable(.) %>%
       set_header_labels(COMMON_NAME_USE = "Species Affected",
                         START_YEAR = "First YEar in Effect",
                         START_DATE = "Effective Date",
                         END_DATE = "End Date",
                         SECTOR = "Fishery",
                         VALUE = "ACL",
                         ZONE = "Region Affected",
                         FR_CITATION  = "FR Reference(s)",
                         ACTION = "Amendment Number or Rule Type",
                         Flag = "Flag",
                         FR_URL = "FR URL") %>%
       merge_v(j = 1, part = "body") %>%
       merge_v(j = 2, part = "body") %>%
       merge_v(j = 3, part = "body") %>%
       merge_v(j = 4, part = "body") %>%
       merge_v(j = 5, part = "body") %>%
       merge_v(j = 6, part = "body") %>%
       theme_box() %>%
       hline_top(part = "header", border = fp_border(color = "black", width = 2)) %>%
       hline_bottom(part = "header", border = fp_border(color = "black", width = 2)) %>%
       fontsize(part = "all", size = 12) %>%
       font(part = "all", fontname = "Times New Roman") %>%
       align(part = "all", align = "center"))


# TEMPORAL CLOSURE ####
clos_tab <- mh %>%
  filter(COMMON_NAME_USE == spp, REGION == region, MANAGEMENT_TYPE_USE == 'CLOSURE', NEVER_IMPLEMENTED == 0, REG_REMOVED == 0, ZONE_USE == "ALL") %>%
  group_by(VALUE, ZONE_USE) %>%
  ungroup() %>%
  mutate(START_DATE = format(START_DATE2, "%m/%d/%Y"),
         END_DATE = format(END_DATE2, "%m/%d/%Y"),
         SECTOR = str_to_title(paste0(SECTOR_USE, " ", "-", " ", SUBSECTOR_USE)),
         ZONE = str_to_title(paste0(REGION, " ", "-", " ", ZONE_USE)),
         VALUE  = paste0(str_to_title(VALUE)),
         ACTION = case_when(is.na(AMENDMENT_NUMBER) & !is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE)),
                            is.na(AMENDMENT_NUMBER) &  is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION)),
                            !is.na(AMENDMENT_NUMBER) & is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", AMENDMENT_NUMBER)),
                            !is.na(AMENDMENT_NUMBER)                       ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE, " ", AMENDMENT_NUMBER))),
        START_YEAR = format(START_DATE2, "%Y"),
        COMMON_NAME_USE      = str_to_title(paste0(COMMON_NAME_USE)),
        MANAGEMENT_TYPE_USE  = str_to_title(paste0(MANAGEMENT_TYPE_USE)),
        MANAGEMENT_STATUS_USE = str_to_sentence(paste0(MANAGEMENT_STATUS_USE)),
        Flag = case_when(FLAG == "YES" ~ "Yes",
                                      FLAG == "NO" ~ "No",
                                      TRUE ~ NA_character_),
        FIRST_DAY = paste0(START_MONTH, "-", START_DAY_USE),
        LAST_DAY = paste0(END_MONTH_USE, "-", END_DAY_USE))

clos_tab_export <- clos_tab %>%
  select(COMMON_NAME_USE, START_YEAR, START_DATE, END_DATE, START_TIME_USE, FIRST_DAY, LAST_DAY, END_TIME_USE, SECTOR, VALUE, MANAGEMENT_STATUS_USE, ZONE, FR_CITATION, ACTION, FR_URL, Flag) %>%
  distinct() %>%
  arrange(SECTOR, ZONE, START_YEAR, START_DATE) %>%
  rename(`Species Affected` = COMMON_NAME_USE,
         `First Year in Effect` = START_YEAR,
         `Effective Date` = START_DATE,
         `End Date` = END_DATE,
         `Start Time` = START_TIME_USE,
         `First Day Closed` = FIRST_DAY,
         `Last Day Closed` = LAST_DAY,
         `END_TIME` = END_TIME_USE,
          Fishery = SECTOR,
          Status = VALUE,
          `Closure Type` = MANAGEMENT_STATUS_USE,
          `Region Affected` = ZONE,
          `FR Reference(s)` = FR_CITATION,
          `Amendment Number or Rule Type` = ACTION,
          Flag = Flag,
          `FR URL` = FR_URL)

clos_tab1 <- clos_tab %>%
  select(COMMON_NAME_USE, START_YEAR, START_DATE, END_DATE, START_TIME_USE, FIRST_DAY, LAST_DAY, END_TIME_USE, SECTOR, VALUE, MANAGEMENT_STATUS_USE, ZONE, FR_CITATION, ACTION, FR_URL, Flag) %>%
  distinct() %>%
  arrange(SECTOR, ZONE, START_DATE) %>%
  do(tab = flextable(.) %>%
       set_header_labels(COMMON_NAME_USE = "Species Affected",
                         START_YEAR = "First Year in Effect",
                         START_DATE = "Effective Date",
                         END_DATE = "End Date",
                         START_TIME_USE = "Start Time",
                         FIRST_DAY = "First Day Closed",
                         LAST_DAY = "Last Day Closed",
                         END_TIME_USE = "End Time",
                         SECTOR = "Fishery",
                         VALUE = "Status",
                         MANAGEMENT_STATUS_USE = "Closure Type",
                         ZONE = "Region Affected",
                         FR_CITATION  = "FR Reference(s)",
                         ACTION  = "Amendment Number or Rule Type",
                         Flag  = "Flag",
                         FR_URL = "FR URL") %>%
       merge_v(j = 1, part = "body") %>%
       merge_v(j = 2, part = "body") %>%
       merge_v(j = 3, part = "body") %>%
       merge_v(j = 4, part = "body") %>%
       merge_v(j = 5, part = "body") %>%
       merge_v(j = 6, part = "body") %>%
       theme_box() %>%
       hline_top(part = "header", border = fp_border(color = "black", width = 2)) %>%
       hline_bottom(part = "header", border = fp_border(color = "black", width = 2)) %>%
       fontsize(part = "all", size = 12) %>%
       font(part = "all", fontname = "Times New Roman") %>%
       align(part = "all", align = "center"))

# SPATIAL CLOSURE ####
spatclos_tab <- mh %>%
  filter(COMMON_NAME_USE == spp, REGION == region, MANAGEMENT_TYPE_USE == 'CLOSURE', NEVER_IMPLEMENTED == 0, REG_REMOVED == 0, ZONE_USE != "ALL") %>%
  group_by(VALUE, ZONE_USE) %>%
  ungroup() %>%
  mutate(START_DATE = format(START_DATE2, "%m/%d/%Y"),
         END_DATE   = format(END_DATE2, "%m/%d/%Y"),
         SECTOR_USE = str_to_title(paste0(SECTOR_USE)),
         SUBSECTOR_USE = str_to_title(paste0(SUBSECTOR_USE)),
         ZONE_USE = str_to_title(paste0(ZONE_USE)),
         VALUE  = paste0(VALUE, " ", tolower(VALUE_UNITS)),
         ACTION = case_when(is.na(AMENDMENT_NUMBER) & !is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE)),
                            is.na(AMENDMENT_NUMBER) &  is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION)),
                            !is.na(AMENDMENT_NUMBER) & is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", AMENDMENT_NUMBER)),
                            !is.na(AMENDMENT_NUMBER)                       ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE, " ", AMENDMENT_NUMBER))),
         START_YEAR = format(START_DATE2, "%Y"),
         COMMON_NAME_USE      = str_to_title(paste0(COMMON_NAME_USE)),
         MANAGEMENT_TYPE_USE  = str_to_title(paste0(MANAGEMENT_TYPE_USE)),
         Flag = case_when(FLAG == "YES" ~ "Yes",
                          FLAG == "NO" ~ "No",
                          TRUE ~ NA_character_),
         FIRST_DAY = paste0(START_MONTH, "-", START_DAY_USE),
         LAST_DAY = paste0(END_MONTH_USE, "-", END_DAY_USE))

spatclos_tab_export <- spatclos_tab %>%
  select(COMMON_NAME_USE, ZONE_USE,START_YEAR, START_DATE, END_DATE, SECTOR_USE, FIRST_DAY, LAST_DAY, SUBSECTOR_USE, FR_CITATION, ACTION, FR_URL, Flag) %>%
  distinct() %>%
  arrange(SECTOR_USE, SUBSECTOR_USE, ZONE_USE, START_DATE) %>%
  rename(`Species Affected` = COMMON_NAME_USE,
         `Area` = ZONE_USE,
         `First Year in Effect` = START_YEAR,
         `Effective Date` = START_DATE,
         `End Date` = END_DATE,
          `Fishery` = SECTOR_USE,
         `First Day Closed` = FIRST_DAY,
         `Last Day Closed` = LAST_DAY,
         `Fishing Restriction in Area` = SUBSECTOR_USE,
         `FR Reference(s)` = FR_CITATION,
         `Amendment Number or Rule Type` = ACTION,
          Flag = Flag,
         `FR URL` = FR_URL)

spatclos_tab1 <- spatclos_tab %>%
  select(COMMON_NAME_USE, ZONE_USE,START_YEAR, START_DATE, END_DATE, SECTOR_USE, FIRST_DAY, LAST_DAY, SUBSECTOR_USE, FR_CITATION, ACTION, FR_URL, Flag) %>%
  distinct() %>%
  arrange(SECTOR_USE, SUBSECTOR_USE, ZONE_USE, START_DATE) %>%
  do(tab = flextable(.) %>%
       set_header_labels(COMMON_NAME_USE = "Species Affected",
                         ZONE_USE = "Area",
                         START_YEAR = "First Year in Effect",
                         START_DATE = "Effective Date",
                         END_DATE = "End Date",
                         SECTOR_USE = "Fishery",
                         FIRST_DAY = "First Day Closed",
                         LAST_DAY = "Last Day Closed",
                         SUBSECTOR_USE = "Fishing Restriction in Area",
                         FR_CITATION  = "FR Reference(s)",
                         ACTION  = "Amendment Number or Rule Type",
                         Flag  = "Flag",
                         FR_URL = "FR URL") %>%
       merge_v(j = 1, part = "body") %>%
       merge_v(j = 2, part = "body") %>%
       merge_v(j = 3, part = "body") %>%
       merge_v(j = 4, part = "body") %>%
       merge_v(j = 5, part = "body") %>%
       merge_v(j = 6, part = "body") %>%
       theme_box() %>%
       hline_top(part = "header", border = fp_border(color = "black", width = 2)) %>%
       hline_bottom(part = "header", border = fp_border(color = "black", width = 2)) %>%
       fontsize(part = "all", size = 12) %>%
       font(part = "all", fontname = "Times New Roman") %>%
       align(part = "all", align = "center"))

#### GEAR REQUIREMENTS ####
gear_tab <- mh %>%
  filter(COMMON_NAME_USE == spp, REGION == region,
         MANAGEMENT_TYPE_USE %in% c('ANCHORING RESTRICTION', 'BRD REQUIREMENT', 'DEHOOKING DEVICE', 'DESCENDING DEVICE',
                               'GILLNET SPECIFICATIONS', 'GOLDEN CRAB TRAP SPECIFICATIONS', 'HARVEST METHOD', 'HOOK SIZE/TYPE/#',
                               'MAXIMUM GEAR', 'NON-OFFSET, NON-STAINLESS STEEL CIRCLE HOOKS', 'NON-STAINLESS STEEL CIRCLE HOOKS',
                               'NON-STAINLESS STEEL HOOKS', 'PERMITTED AND AUTHORIZED GEAR', 'PROHIBITED GEAR', 'SEA BASS POT/TRAP SPECIFICATIONS',
                               'TRAP SPECIFICATIONS', 'TRAWL SPECIFICATIONS', 'TURTLE RELEASE GEAR', 'VENTING TOOL'),
         NEVER_IMPLEMENTED == 0, REG_REMOVED == 0) %>%
  ungroup() %>%
  mutate(START_DATE = format(START_DATE2, "%m/%d/%Y"),
         END_DATE = format(END_DATE2, "%m/%d/%Y"),
         SECTOR = str_to_title(paste0(SECTOR_USE, " ", "-", " ", SUBSECTOR_USE)),
         ZONE = str_to_title(paste0(REGION, " ", "-", " ", ZONE_USE)),
         ACTION = case_when(is.na(AMENDMENT_NUMBER) & !is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE)),
                            is.na(AMENDMENT_NUMBER) &  is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION)),
                            !is.na(AMENDMENT_NUMBER) & is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", AMENDMENT_NUMBER)),
                            !is.na(AMENDMENT_NUMBER) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE, " ", AMENDMENT_NUMBER))),
         START_YEAR = format(START_DATE2, "%Y"),
         COMMON_NAME_USE = str_to_title(paste0(COMMON_NAME_USE)),
         MANAGEMENT_TYPE_USE  = str_to_title(paste0(MANAGEMENT_TYPE_USE)),
         Flag = case_when(FLAG == "YES" ~ "Yes",
                          FLAG == "NO" ~ "No",
                          TRUE ~ NA_character_))

gear_tab_export <- gear_tab %>%
  select(COMMON_NAME_USE, START_YEAR, START_DATE, END_DATE, SECTOR, MANAGEMENT_TYPE_USE, ZONE, FR_CITATION, ACTION, FR_URL, Flag) %>%
  arrange(ZONE, SECTOR, START_YEAR) %>%
  rename(`Species Affected`= COMMON_NAME_USE,
         `First Year in Effect` = START_YEAR,
         `Effective Date` = START_DATE,
         `End Date` = END_DATE,
          Gear = SECTOR,
         `Regulation Type` = MANAGEMENT_TYPE_USE,
          `Region Affected` = ZONE,
         `FR Reference(s)` = FR_CITATION,
         `Amendment Number or Rule Type` = ACTION,
          Flag = Flag,
         `FR URL` = FR_URL)

gear_tab1 <- gear_tab %>%
  select(COMMON_NAME_USE, START_YEAR, START_DATE, END_DATE, SECTOR, MANAGEMENT_TYPE_USE, ZONE, FR_CITATION, ACTION, FR_URL, Flag) %>%
  arrange(ZONE, SECTOR, START_YEAR) %>%
  do(tab = flextable(.) %>%
       set_header_labels(COMMON_NAME_USE = "Species Affected",
                         START_YEAR = "First Year in Effect",
                         START_DATE = "Effective Date",
                         END_DATE = "End Date",
                         SECTOR = "Gear",
                         MANAGEMENT_TYPE_USE = "Regulation Type",
                         ZONE = "Region Affected",
                         FR_CITATION = "FR Reference(s)",
                         ACTION = "Amendment Number or Rule Type",
                         Flag = "Flag",
                         FR_URL = "FR URL") %>%
       merge_v(j = 1, part = "body") %>%
       merge_v(j = 2, part = "body") %>%
       merge_v(j = 3, part = "body") %>%
       merge_v(j = 4, part = "body") %>%
       theme_box() %>%
       hline_top(part = "header", border = fp_border(color = "black", width = 2)) %>%
       hline_bottom(part = "header", border = fp_border(color = "black", width = 2)) %>%
       fontsize(part = "all", size = 12) %>%
       font(part = "all", fontname = "Times New Roman") %>%
       align(part = "all", align = "center"))


# Export each table as sheet in Excel workbook
wb <- createWorkbook()
addWorksheet(wb, "Trip Limit")
addWorksheet(wb, "Bag Limit")
addWorksheet(wb, "Size Limit")
addWorksheet(wb, "ACL")
addWorksheet(wb, "Closure")
addWorksheet(wb, "Spatial Restrictions")
addWorksheet(wb, "Gear Restrictions")

writeData(wb, "Trip Limit", trip_tab_export)
writeData(wb, "Bag Limit", bag_tab_export)
writeData(wb, "Size Limit", size_tab_export)
writeData(wb, "ACL", ACL_table_export)
writeData(wb, "Closure", clos_tab_export)
writeData(wb, "Spatial Restrictions", spatclos_tab_export)
writeData(wb, "Gear Restrictions", gear_tab_export)

saveWorkbook(wb, here("ODM-MH-Analysis_ready", "Management History Comparison Files", "ManagementHistory_outputs", paste0("Management History_Report_", spp, "_", region, format(Sys.Date(), "%d%b%Y"),".xlsx")),
overwrite = TRUE)

# Export each table into a Word Document
doc_path <- here("ODM-MH-Analysis_ready", "Management History Comparison Files", "ManagementHistory_outputs", paste0("ManagementHistory_Report_", str_replace_all(spp, "[^A-Za-z0-9]+", "_"),
                                    "_", str_replace_all(region, "[^A-Za-z0-9]+", "_"), "_",
                                    format(Sys.Date(), "%d%b%Y"), ".docx"))

doc <- read_docx()

add_section <- function(doc, title_text, ft_obj) {
  doc %>%
    body_add_par(value = title_text, style = "heading 1") %>%
    body_add_par(value = "", style = "Normal") %>%
    body_add_flextable(ft_obj) %>%
    body_add_par(value = "", style = "Normal") %>%
    body_add_break()
}

doc <- add_section(doc, "Trip Limit", trip_tab2$tab[[1]])
doc <- add_section(doc, "Size Limit", size_tab2$tab[[1]])
doc <- add_section(doc, "ACL", ACL_table1$tab[[1]])
doc <- add_section(doc, "Closure", clos_tab1$tab[[1]])
doc <- add_section(doc, "Gear Requirements", gear_tab1$tab[[1]])

doc <- body_remove(doc)

print(doc, target = doc_path)

