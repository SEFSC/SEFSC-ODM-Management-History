# MH Closure processing

#Load packages ####
librarian::shelf(here, tidyverse, lubridate, dplyr, tidyr, neatRanges, splitstackshape)

# Read in MH Data log ####
mh_data_log <- readRDS(here("ODM-MH-Data_log", "data", "results", "MH_DL_2023Nov13.RDS"))

# Select species and region ####
spp <- 'SNAPPER, YELLOWTAIL'
region <- 'CARIBBEAN'

# Seasonal Closures ####
# Filter dataset to seasonal closure regulations for the specified species and region
Seasonal_closures <- mh_data_log %>%
  filter(MANAGEMENT_TYPE_USE == 'CLOSURE', MANAGEMENT_STATUS_USE == 'SEASONAL', NEVER_IMPLEMENTED == 0, REG_REMOVED == 0, COMMON_NAME_USE == spp, REGION == region)

# Create date_sequence to expand dates between EFFECTIVE_DATE and END_DATE2 
expand_seasonal <- Seasonal_closures %>%
  mutate(date_sequence = map2(EFFECTIVE_DATE, END_DATE2, seq, by = "days")) %>%
  unnest(date_sequence)

# Add START_YEAR_expand and END_YEAR_expand to extract the year from the date_sequence field
expand_seasonal_year <- expand_seasonal %>%
  rowwise() %>%
  mutate(START_YEAR_expand = year(date_sequence),
         END_YEAR_expand = year(date_sequence))

# Create START_DATE_EXPAND and END_DATE_EXPAND using start/end day and month information and the expanded year
expand_seasonal_date <- expand_seasonal_year %>%
  mutate(START_DATE_EXPAND = as.Date(paste(START_YEAR_expand, START_MONTH, START_DAY, sep = "-")),
         END_DATE_EXPAND = as.Date(paste(END_YEAR_expand, END_MONTH, END_DAY, sep = "-")))

# Remove records where date_sequence is prior to the START_DATE_EXPAND and date_sequence is after the END_DATE_EXPAND
remove_seasonal_date <- expand_seasonal_date %>%
  filter(date_sequence >= START_DATE_EXPAND,
         END_DATE_EXPAND >= date_sequence)

# Create CLOSE_OPEN field to indicate that expanded records are related to Closures or Openings
add_closure_value_seasonal <- remove_seasonal_date %>%
  mutate(CLOSE_OPEN = VALUE)

# Weekly Closures ####
# Filter dataset to Weekly Recurring regulations
Weekly_closures <- mh_data_log %>%
  filter(MANAGEMENT_TYPE_USE == 'CLOSURE', MANAGEMENT_STATUS_USE == 'WEEKLY RECURRING', NEVER_IMPLEMENTED == 0, REG_REMOVED == 0, COMMON_NAME_USE == spp, REGION == region)

# Expand dates between EFFECTIVE_DATE and END_DATE2
expand_weekly <- Weekly_closures %>%
  mutate(date_sequence = map2(EFFECTIVE_DATE, END_DATE2, seq, by = "days")) %>%
  unnest(date_sequence)

# Add START_YEAR_expand and END_YEAR_expand to extract the year from the date_sequence field
expand_weekly_year <- expand_weekly %>%
  rowwise() %>%
  mutate(START_YEAR_expand = year(date_sequence),
         END_YEAR_expand = year(date_sequence))

# Create Expand_day_of_week to indicate the day of the week for the closure/reopening
expand_weekly_day_of_week <- expand_weekly_year %>%
  mutate(Expand_day_of_week = wday(date_sequence, label = TRUE))

# Create START_DAY_OF_WEEK_EXPAND and END_DAY_OF_WEEK_EXPAND to format the days of the week to align with Expand_day_of_week
expand_weekly_day_format <- expand_weekly_day_of_week %>%
  mutate(START_DAY_OF_WEEK_EXPAND = case_when(
    START_DAY_OF_WEEK_USE == "MONDAY" ~ "Mon",
    START_DAY_OF_WEEK_USE == "TUESDAY" ~ "Tue",
    START_DAY_OF_WEEK_USE == "WEDNESDAY" ~ "Wed",
    START_DAY_OF_WEEK_USE == "THURSDAY" ~ "Thu",
    START_DAY_OF_WEEK_USE == "FRIDAY" ~ "Fri",
    START_DAY_OF_WEEK_USE == "SATURDAY" ~ "Sat",
    START_DAY_OF_WEEK_USE == "SUNDAY" ~ "Sun",
    TRUE ~ START_DAY_OF_WEEK_USE),
    END_DAY_OF_WEEK_EXPAND = case_when(
      END_DAY_OF_WEEK_USE == "MONDAY" ~ "Mon",
      END_DAY_OF_WEEK_USE == "TUESDAY" ~ "Tue",
      END_DAY_OF_WEEK_USE == "WEDNESDAY" ~ "Wed",
      END_DAY_OF_WEEK_USE == "THURSDAY" ~ "Thu",
      END_DAY_OF_WEEK_USE == "FRIDAY" ~ "Fri",
      END_DAY_OF_WEEK_USE == "SATURDAY" ~ "Sat",
      END_DAY_OF_WEEK_USE == "SUNDAY" ~ "Sun",
      TRUE ~ END_DAY_OF_WEEK_USE))

# Keep any records where Expand_day_of_week is between START_DAY_OF_WEEK_EXPAND and END_DAY_OF_WEEK_EXPAND
expand_weekly_day_keep <- expand_weekly_day_format %>%
  filter(Expand_day_of_week >= START_DAY_OF_WEEK_EXPAND |
           Expand_day_of_week <= END_DAY_OF_WEEK_EXPAND)

# Create CLOSE_OPEN field to indicate that expanded records are related to Closures or Openings
add_closure_value_weekly <- expand_weekly_day_keep %>%
  mutate(CLOSE_OPEN = VALUE)

# One Time Closures ####
# Filter dataset to One Time regulations
One_closures <- mh_data_log %>%
  filter(MANAGEMENT_TYPE_USE == 'CLOSURE', MANAGEMENT_STATUS_USE == 'ONCE', NEVER_IMPLEMENTED == 0, REG_REMOVED == 0, COMMON_NAME_USE == spp, REGION == region)

# Expand dates between EFFECTIVE_DATE and END_DATE2
expand_one <- One_closures %>%
  filter(END_DATE2 >= START_DATE2) %>%
  mutate(date_sequence = map2(START_DATE2, END_DATE2, seq, by = "1 day")) %>%
  unnest(date_sequence)

# Create CLOSE_OPEN field to indicate that expanded records are related to Closures or Openings
add_closure_value_one <- expand_one %>%
  mutate(CLOSE_OPEN = VALUE)

# Join Closure Types Together ####
# Combine closure data frames
Combined_closures <- bind_rows(add_closure_value_seasonal, add_closure_value_weekly, add_closure_value_one)

# Sort the data frame by date_sequence
sort_closures <- Combined_closures %>%
  arrange(date_sequence)

# Create Multi_expand to indicate whether records have the same date_sequence 
sort_closures_multi <- sort_closures %>%
  group_by(date_sequence) %>%
  mutate(Multi_expand = ifelse(n() > 1, 1, 0)) %>%
  ungroup()

# Keep information from newest FR 
closures_remove_multi <- sort_closures_multi %>%
  arrange(date_sequence, desc(FR_CITATION)) %>%
  group_by(ZONE_USE, date_sequence) %>%
  slice(1) %>%
  ungroup()

# Remove extra processing fields from dataset
closures_remove_fields <- closures_remove_multi %>%
  select(-START_YEAR_expand, -END_YEAR_expand, -START_DATE_EXPAND, -END_DATE_EXPAND,
         -START_DAY_OF_WEEK_EXPAND, -END_DAY_OF_WEEK_EXPAND, -Multi_expand)

# Create EFFECTIVE_YEAR field to indicate the effective year of the closure
closures_effective_year <- closures_remove_fields %>%
  mutate(EFFECTIVE_YEAR = year(date_sequence))

# Create EFFECTIVE_MONTH field to indicate the effective month of the closure
closures_effective_month <- closures_effective_year %>%
  mutate(EFFECTIVE_MONTH = month(date_sequence))

# Create data frame with only REGION, ZONE_USE, date_sequence, CLOSE_OPEN, EFFECTIVE_YEAR, and EFFECTIVE_MONTH
select_closures <- closures_effective_month %>%
  select(REGION, ZONE_USE, date_sequence, CLOSE_OPEN, EFFECTIVE_YEAR, EFFECTIVE_MONTH)

# Fill in missing date_sequence dates
# Create a data frame with all dates for the specified EFFECTIVE_YEAR
all_dates <- select_closures %>%
  group_by(REGION, ZONE_USE, EFFECTIVE_YEAR) %>%
  summarize(start_date = as.Date(paste(EFFECTIVE_YEAR, "01-01", sep = "-")),
            end_date = as.Date(paste(EFFECTIVE_YEAR, "12-31", sep = "-"))) %>%
  rowwise() %>%
  mutate(date_sequence = list(seq.Date(start_date, end_date, by = "1 day"))) %>%
  ungroup() %>%
  select(-start_date, -end_date)

# Expand the date_sequence list into separate rows
# LOSE ZONES HERE!!!
all_dates_expand <- all_dates %>%
  rowwise() %>%
  mutate(date_sequence = list(unlist(date_sequence))) %>%
  unnest(date_sequence)

# Merge the expanded data frame with the original select_closures data frame
merged_dates <- all_dates %>%
  left_join(select_closures, by = c("REGION", "ZONE_USE", "EFFECTIVE_YEAR", "date_sequence"))

# Remove any duplicated dates
duplicated_data <- merged_dates %>%
  group_by(REGION, ZONE_USE, date_sequence) %>%
  summarize(CLOSE_OPEN = ifelse(all(is.na(CLOSE_OPEN)), "OPEN", first(na.omit(CLOSE_OPEN)))) %>%
  ungroup()

# Extract EFFECTIVE_YEAR from date_sequence
effective_year <- duplicated_data %>%
  mutate(EFFECTIVE_YEAR1 = year(date_sequence))

# Extract EFFECTIVE_MONTH1 from date_sequence
effective_month <- effective_year %>%
  mutate(EFFECTIVE_MONTH1 = month(date_sequence))

# Create MONTH_TOTAL field to indicate how many days the fishery was closed for each month
month_total <- effective_month %>%
  group_by(REGION, ZONE_USE, EFFECTIVE_YEAR1, EFFECTIVE_MONTH1) %>%
  summarise(MONTH_TOTAL = sum(CLOSE_OPEN == "CLOSE", na.rm = TRUE))

# Create SEASON field to indicate if the fishery is opened or closed for the EFFECTIVE_MONTH1 of the EFFECTIVE_YEAR1
# The fishery is considered closed if more than 15 days within a month are closed
final_closure <- month_total %>%
  group_by(EFFECTIVE_YEAR1, EFFECTIVE_MONTH1) %>%
  mutate(SEASON = ifelse(MONTH_TOTAL > 15, "CLOSED", "OPEN")) %>%
  slice(1) %>%
  ungroup()
