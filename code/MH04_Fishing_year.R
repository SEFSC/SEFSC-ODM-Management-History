# FISHING YEAR MH PROCESSING

# WHAT MANAGMENT TYPES RECORD JUST YEAR
action_year <- mh_expanded %>%
  filter(is.na(START_DAY), is.na(START_MONTH), !is.na(START_YEAR))
unique(action_year$MANAGEMENT_TYPE_USE)

# ISLATE FISHING YEAR TO UNDERSTAND DATA MORE
fishing_year_check <- mh_cluster_ids %>%
  filter(MANAGEMENT_TYPE_USE == "FISHING YEAR")

# LOOK AT DISTINCT COMBINATIONS
unique_starts <- fishing_year_check %>%
  select(REGULATION_ID, START_YEAR, START_MONTH, START_DAY, START_DAY_USE, END_MONTH, END_DAY, END_DAY_USE) %>%
  distinct()

  # July1-May1 (THIS IS AN ADJUSTMENT)
  # Missing end month when start is May1
  # Missing end month when start is Jan1
  # Missing day when start is March and end is Feb (might need to be March 1st for both the start and end)


# 5 CLUSTERS WITH INEFFECTIVE DATE (967 303 302  79  80)
# MIGHT HAVE TO INTEPRET AS ADJUSTMENTS
# MAYBE REMOVE THEM AND MANUALLY PROCESS THEM BACK IN

fishing_year_check %>%
  filter(!is.na(INEFFECTIVE_DATE)) %>%
  distinct(CLUSTER) %>%
  pull()

# ADDITIONAL CHECKS RELATED TO FISHING YEAR REGULATION
summary(fishing_year_check$MULTI_REG)
unique(fishing_year_check$MANAGEMENT_STATUS_USE)
unique(fishing_year_check$ADJUSTMENT)
unique(fishing_year_check$START_TIME_USE)
unique(fishing_year_check$END_TIME_USE)
table(is.na(fishing_year_check$INEFFECTIVE_DATE))

unique(fishing_year_check$FMP)
n_distinct(fishing_year_check$CLUSTER)

view(fishing_year_check %>% filter(END_TIME_USE == "12:01:00 AM"))

fishing_year_species <- fishing_year_check %>%
  select(FMP, SPP_NAME) %>%
  distinct() %>%
  arrange(FMP)

fishing_year_all <- fishing_year_check %>%
  filter(SPP_NAME == "ALL")


# PROCESS FISHING YEAR FIRST

mh_fishing_year <- mh_cluster_ids %>%
  filter(MANAGEMENT_TYPE_USE == "FISHING YEAR") %>%
  arrange(CLUSTER, desc(START_DATE), desc(vol), desc(page)) %>%
  group_by(CLUSTER, ZONE_USE, MANAGEMENT_STATUS_USE) %>%
  # CREATE: the variable of diff and diff_days to signify the length of time between a 
  # regulation and its subsequent replacement (amount of days the regulation is effective for)
  # The most recent regulation in each grouping gets NA for diff because no regulations happens after it (it is the most recent and at the top of the list)
  mutate(diff = as.numeric(lag(START_DATE) - START_DATE, units = 'days'),
         # If a record doesn't have a START_TIME diff should be adjusted by -1 (one day prior) 
         # This indicates that the regulation started at start of the START_DAY listed and 
         # ended the day before the next regulation started (day before START_DAY of subsequent regulation)
         diff_days = case_when(is.na(lag(START_TIME_USE)) ~ diff - 1, 
                               # When START_TIME is "12:01:00 AM", diff should days should be lagged by one day
                               # This will infer that the regulation began at the start of the day, not one minute into the day
                               # This will help properly calculate diff_days
                               lag(START_TIME_USE) == "12:01:00 AM" ~ diff - 1,
                               # When START_TIME is "12:02:00 AM", diff should days should be lagged by one day
                               # This will infer that the regulation began at the start of the day, not two minutes into the day
                               # This will help properly calculate diff_days
                               lag(START_TIME_USE) == "12:02:00 AM" ~ diff - 1,
                               TRUE ~ diff),
         # CREATE: the variable of CHANGE_DATE to indicate when a regulation changed to the subsequent regulation (day before START_DATE of subsequent regulation)
         # When diff_days is not calculated due to there being no subsequent regulation, 
         # the end of the time series should be used as the CHANGE_DATE
         # This variable just indicates the time of a subsequent regulation, it does not necessarily mean the regulation ended then 
         CHANGE_DATE = case_when(is.na(diff_days) ~ end_timeseries,
                                 TRUE ~ START_DATE + diff_days),
         # When diff_days is calculated as -1, the CHANGE_DATE should be lagged by one day 
         # For most cases, a diff_day of -1 indicates that the regulation never went into effect (except in the cases of MULTI_REG regulations)
         # Therefore, the CHANGE_DATE is lagged by one day to indicate that the regulation is not implemented 
         # and allow the information from the prior regulation to link with the subsequent regulation that is implemented. 
         # Lagging the change date is different than subtracting one day from the duration (since the diff_days variable is equal to -1)
         CHANGE_DATE = case_when(diff_days == -1 ~ lag(CHANGE_DATE), 
                                 TRUE ~ CHANGE_DATE),
         CHANGE_DATE = case_when(CHANGE_DATE > lag(CHANGE_DATE) ~ lag(CHANGE_DATE), 
                                 TRUE ~ CHANGE_DATE),
         # When an END_DATE is provided it should be used to signify the END_DATE
         # Otherwise, the CHANGE_DATE should be used for the END_DATE information
         END_DATE = case_when(!is.na(END_DATE) ~ END_DATE,
                              # is.na(END_YEAR) & !is.na(INEFFECTIVE_DATE) ~ INEFFECTIVE_DATE,
                              TRUE ~ CHANGE_DATE),
         # If the CHANGE_DATE is after the END_DATE and there is an END_DATE provided, then the END_DATE should be used
         # If the CHANGE_DATE is after the INEFFECTIVE_DATE and an INEFFECTIVE_DATE is provided, then the END_DATE should be used
         # Otherwise, the CHANGE_DATE should be used as the END_DATE
         END_DATE = case_when(CHANGE_DATE > END_DATE & !is.na(END_DATE) ~ END_DATE,
                              CHANGE_DATE > INEFFECTIVE_DATE & !is.na(INEFFECTIVE_DATE) ~ END_DATE,
                              TRUE ~ CHANGE_DATE),
         # CREATE: the variable of NEVER_IMPLEMENTED to signify regulations that were created but never went into effect
         # When the MULTI_REG variable is flagged (1), NEVER_IMPLEMENTED should not be flagged (0) meaning the regulation did go into effect
         # When the diff_days variable is less than or equal to -1 and MULTI_REG is 0, NEVER_IMPLEMENTED should be flagged (1) meaning the regulation did not go into effect
         # When the START_DATE is after the END_DATE, NEVER_IMPLEMENTED should be flagged (1) meaning the regulation did not go into effect
         NEVER_IMPLEMENTED = case_when(#MULTI_REG == 1 ~ 0,
           diff_days <= -1 ~ 1,
           START_DATE > END_DATE ~ 1,
           TRUE ~ 0))

# Filter for only fishing year management type
# Include REG_REMOVED == 0 because this helps us in getting an end date for fishing years that end, but we do not want to keep the record that "turns off" the regulation
mh_fy <- mh_dates %>%
  filter(MANAGEMENT_TYPE_USE == 'FISHING YEAR', REG_REMOVED == 0)

# Identify clusters that have a split fishing year - NONE
mh_fy2 <- mh_fy %>% group_by(FR_CITATION, FMP, REGION, SPP_NAME, SECTOR_USE, SUBSECTOR_USE, ZONE_USE, EFFECTIVE_DATE) %>%
  summarise(N = n())

mh_fy_dates <- mh_fy %>% ungroup() %>%
  select(CLUSTER, ZONE_USE, START_DATE_USE, END_DATE, START_MONTH, START_DAY_USE) %>%
  # Format start month and day into FY1
  mutate()

chk <- mh_fy %>% filter(FR_CITATION == '80 FR 4216')
chk2 <- mh_fy %>% filter(CLUSTER == 79) %>% select(CLUSTER, FR_CITATION, FMP, REGION, SPP_NAME, SECTOR_USE, SUBSECTOR_USE, EFFECTIVE_DATE,
                                                   START_DATE_USE, END_DATE, START_MONTH, START_DAY_USE)
