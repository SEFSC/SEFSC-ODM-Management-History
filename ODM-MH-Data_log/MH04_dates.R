# Script 4
# Address ADJUSTMENT records and fill in dates

# Overview: ####
  # Create variables to properly sort ADJUSTMENT records
  # Insert reversion records into the time series after an ADJUSTMENT record ends
  # Create date related logic and overwrite END_DATE as needed

# Create variables to properly sort ADJUSTMENT records ####
# Results in mh_dates_prep data frame
mh_dates_prep <- mh_cluster_ids %>%
  # Only process regulations that happen before the terminal date of the processing period
  filter(EFFECTIVE_DATE <= end_timeseries) %>%
  # Arrange by CLUSTER, START_DATE, vol, and page
  # Group by CLUSTER, ZONE_USE, and MANAGEMENT_STATUS_USE
  arrange(CLUSTER, desc(START_DATE), desc(vol), desc(page)) %>%
  group_by(CLUSTER, ZONE_USE, MANAGEMENT_STATUS_USE) %>%
  # CREATE: the variable of GROUP_START to indicate the START_DATE of the first regulation 
  # in the cluster (when the CLUSTER began)
  # CREATE: the variable of FIRST_REG to flag the first record within a CLUSTER grouping
  mutate(GROUP_START = min(START_DATE),
         FIRST_REG = GROUP_START == START_DATE) %>%
  # CREATE: the variable of LINK to connect ADJUSTMENT and reversion records
  # The LINK variable will only be present for regulations that are an ADJUSTMENT
  # The LINK variable indicates the REGULATION_ID of the non-ADJUSTMENT records that occurred before an ADJUSTMENT record
  # The record associated with that REGULATION_ID will go back into effect once the ADJUSTMENT period has ended
  mutate(LINK = case_when(ADJUSTMENT == 1 & FIRST_REG == 1 ~ 0,
                          ADJUSTMENT == 1 & lead(REG_REMOVED, 1) == 1 ~ 0,
                          ADJUSTMENT == 1 & lead(ADJUSTMENT, 1) != 1 ~ lead(REGULATION_ID, 1),
                          ADJUSTMENT == 1 & lead(ADJUSTMENT, 2) != 1 ~ lead(REGULATION_ID, 2),
                          ADJUSTMENT == 1 & lead(ADJUSTMENT, 3) != 1 ~ lead(REGULATION_ID, 3),
                          ADJUSTMENT == 1 & lead(ADJUSTMENT, 4) != 1 ~ lead(REGULATION_ID, 4),
                          ADJUSTMENT == 1 & lead(ADJUSTMENT, 5) != 1 ~ lead(REGULATION_ID, 5),
                          ADJUSTMENT == 1 & lead(ADJUSTMENT, 6) != 1 ~ lead(REGULATION_ID, 6),
                          ADJUSTMENT == 1 & lead(ADJUSTMENT, 1) == 1 & lead(FIRST_REG, 1) == 1 ~ 0,
                          ADJUSTMENT == 1 & lead(ADJUSTMENT, 2) == 1 & lead(FIRST_REG, 2) == 1 ~ 0,
                          ADJUSTMENT == 1 & lead(ADJUSTMENT, 3) == 1 & lead(FIRST_REG, 3) == 1 ~ 0,
                          ADJUSTMENT == 1 & lead(ADJUSTMENT, 4) == 1 & lead(FIRST_REG, 4) == 1 ~ 0,
                          ADJUSTMENT == 1 & lead(ADJUSTMENT, 5) == 1 & lead(FIRST_REG, 5) == 1 ~ 0,
                          ADJUSTMENT == 1 & lead(ADJUSTMENT, 6) == 1 & lead(FIRST_REG, 6) == 1 ~ 0,
                          ADJUSTMENT == 1 & lead(ADJUSTMENT, 7) == 1 & lead(FIRST_REG, 7) == 1 ~ 0,
                          ADJUSTMENT == 1 & lead(ADJUSTMENT, 8) == 1 & lead(FIRST_REG, 8) == 1 ~ 0,
                          ADJUSTMENT == 1 & lead(ADJUSTMENT, 9) == 1 & lead(FIRST_REG, 9) == 1 ~ 0,
                          ADJUSTMENT == 1 & lead(ADJUSTMENT, 10) == 1 & lead(FIRST_REG, 10) == 1 ~ 0,
                          ADJUSTMENT == 1 & lead(ADJUSTMENT, 11) == 1 & lead(FIRST_REG, 11) == 1 ~ 0,
                          ADJUSTMENT == 1 & lead(ADJUSTMENT, 12) == 1 & lead(FIRST_REG, 12) == 1 ~ 0,
                          ADJUSTMENT == 1 & lead(ADJUSTMENT, 13) == 1 & lead(FIRST_REG, 13) == 1 ~ 0,
                          ADJUSTMENT == 1 & lead(ADJUSTMENT, 14) == 1 & lead(FIRST_REG, 14) == 1 ~ 0,
                          ADJUSTMENT == 1 & lead(ADJUSTMENT, 15) == 1 & lead(FIRST_REG, 15) == 1 ~ 0,
                          ADJUSTMENT == 1 & lead(ADJUSTMENT, 16) == 1 & lead(FIRST_REG, 16) == 1 ~ 0,
                          ADJUSTMENT == 1 & lead(ADJUSTMENT, 17) == 1 & lead(FIRST_REG, 17) == 1 ~ 0))
 
   # Explanation of LINK cases: 
    #1 ADJUSTMENT with FIRST_REG (LINK = 0): The FIRST_REG in a CLUSTER is an ADJUSTMENT MANAGEMENT_TYPE, therefore there is no LINK to a prior regulation
    #2 ADJUSTMENT not FIRST_REG, with real reversion sometime before it (LINK != 0): The ADJUSTMENT record is not a FIRST_REG, therefore, it should revert to a prior regulation
    #3 ADJUSTMENT not FIRST_REG, with FIRST_REG ADJUSTMENT sometime before it (LINK = 0): The ADJUSTMENT record is not a FIRST_REG, however, the prior regulation is a FIRST_REG that is an ADJUSTMENT, therefore there is no reversion
    #4 Non-ADJUSTMENT FIRST_REG (link is null): record is a FIRST_REG but is not an ADJUSTMENT
    #5 Non-ADJUSTMENT not FIRST_REG (link is null): record is not a FIRST_REG or an ADJUSTMENT

    # CHECK: number of REOPENING records that are flagged as ADJUSTMENT
    #62 records where VALUE == "OPEN" flagged as ADJUSTMENT (have ineffective_date)
    mh_dates_open <- mh_dates_prep %>% filter(VALUE == "OPEN" & !is.na(INEFFECTIVE_DATE))
    summary(as.factor(mh_dates_open$ADJUSTMENT))

    # CHECK: Quantifying ADJUSTMENT LINKs for REOPENING records
    # mh_dates_adj = mh_dates_prep %>% filter(ADJUSTMENT == 1)
    # summary(mh_dates_adj$LINK == 0) # Situation 1 + 3 happen 78; Situation 3 happens 164 times
    # 
    # table(mh_dates_prep$VALUE == "OPEN" & !is.na(mh_dates_prep$INEFFECTIVE_DATE), !is.na(mh_dates_prep$LINK)) #All 62 have links
    # table(mh_dates_prep$VALUE == "OPEN" & !is.na(mh_dates_prep$INEFFECTIVE_DATE), abs(mh_dates_prep$LINK) > 0) #113 Links are not associated with re-openings
    # table(mh_dates_prep$VALUE == "OPEN" & !is.na(mh_dates_prep$INEFFECTIVE_DATE), mh_dates_prep$LINK == 0) #11
    # table(mh_dates_prep$VALUE == "OPEN" & !is.na(mh_dates_prep$INEFFECTIVE_DATE), mh_dates_prep$LINK == 0 & mh_dates_prep$FIRST_REG == TRUE)
    
    
# Insert reversion records into the time series after an ADJUSTMENT record ends ####
# Results in mh_reversions data frame
# After an ADJUSTMENT record ends, the prior non-ADJUSTMENT regulation goes back into effect
# A REVERSION record is a duplicate of the non-ADJUSTMENT record prior to an ADJUSTMENT record that goes back into effect when ADJUSTMENT ends
# ADJUSTMENT regulations that are FIRST_REG do not need a reversion since they either end or are overwritten and there is no regulations prior to them
# REVERSION records are only added after ADJUSTMENT records that are not FIRST_REG
mh_reversions = mh_dates_prep %>%
  # Filter mh_dates_prep data frame to only include ADJUSTMENT records (LINK > 0)
  filter(abs(LINK) > 0) %>% #164 regulations
  group_by(LINK, REGULATION_ID) %>%
  # CREATE: the variable of REVERSION (TRUE/FALSE) to indicate that the record is a duplication of the record that occurred before the prior ADJUSTMENT record
  summarize(REVERSION = TRUE,
        # CREATE: the variable of START_DATE_USE to give each REVERSION record a corrected start date (the day after the respective ADJUSTMENT ends)
        START_DATE_USE = END_DATE + 1, 
        # CREATE: the variable of SORT_DATE_USE to insert REVERSION records correctly into time series 
        # REVERSIONS should occur after their corresponding ADJUSTMENT 
        SORT_DATE_USE = START_DATE+ 0.5) %>% 
        # CREATE: the variable of ADJUSTMENT_ID which is the REGULATION_ID of the ADJUSTMENT record prior to the newly created REVERSION record
        # Overwrite REGULATION_ID with the LINK number of the ADJUSTMENT record to indicate that the REVERSION record is a duplication of the record prior to the ADJUSTMENT record
      rename(ADJUSTMENT_ID = REGULATION_ID, 
         REGULATION_ID = LINK) %>%
  # Join mh_reversions data frame to the mh_dates_prep data frame by REGULATION_ID
  left_join(mh_dates_prep, by = "REGULATION_ID") %>%
  bind_rows(mh_dates_prep) %>%
  # Adjust START_DATE_USE to accommodate REVERSION records in whole data set
  # When START_DATE_USE is not provided, use START_DATE, otherwise use START_DATE_USE
  mutate(START_DATE_USE = case_when(is.na(START_DATE_USE) ~ START_DATE,
                                    TRUE ~ START_DATE_USE),
         # When SORT_DATE_USE is not provided, use START_DATE_USE, otherwise use SORT_DATE_USE
         SORT_DATE_USE = case_when(is.na(SORT_DATE_USE) ~ START_DATE_USE,
                                   TRUE ~ SORT_DATE_USE),
         # If REVERSION is null, fill in value of FALSE
         REVERSION = case_when(REVERSION == TRUE ~ TRUE,
                               TRUE ~ FALSE))

  # CHECK: REGULATION_ID 927 to confirm that new variables are working correctly 
  chk_reg = mh_reversions %>% filter(REGULATION_ID == "927")

  
# Create date related logic and overwrite end date as needed ####
# Results in mh_dates data frame
# Add END_DATE and CHANGE_DATE logic to sorted records
# CREATE: END_DATE variable dependent on INEFFECTIVE_DATE of current record, START_DATE of next record,
# and CHANGE_DATE of current record
mh_dates <- mh_reversions %>%
  # Arrange by CLUSTER, SORT_DATE_USE, vol, and page
  # Group by CLUSTER, ZONE_USE, and MANAGEMENT_STATUS_USE
  arrange(CLUSTER, desc(SORT_DATE_USE), desc(vol), desc(page)) %>%
  group_by(CLUSTER, ZONE_USE, MANAGEMENT_STATUS_USE) %>%
  # CREATE: the variable of diff and diff_days to signify the length of time between a 
  # regulation and its subsequent replacement (amount of days the regulation is effective for)
  # The most recent regulation in each grouping gets NA for diff because no regulations happens after it (it is the most recent and at the top of the list)
  mutate(diff = as.numeric(lag(START_DATE_USE) - START_DATE_USE, units = 'days'),
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
         # CREATE: the variable of CHANGE_DATE to indicate when a regulation changed to the subsequent regulation (day before START_DATE_USE of subsequent regulation)
         # When diff_days is not calculated due to there being no subsequent regulation, 
         # the end of the time series should be used as the CHANGE_DATE
         # This variable just indicates the time of a subsequent regulation, it does not necessarily mean the regulation ended then 
         CHANGE_DATE = case_when(is.na(diff_days) ~ end_timeseries,
                                 TRUE ~ START_DATE_USE + diff_days),
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
         # When the MULTI_REG_VALUE variable is flagged (1), NEVER_IMPLEMENTED should not be flagged (0) meaning the regulation did go into effect
         # When the MULTI_REG_CLOSURE variable is flagged (1), NEVER_IMPLEMENTED should not be flagged (0) meaning the regulation did go into effect
         # When the diff_days variable is less than or equal to -1, NEVER_IMPLEMENTED should be flagged (1) meaning the regulation did not go into effect
         # When the START_DATE_USE is after the END_DATE, NEVER_IMPLEMENTED should be flagged (1) meaning the regulation did not go into effect
         # When MULTI_REG_FORECAST is flagged (1) and the START_DATE_USE is equal to the START_DATE_USE of a later FR_CITATION, then NEVER_IMPLEMENTED should be flagged (1) meaning the regulation did not go into effect
         NEVER_IMPLEMENTED = case_when(MULTI_REG_VALUE == 1 ~ 0,
                                       MULTI_REG_CLOSURE == 1 ~ 0,
                                       diff_days <= -1 ~ 1,
                                       START_DATE_USE > END_DATE ~ 1,
                                       MULTI_REG_FORECAST == 1 & START_DATE_USE == max(START_DATE_USE[FR_CITATION > FR_CITATION]) ~ 1,
                                       TRUE ~ 0),
         # ADJUST THE START DATE AND START TIME FOR CLUSTER 306 WHEN THE REOPENING ENDS IN THE MIDDLE OF THE DAY
         START_TIME_USE = case_when(lead(!is.na(END_TIME_USE) & STATUS_TYPE == 'SIMPLE' & VALUE == 'OPEN') & START_DATE_USE == lead(END_DATE) + 1 ~ format(as.POSIXct(lead(END_TIME_USE), format = '%I:%M:%S %p') %m+% minutes(1), "%I:%M:%S %p"),
                                    TRUE ~ START_TIME_USE),
         START_DATE_USE = case_when(lead(!is.na(END_TIME_USE) & STATUS_TYPE == 'SIMPLE' & VALUE == 'OPEN') & START_DATE_USE == lead(END_DATE) + 1 ~ lead(END_DATE),
                                    TRUE ~ START_DATE_USE))
  
  # CHECK: Make sure no reversions are also regulation removals
  dim(filter(mh_dates, REG_REMOVED == 1, REVERSION == TRUE))
  