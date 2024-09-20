# Script 4
# Address ADJUSTMENT records and fill in dates for DETAILED MANAGEMENT_TYPEs, excluding CATCH LIMITS.
# For CATCH LIMITS, adjust the start information based on available FISHING YEAR/FISHING SEASON information

# Overview: ####
  # For DETAILED MANAGEMENT_TYPEs excluding CATCH LIMITs
  # Create variables to properly sort ADJUSTMENT records
  # Insert reversion records into the time series after an ADJUSTMENT record ends
  # Create date related logic and overwrite END_DATE as needed

  # For MANAGEMENT_CATEGORY == CATCH LIMITs
  # Adjust START_DATE information to align with the start of applicable FISHING YEAR/FISHING SEASON information

# Create variables to properly sort ADJUSTMENT records for DETAILED MANAGEMENT_TYPEs excluding CATCH LIMITS ####
# Results in mh_dates_detailed data frame
mh_dates_detailed <- mh_cluster_ids %>%
  # Process regulations for MANAGEMENT CATEGORY other than CATCH LIMITS
  # CATCH LIMITS regulations will be processed separately
  filter(MANAGEMENT_CATEGORY != "CATCH LIMITS") %>%
  # Only process DETAILED == YES MANAGEMENT_TYPEs
  filter(DETAILED == "YES") %>%
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
    mh_dates_open <- mh_dates_detailed %>% filter(VALUE == "OPEN" & !is.na(INEFFECTIVE_DATE))
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
mh_reversions <- mh_dates_detailed %>%
  # Filter mh_dates_detailed data frame to only include ADJUSTMENT records (LINK > 0)
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
  left_join(mh_dates_detailed, by = "REGULATION_ID") %>%
  bind_rows(mh_dates_detailed) %>%
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

# Create date related logic and overwrite end date for DETAILED MANAGEMENT_TYPEs excluding CATCH LIMITS ####
# Results in mh_dates1 data frame
# Add END_DATE and CHANGE_DATE logic to sorted records
# CREATE: END_DATE variable dependent on INEFFECTIVE_DATE of current record, START_DATE of next record,
# and CHANGE_DATE of current record
mh_dates1 <- mh_reversions %>%
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
         END_DATE = case_when(EFFECTIVE_DATE == INEFFECTIVE_DATE ~ START_DATE,
                              !is.na(END_DATE) ~ END_DATE,
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
         # When the MULTI_REG_SEASONAL variable is flagged (1), NEVER_IMPLEMENTED should not be flagged (0) meaning the regulation did go into effect
         # When the diff_days variable is less than or equal to -1, NEVER_IMPLEMENTED should be flagged (1) meaning the regulation did not go into effect
         # When the START_DATE_USE is after the END_DATE, NEVER_IMPLEMENTED should be flagged (1) meaning the regulation did not go into effect
         # When MULTI_REG_FORECAST is flagged (1) and the START_DATE_USE is equal to the START_DATE_USE of a later FR_CITATION, then NEVER_IMPLEMENTED should be flagged (1) meaning the regulation did not go into effect
         NEVER_IMPLEMENTED = case_when(MANAGEMENT_CATEGORY == "TEMPORAL CONTROLS" & START_DATE_USE == lag(START_DATE_USE) & END_DATE != lag(END_DATE) & VALUE != lag(VALUE) ~ 0,
                                      MULTI_REG_SEASONAL == 1 ~ 0,
                                      START_DATE_USE > END_DATE ~ 1,
                                       MULTI_REG_VALUE == 1 ~ 0,
                                       diff_days <= -1 ~ 1,
                                       # Added 2/2/24 - if multi reg forcast & only include start year, do not want to flag
                                       MULTI_REG_FORECAST == 1 & is.na(START_DAY_USE) & is.na(START_MONTH) & !is.na(START_YEAR) ~ 0,
                                       MULTI_REG_FORECAST == 1 & START_DATE_USE == max(START_DATE_USE[FR_CITATION > FR_CITATION]) ~ 1,
                                       # Added 2/5/24 to hard code the two cases of WITHDRAWN/DELAYED regulations
                                       REGULATION_ID %in% c(1355, 1589, 1356, 1463, 1341) ~ 1,
                                       TRUE ~ 0),
         # ADJUST THE START DATE AND START TIME FOR CLUSTER 306 WHEN THE REOPENING ENDS IN THE MIDDLE OF THE DAY
         START_TIME_USE = case_when(lead(!is.na(END_TIME_USE) & STATUS_TYPE == 'SIMPLE' & VALUE == 'OPEN') & START_DATE_USE == lead(END_DATE) + 1 ~ format(as.POSIXct(lead(END_TIME_USE), format = '%I:%M:%S %p') %m+% minutes(1), "%I:%M:%S %p"),
                                    TRUE ~ START_TIME_USE),
         START_DATE_USE = case_when(lead(!is.na(END_TIME_USE) & STATUS_TYPE == 'SIMPLE' & VALUE == 'OPEN') & START_DATE_USE == lead(END_DATE) + 1 ~ lead(END_DATE),
                                    TRUE ~ START_DATE_USE)) %>%
    # Added 2/2/24 - fix on end date
    group_by(CLUSTER, FR_CITATION) %>%
    mutate(END_DATE_adjust = case_when(MULTI_REG_VALUE == 1 & MULTI_REG_FORECAST != 1 & MULTI_REG_SEASONAL != 1 ~ max(END_DATE)),
           END_DATE = case_when(MANAGEMENT_TYPE_USE == "DEFINITION" ~ end_timeseries,
                               # is.na(INEFFECTIVE_DATE) & !is.na(END_DATE_adjust) ~ END_DATE_adjust,
                                TRUE ~ END_DATE)) %>%
    # Added 2/2/24 - do not include records that were added because of an adjustment, but not actually implemented
    filter(!(NEVER_IMPLEMENTED == 1 & REVERSION == TRUE))
  
  # CHECK: Make sure no reversions are also regulation removals
  dim(filter(mh_dates1, REG_REMOVED == 1, REVERSION == TRUE))
  
  # Adjust start date information for CATCH LIMIT MANAGEMENT_TYPEs ####
  # First reclassify regulations entered as FISHING SEASON to FISHING YEAR
  # Only include REG_REMOVED == 0 since we do not want to keep records that "turn off" the previous regulations
  mh_fs <- mh_dates1 %>%
    filter(MANAGEMENT_TYPE_USE == 'FISHING SEASON', REG_REMOVED == 0)
  
  # Identify cases that have a split fishing season
  mh_fs_split <- mh_fs %>% group_by(FR_CITATION, FMP, REGION, SPP_NAME, SECTOR_USE, SUBSECTOR_USE, ZONE_USE, EFFECTIVE_DATE) %>%
    summarise(N_seasons = n()) 
  
  # Join to data
  mh_fs2 <- mh_fs %>% 
    left_join(mh_fs_split, by = c("FR_CITATION", "REGION", "FMP", "EFFECTIVE_DATE", "SECTOR_USE", "SPP_NAME", "ZONE_USE",
                                 "SUBSECTOR_USE"))
  
  # Identify number of days within a fishing season 
  mh_fs_numdays <- mh_fs2 %>% 
    # Create date fields to determine season length
    mutate(current_year = format(Sys.Date(), "%Y"),
           START_YEAR_USE = year(START_DATE),
           next_end_year = START_YEAR_USE + 1,
           # CREATE: START_FY to indicate the start date of the fishing year
           # When a START_YEAR_USE is not available, use the current_year
           START_FY = case_when(!is.na(START_MONTH) & !is.na(START_DAY_USE) & is.na(START_YEAR_USE) ~ as.Date(paste0(current_year, "-", START_MONTH, "-", START_DAY_USE), "%Y-%m-%d"),
                                !is.na(START_MONTH) & !is.na(START_DAY_USE) & !is.na(START_YEAR_USE) ~ as.Date(paste0(START_YEAR_USE, "-", START_MONTH, "-", START_DAY_USE), "%Y-%m-%d")),
           # CREATE: END_FY to indicate the end date of the fishing year
           # When END_YEAR_USE is not available, use the next_end_year if the END_MONTH_USE occurs before the START_MONTH
           # When END_YEAR_USE is not available, use the START_YEAR_USE if the END_MONTH_USE occurs after the START_MONTH
           # When an END_YEAR_USE is available, use END_YEAR_USE
           # When END_MONTH_USE, END_DAY_USE, END_YEAR_USE is not available and START_DAY_OF_WEEK_USE and END_DAY_OF_WEEK_USE is available, use one day after the START_FY
           END_FY = case_when(END_MONTH_USE < START_MONTH & !is.na(END_MONTH_USE) & !is.na(END_DAY_USE) & is.na(END_YEAR_USE) ~ as.Date(paste0(next_end_year, "-", END_MONTH_USE, "-", END_DAY_USE), "%Y-%m-%d"),
                              END_MONTH_USE >= START_MONTH & !is.na(END_MONTH_USE) & !is.na(END_DAY_USE) & is.na(END_YEAR_USE) ~ as.Date(paste0(START_YEAR_USE, "-", END_MONTH_USE, "-", END_DAY_USE), "%Y-%m-%d"),
                              !is.na(END_MONTH_USE) & !is.na(END_DAY_USE) & !is.na(END_YEAR_USE) ~ as.Date(paste0(END_YEAR_USE, "-", END_MONTH_USE, "-", END_DAY_USE), "%Y-%m-%d"),
                              is.na(END_MONTH_USE) & is.na(END_DAY_USE) & is.na(END_YEAR_USE) & !is.na(END_DAY_OF_WEEK_USE) & !is.na(START_DAY_OF_WEEK_USE) ~ START_FY + 1),
           # CREATE: DIFF to calculate the amount of time between START_FY and END_FY
           # When END_MONTH_USE is after START_MONTH and END_YEAR_USE and START_YEAR_USE is the same, then subtract the START_FY from END_FY
           # When END_MONTH_USE is before START_MONTH and END_YEAR_USE and START_YEAR_USE is the same, then subtract END_FY from START_FY
           # When the END_MONTH_USE is not available, and END_FY is after START_FY, then subtract START_FY from END_FY
           DIFF = case_when(END_MONTH_USE >= START_MONTH ~ END_FY - START_FY,
                            END_MONTH_USE < START_MONTH ~ START_FY - END_FY,
                            is.na(END_MONTH_USE) & END_FY > START_FY ~ END_FY - START_FY),
           DIFF = abs(DIFF))
  
  # Prepare to reclassify FISHING SEASON records as FISHING YEAR that are 364/365 days
  fs_to_fy <- mh_fs_numdays %>% ungroup() %>%
    filter(DIFF == 364 | DIFF == 365) %>%
    select(REGULATION_ID) %>% pull()
  
  # Filter for only FISHING YEAR MANAGEMENT_TYPEs
  # Reclassify regulations to FISHING YEAR if FISHING SEASON is 365 days
  # Only include REG_REMOVED == 0 since we do not want to keep records that "turn off" the previous regulations
  mh_fy <- mh_dates1 %>% ungroup() %>%
    mutate(MANAGEMENT_TYPE_USE = case_when(REGULATION_ID %in% fs_to_fy ~ 'FISHING YEAR',
                                           TRUE ~ MANAGEMENT_TYPE_USE)) %>%
    filter(MANAGEMENT_TYPE_USE == 'FISHING YEAR', REG_REMOVED == 0) %>%
    # Recode CLUSTER ID of recoded MANAGEMENT_TYPEs so the CLUSTERs match
    # These are clusters that were entered as FISHING SEASON but are actually a FISHING_YEAR
    mutate(NEW_CLUSTER = case_when(!REGULATION_ID %in% fs_to_fy & SPP_NAME != 'ALL' ~ CLUSTER)) %>%
    arrange(START_DATE_USE) %>%
    group_by(MANAGEMENT_TYPE_USE, JURISDICTION, JURISDICTIONAL_WATERS, FMP, SECTOR_USE, SUBSECTOR_USE, REGION, SPP_NAME) %>%
    # first down and then up - this satisfies cases where first date needs a new cluster
    fill(NEW_CLUSTER, .direction = "downup") %>%
    ungroup()
  
  # Identify clusters that have a split fishing year
  mh_fy_split <- mh_fy %>% group_by(FR_CITATION, FMP, REGION, SPP_NAME, SECTOR_USE, SUBSECTOR_USE, ZONE_USE, EFFECTIVE_DATE) %>%
    summarise(N = n()) %>%
    filter(N > 1)
  # STOP code if we have a split fishing year because the code needs to account for this
  if((nrow(mh_fy_split) != 0)) { 
    stop("Fishing year split, please investigate")}
  
  # Identify clusters that have a fishing year < 365 days
  mh_fy_less365 <- mh_fy %>%
    select(FMP, REGION, SPP_NAME, SECTOR_USE, SUBSECTOR_USE, ZONE_USE, MANAGEMENT_STATUS_USE, START_MONTH, START_DAY_USE, END_MONTH_USE, END_DAY_USE) %>%
    # Create as date field to subtract (the year is not important here)
    mutate(START_FY = as.Date(paste0("2023-", START_MONTH, "-", START_DAY_USE), "%Y-%m-%d"),
           END_FY = as.Date(paste0("2023-", END_MONTH_USE, "-", END_DAY_USE), "%Y-%m-%d"),
           DIFF = START_FY - END_FY) %>%
    filter(DIFF != 1 & DIFF != 0 & DIFF != -364)
  # STOP code if we have a split fishing year because the code needs to account for this
  if(nrow(mh_fy_less365) != 0) { stop("Fishing year not 365 days")}
  
  # CREATE: START_MONTH2 to use for determining the start month and day of the FISHING YEAR
  mh_fy2 <- mh_fy %>% ungroup() %>%
    select(NEW_CLUSTER, FMP, REGION, SPP_NAME, SECTOR_USE, SUBSECTOR_USE, ZONE_USE, START_DATE_USE, START_MONTH, START_DAY_USE) %>%
    # Format start month and day as FY_MONTH and FY_DAY
    # Get month from number to month name abbreviation
    mutate(START_MONTH2 = format(as.Date(paste0("2021-", START_MONTH, "-01"), "%Y-%m-%d"), "%b"),
           FY = paste0(START_DAY_USE, "-", START_MONTH2))
  
  # Consolidate into only meaningful changes
  mh_fy3 <- mh_fy2 %>%
    ungroup() %>%
    group_by(NEW_CLUSTER, ZONE_USE, FY) %>%
    mutate(EFFECTIVE_DATE_FY = min(START_DATE_USE)) %>%
    select(NEW_CLUSTER, FMP, REGION, SPP_NAME, SECTOR_USE, SUBSECTOR_USE, ZONE_USE,
           EFFECTIVE_DATE_FY, FY) %>%
    distinct() %>%
    group_by(FMP, REGION, SECTOR_USE, SUBSECTOR_USE, ZONE_USE, SPP_NAME) %>%
    arrange(EFFECTIVE_DATE_FY) %>%
    mutate(reg_order = rank(EFFECTIVE_DATE_FY)) #%>%
    # Remove clusters 691 and 692 because cause duplicates when species expansion
    #filter(!NEW_CLUSTER %in% c(691, 692))
  
  # Identify changes in fishing year
  chk_change <- mh_fy3 %>% 
    group_by(FMP, REGION, SECTOR_USE, SUBSECTOR_USE, ZONE_USE, SPP_NAME) %>% 
    summarize(N = n()) %>% filter(N != 1)
  
  # Structure fishing year data to wide-form for joining to the rest of MH data
  mh_fy3_w <- mh_fy3 %>%
    pivot_wider(names_from = reg_order, values_from = c(EFFECTIVE_DATE_FY, FY)) %>%
    ungroup()
  
  # CHECK for duplicates 
  chk_dups <- mh_fy3_w %>%
    group_by(FMP, REGION, SECTOR_USE, SUBSECTOR_USE, ZONE_USE, SPP_NAME) %>%
    summarise(nrecs = n())
  if(any(chk_dups$nrecs) > 1){stop("You have duplicate records in the fishing year dataset ready to join")}

  
  # Subset FISHING YEAR expansion for the join
  # FISHING YEAR for specific subsector, zone, and species
  mh_fy_sz <- mh_fy3_w %>% 
    filter(SUBSECTOR_USE != 'ALL', ZONE_USE != 'ALL') %>%
    rename(EFFECTIVE_DATE_FY_1_sz = "EFFECTIVE_DATE_FY_1",
           EFFECTIVE_DATE_FY_2_sz = "EFFECTIVE_DATE_FY_2",
           FY_1_sz = "FY_1",
           FY_2_sz = "FY_2") %>%
    select(-c(NEW_CLUSTER))
  # Fishing year for specific zone and species
  mh_fy_z <- mh_fy3_w %>% 
    filter(ZONE_USE != 'ALL', SUBSECTOR_USE == 'ALL') %>%
    rename(EFFECTIVE_DATE_FY_1_z = "EFFECTIVE_DATE_FY_1",
           EFFECTIVE_DATE_FY_2_z = "EFFECTIVE_DATE_FY_2",
           FY_1_z = "FY_1",
           FY_2_z = "FY_2") %>%
    select(-c(NEW_CLUSTER, SUBSECTOR_USE))
  # FISHING YEAR for specific species (all zones and subsectors)
  mh_fy_s <- mh_fy3_w %>%
    filter(SPP_NAME != "ALL", ZONE_USE == 'ALL', SUBSECTOR_USE == 'ALL') %>%
    rename(EFFECTIVE_DATE_FY_1_s = "EFFECTIVE_DATE_FY_1",
           EFFECTIVE_DATE_FY_2_s = "EFFECTIVE_DATE_FY_2",
           FY_1_s = "FY_1",
           FY_2_s = "FY_2") %>%
    select(-c(NEW_CLUSTER, SUBSECTOR_USE, ZONE_USE))
  # FISHING YEAR for all zones, subsectors, and species for management types where species expanded
  mh_fy_a <- mh_fy3_w %>% 
    filter(ZONE_USE == 'ALL', SUBSECTOR_USE == 'ALL', SPP_NAME == 'ALL') %>%
    rename(EFFECTIVE_DATE_FY_1_a = "EFFECTIVE_DATE_FY_1",
           EFFECTIVE_DATE_FY_2_a = "EFFECTIVE_DATE_FY_2",
           FY_1_a = "FY_1",
           FY_2_a = "FY_2") %>%
    select(-c(NEW_CLUSTER, SUBSECTOR_USE, ZONE_USE, SPP_NAME))
  
  # Filter the mh_cluster_ids data frame to obtain all CATCH LIMITS records
  Catchlim <- mh_cluster_ids %>%
    filter(MANAGEMENT_CATEGORY == "CATCH LIMITS") %>%
    filter(REG_REMOVED == 0)
  
  # Add FISHING YEAR information to MANAGEMENT_CATEGORY == CATCH LIMITS records
  mh_combine <- Catchlim %>%
    ungroup() %>%
    # When specific to a subsector and zone
    left_join(mh_fy_sz, by = join_by("REGION", "FMP", "SECTOR_USE", "ZONE_USE", "SUBSECTOR_USE",
                                     "SPP_NAME")) %>%
    # When specific to a zone only
    left_join(mh_fy_z, by = join_by("REGION", "FMP", "SECTOR_USE", "ZONE_USE",
                                    "SPP_NAME")) %>%
    # When specific to a species only
    left_join(mh_fy_s, by = join_by("REGION", "FMP", "SECTOR_USE",
                                    "SPP_NAME")) %>%
    # When general to all zones and subsectors for expanded mtypes
    left_join(mh_fy_a, by = join_by("REGION", "FMP", "SECTOR_USE")) %>%
    # Take the first non-null value
    mutate(EFFECTIVE_DATE_FY_1 = coalesce(EFFECTIVE_DATE_FY_1_sz, EFFECTIVE_DATE_FY_1_z, EFFECTIVE_DATE_FY_1_s, EFFECTIVE_DATE_FY_1_a),
           EFFECTIVE_DATE_FY_2 = coalesce(EFFECTIVE_DATE_FY_2_sz, EFFECTIVE_DATE_FY_2_z, EFFECTIVE_DATE_FY_2_s, EFFECTIVE_DATE_FY_2_a),
           FY_1 = coalesce(FY_1_sz, FY_1_z, FY_1_s, FY_1_a),
           FY_2 = coalesce(FY_2_sz, FY_2_z, FY_2_s, FY_2_a)) %>%
    # Remove unnecessary columns from join
    select(-c(EFFECTIVE_DATE_FY_1_sz, EFFECTIVE_DATE_FY_1_z, EFFECTIVE_DATE_FY_1_s, EFFECTIVE_DATE_FY_1_a, 
              EFFECTIVE_DATE_FY_2_sz, EFFECTIVE_DATE_FY_2_z, EFFECTIVE_DATE_FY_2_s, EFFECTIVE_DATE_FY_2_a,
              FY_1_sz, FY_1_z, FY_1_s, FY_1_a,
              FY_2_sz, FY_2_z, FY_2_s, FY_2_a))
  
# Create logic on how to adjust START_DATE for MANAGEMENT_CATEGORY == CATCH LIMITS with one FISHING YEAR or two FISHING YEAR regulations
  mh_dates_logic <- mh_combine %>%
    # CRERATE: year_number to indicate the number of FISHING YEAR records for a particular strata
    mutate(year_number = case_when(!is.na(EFFECTIVE_DATE_FY_1) & !is.na(EFFECTIVE_DATE_FY_2) ~ 2,
                                     !is.na(EFFECTIVE_DATE_FY_1) & is.na(EFFECTIVE_DATE_FY_2) ~ 1,
                                     TRUE ~ NA),
           # CREATE: adjustment_number to assign a unique number for each combination of regulation effectiveness/FISHING YEAR effectiveness
           adjustment_number = case_when(is.na(year_number) ~ "1",
                                         year_number == 1 & EFFECTIVE_DATE_FY_1 < START_DATE ~ "2",
                                         year_number == 1 & EFFECTIVE_DATE_FY_1 > START_DATE ~ "3",
                                         year_number == 1 & EFFECTIVE_DATE_FY_1 == START_DATE ~ "4",
                                         year_number == 1 & EFFECTIVE_DATE_FY_1 > END_DATE ~ "5",
                                         year_number == 2 & EFFECTIVE_DATE_FY_1 < START_DATE & EFFECTIVE_DATE_FY_2 < START_DATE ~ "6",
                                         year_number == 2 & EFFECTIVE_DATE_FY_1 < START_DATE & EFFECTIVE_DATE_FY_2 > START_DATE & is.na(END_DATE) ~ "7",
                                         year_number == 2 & EFFECTIVE_DATE_FY_1 < START_DATE & EFFECTIVE_DATE_FY_2 > START_DATE & !is.na(END_DATE) & EFFECTIVE_DATE_FY_2 > END_DATE ~ "8",
                                         year_number == 2 & EFFECTIVE_DATE_FY_1 == START_DATE & is.na(END_DATE) ~ "9",
                                         year_number == 2 & EFFECTIVE_DATE_FY_1 > START_DATE & is.na(END_DATE) ~ "10",
                                         year_number == 2 & EFFECTIVE_DATE_FY_1 > END_DATE ~ "11",
                                         year_number == 2 & EFFECTIVE_DATE_FY_1 > START_DATE & END_DATE < EFFECTIVE_DATE_FY_2 ~ "12",
                                         year_number == 2 & EFFECTIVE_DATE_FY_1 == START_DATE & END_DATE < EFFECTIVE_DATE_FY_2 ~ "13",
                                         TRUE ~ "0"),
           # CREATE: adjustment_logic to explain each combination of regulation effectiveness/FISHING YEAR effectiveness
           adjustment_logic = case_when(adjustment_number == "1" ~ "No fishing year info",
                                        adjustment_number == "2" ~ "Fishing starts before reg",
                                        adjustment_number == "3" ~ "Fishing starts after reg",
                                        adjustment_number == "4" ~ "Start on same date",
                                        adjustment_number == "5" ~ "Fishing year after end date",
                                        adjustment_number == "6" ~ "Both seasons start prior to reg",
                                        adjustment_number == "7" ~ "First season starts prior to reg, second beings during",
                                        adjustment_number == "8" ~ "First season starts prior to reg, second occurs after end date",
                                        adjustment_number == "9" ~ "Regulation aligns with fishing year 1",
                                        adjustment_number == "10" ~ "Regulation starts before first fishing year",
                                        adjustment_number == "11" ~ "Fishing year after end date",
                                        adjustment_number == "12" ~ "Regulation starts prior and ends before second fishing year",
                                        adjustment_number == "13" ~ "Regulation starts at same time as fishing year 1 and ends before second fishing year",
                                        TRUE ~ "check"))
  
  # Create logic to adjust START_DATE for each set of conditions
  mh_dates_adjust <- mh_dates_logic %>%
    # Duplicate records for the following adjustment_numbers to accurately capture the start of each FISHING YEAR
      bind_rows(mh_dates_logic %>% filter(adjustment_number == 3),
                mh_dates_logic %>% filter(adjustment_number == 7),
                mh_dates_logic %>% filter(adjustment_number == 9),
                mh_dates_logic %>% filter(adjustment_number == 10),
                mh_dates_logic %>% filter(adjustment_number == 10),
                mh_dates_logic %>% filter(adjustment_number == 12)) %>%
      arrange(REGULATION_ID, adjustment_number) %>%
      group_by(REGULATION_ID, adjustment_number) %>%
    # CREATE: variables needed to adjust START_DATE_final  
    mutate(reg_order = row_number(),
             current_year = format(Sys.Date(), "%Y"),
             FY_1_date = as.Date(paste0(current_year, "-", FY_1), format = "%Y-%d-%b"),
             FY_2_date = as.Date(paste0(current_year, "-", FY_2), format = "%Y-%d-%b"),
             MONTH_FY1 = month(FY_1_date),
             DAY_FY1 = day(FY_1_date),
             YEAR_FY1 = year(EFFECTIVE_DATE_FY_1),
             MONTH_FY2 = month(FY_2_date),
             DAY_FY2 = day(FY_2_date),
             YEAR_FY2 = year(EFFECTIVE_DATE_FY_2),
           # CREATE: START_DATE_final to adjust START_DATE based on the FISHING YEAR conditions
             START_DATE_final = case_when(adjustment_number == 1 ~ START_DATE,
                                          adjustment_number == 2 ~ as.Date(paste0(START_YEAR, "-", MONTH_FY1, "-", DAY_FY1), "%Y-%m-%d"),
                                          adjustment_number == 3 & reg_order == 1 ~ START_DATE,
                                          adjustment_number == 3 & reg_order == 2 ~ as.Date(paste0(YEAR_FY1,"-", MONTH_FY1, "-", DAY_FY1), "%Y-%m-%d"),
                                          adjustment_number == 4 ~ as.Date(paste0(YEAR_FY1,"-", MONTH_FY1, "-", DAY_FY1), "%Y-%m-%d"),
                                          adjustment_number == 5 ~ START_DATE,
                                          adjustment_number == 6 ~ as.Date(paste0(START_YEAR, "-", MONTH_FY2, "-", DAY_FY2), "%Y-%m-%d"),
                                          adjustment_number == 7 & reg_order == 1 ~ as.Date(paste0(START_YEAR,"-", MONTH_FY1, "-", DAY_FY1), "%Y-%m-%d"),
                                          adjustment_number == 7 & reg_order == 2 ~ as.Date(paste0(YEAR_FY2,"-", MONTH_FY2, "-", DAY_FY2), "%Y-%m-%d"),
                                          adjustment_number == 8 ~ as.Date(paste0(START_YEAR, "-", MONTH_FY1, "-", DAY_FY1), "%Y-%m-%d"),
                                          adjustment_number == 9 & reg_order == 1 ~ as.Date(paste0(YEAR_FY1,"-", MONTH_FY1, "-", DAY_FY1), "%Y-%m-%d"),
                                          adjustment_number == 9 & reg_order == 2 ~ as.Date(paste0(YEAR_FY2,"-", MONTH_FY2, "-", DAY_FY2), "%Y-%m-%d"),
                                          adjustment_number == 10 & reg_order == 1 ~ START_DATE,
                                          adjustment_number == 10 & reg_order == 2 ~ as.Date(paste0(YEAR_FY1,"-", MONTH_FY1, "-", DAY_FY1), "%Y-%m-%d"),
                                          adjustment_number == 10 & reg_order == 3 ~ as.Date(paste0(YEAR_FY2,"-", MONTH_FY2, "-", DAY_FY2), "%Y-%m-%d"),
                                          adjustment_number == 11 & reg_order == 1 ~ START_DATE,
                                          adjustment_number == 12 & reg_order == 1 ~ START_DATE,
                                          adjustment_number == 12 & reg_order == 2 ~ as.Date(paste0(YEAR_FY1,"-", MONTH_FY1, "-", DAY_FY1), "%Y-%m-%d"),
                                          adjustment_number == 13 & reg_order == 1 ~ as.Date(paste0(YEAR_FY1,"-", MONTH_FY1, "-", DAY_FY1), "%Y-%m-%d"),
                                          TRUE ~ NA))
  
  # For MANAGEMENT_CATEGORY == CATCH LIMITS, use the created START_DATE_final field to update the START_DATE
  # For all other records, keep using START_DATE
  mh_catchlim_prep <- mh_dates_adjust %>%
    mutate(START_DATE_USE = case_when(!is.na(START_DATE_final) ~ START_DATE_final,
                                  TRUE ~ START_DATE)) %>%
    select(-START_DATE_final, -EFFECTIVE_DATE_FY_1, -EFFECTIVE_DATE_FY_2, -FY_1, -FY_2, -year_number, -adjustment_number, -adjustment_logic, -reg_order, -current_year,
           -FY_1_date, -FY_2_date, -MONTH_FY1, -MONTH_FY2, -DAY_FY1, -DAY_FY2, -YEAR_FY1, -YEAR_FY2, -FY_1_date, -FY_2_date)
  
  # Separate CATCH LIMITS regulations where REG_REMOVED == 1
  mh_removed_catchlim <- mh_cluster_ids %>%
    filter(MANAGEMENT_CATEGORY == "CATCH LIMITS") %>%
    filter(REG_REMOVED == 1)
  
  # Separate DETAILED == NO regulations
  mh_nondetailed <- mh_cluster_ids %>%
    filter(DETAILED == "NO")
  
  # Compile all data frames
  mh_dates <- mh_dates1 %>%
    bind_rows(mh_catchlim_prep) %>%
    bind_rows(mh_removed_catchlim) %>%
    bind_rows(mh_nondetailed) %>%
    select(-adjustment_number) %>%
    mutate(START_DATE_USE = case_when(!is.na(START_DATE_USE) ~ START_DATE_USE,
                                      TRUE ~ START_DATE))
  