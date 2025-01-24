# Function to expand dates based on management status
# status may be once, seasonal, monthly recurring, weekly recurring, or daily

expand_status <- function(x, y) {
  
  # x = data frame
  # y = management type
  df = filter(x, MANAGEMENT_TYPE_USE == y,
              # only retain records that were in effect
              NEVER_IMPLEMENTED == 0, REG_REMOVED == 0)
  
  if(nrow(df) == 0) {stop("no records for that management type")}
  
  # Still need to figure out how to run if there is no data for one mstatus
  mstats = unique(df$MANAGEMENT_STATUS_USE)
  
  if('ONCE' %in% mstats){
    # One-time event
    df_one <- filter(df, MANAGEMENT_STATUS_USE == 'ONCE') %>%
      # this filter should not be needed once we clean up errors in date processing
      filter(END_DATE2 >= START_DATE2) %>%
      mutate(date_sequence = map2(START_DATE2, END_DATE2, seq, by = "1 day")) %>%
      unnest(date_sequence) %>%
      select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, MANAGEMENT_TYPE_USE, date_sequence, 
             CLUSTER, REGULATION_ID, FR_CITATION, VALUE, VALUE_UNITS, VALUE_TYPE, VALUE_RATE, MULTI_REG_VALUE) %>%
      arrange(date_sequence, desc(FR_CITATION)) %>%
      group_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, MANAGEMENT_TYPE_USE, date_sequence) %>%
      # Retain only most recent FR if there are 2 records with the same date_sequence
      mutate(num_records = n())  %>%
      mutate(action_value = case_when(num_records > 1 & MULTI_REG_VALUE != 1 & COMMON_NAME_USE == lag(COMMON_NAME_USE) ~ "slice",
                                      num_records > 1 & MULTI_REG_VALUE == 1 ~ "keep",
                                      num_records > 1 & MULTI_REG_VALUE != 1 & COMMON_NAME_USE != lag(COMMON_NAME_USE) ~ "keep",
                                      TRUE ~ "other")) %>%
      filter(!(action_value == "slice")) %>%
      rename(CLUSTER_one = "CLUSTER",
             REGULATION_ID_one = "REGULATION_ID",
             FR_CITATION_one = "FR_CITATION",
             VALUE_one = "VALUE",
             VALUE_UNITS_one = "VALUE_UNITS",
             VALUE_TYPE_one = "VALUE_TYPE",
             VALUE_RATE_one = "VALUE_RATE") %>%
      select(-num_records, -action_value)
  } else {
    df_one <- data.frame(FMP = character(), COMMON_NAME_USE = character(), REGION = character(), ZONE_USE = character(), 
                            SECTOR_USE = character(), SUBSECTOR_USE = character(), MANAGEMENT_TYPE_USE = character(),
                            date_sequence = as.Date(character()), CLUSTER_one = numeric(), REGULATION_ID_one = numeric(),
                            FR_CITATION_one = character(), VALUE_one = character(), VALUE_UNITS_one = character(), 
                            VALUE_TYPE_one = character(), VALUE_RATE_one = character())
  }
   
  if('SEASONAL' %in% mstats){
    # Seasonal
    df_seasonal <- filter(df, MANAGEMENT_STATUS_USE == 'SEASONAL') %>%
      mutate(date_sequence = map2(START_DATE2, END_DATE2, seq, by = "days")) %>%
      unnest(date_sequence) %>%
      mutate(START_YEAR_expand = year(date_sequence),
             END_YEAR_expand = year(date_sequence)) %>%
      mutate(START_DATE_EXPAND = as.Date(paste(START_YEAR_expand, START_MONTH, START_DAY, sep = "-")),
             END_DATE_EXPAND = as.Date(paste(END_YEAR_expand, END_MONTH, END_DAY, sep = "-"))) %>%
      # Create START_DATE_EXPAND2 & END_DATE_EXPAND2 for cases where the duration of the closure goes from the end of the year to the beginning of the enxt year
      # Create START_DATE_EXPAND_FINAL & END_DATE_EXPAND_FINAL to choose the correct START_DATE_EXPAND and END_DATE_EXPAND for each set of circumstances
      mutate(START_DATE_EXPAND2 = case_when(START_DATE_EXPAND > END_DATE_EXPAND ~ START_DATE_EXPAND - lubridate::years(1),
                                            TRUE ~ START_DATE_EXPAND),
             END_DATE_EXPAND2 = case_when(START_DATE_EXPAND > END_DATE_EXPAND ~ END_DATE_EXPAND + lubridate::years(1), 
                                          TRUE ~ END_DATE_EXPAND),
             START_DATE_EXPAND_FINAL = case_when(START_DATE_EXPAND > END_DATE_EXPAND & date_sequence <= START_DATE_EXPAND ~ START_DATE_EXPAND2,
                                                 TRUE ~ START_DATE_EXPAND),
             END_DATE_EXPAND_FINAL = case_when(START_DATE_EXPAND > END_DATE_EXPAND & date_sequence >= START_DATE_EXPAND & date_sequence >= END_DATE_EXPAND ~ END_DATE_EXPAND2,
                                               TRUE ~ END_DATE_EXPAND)) %>%
      # Remove date_sequence records outside expand range
      filter(date_sequence >= START_DATE_EXPAND_FINAL,
             END_DATE_EXPAND_FINAL >= date_sequence) %>%
      select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, MANAGEMENT_TYPE_USE, date_sequence,  
             CLUSTER, REGULATION_ID, FR_CITATION, VALUE, VALUE_UNITS, VALUE_TYPE, VALUE_RATE) %>%
      arrange(date_sequence, desc(FR_CITATION)) %>%
      group_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, MANAGEMENT_TYPE_USE, date_sequence) %>%
      # Retain only most recent FR if there are 2 records with the same date_sequence
      slice(1) %>%
      rename(CLUSTER_seasonal = "CLUSTER",
             REGULATION_ID_seasonal = "REGULATION_ID",
             FR_CITATION_seasonal = "FR_CITATION",
             VALUE_seasonal = "VALUE",
             VALUE_UNITS_seasonal = "VALUE_UNITS",
             VALUE_TYPE_seasonal = "VALUE_TYPE",
             VALUE_RATE_seasonal = "VALUE_RATE")
  } else {
    df_seasonal <- data.frame(FMP = character(), COMMON_NAME_USE = character(), REGION = character(), ZONE_USE = character(), 
                            SECTOR_USE = character(), SUBSECTOR_USE = character(), MANAGEMENT_TYPE_USE = character(),
                            date_sequence = as.Date(character()), CLUSTER_seasonal = numeric(), REGULATION_ID_seasonal = numeric(),
                            FR_CITATION_seasonal = character(), VALUE_seasonal = character(), VALUE_UNITS_seasonal = character(), 
                            VALUE_TYPE_seasonal = character(), VALUE_RATE_seasonal = character())
  }
  
  if('MONTHLY RECURRING' %in% mstats){
    # Monthly
    df_monthly <- filter(df, MANAGEMENT_STATUS == 'MONTHLY RECURRING') %>%
      mutate(date_sequence = map2(START_DATE2, END_DATE2, seq, by = "days")) %>%
      unnest(date_sequence) %>%
      # Extract year from date_sequence
      mutate(START_YEAR_expand = year(date_sequence),
             END_YEAR_expand = year(date_sequence),
             # Create START_MONTH_expand and END_MONTH_expand to eventually create the dates that are the bounds of which expansion dates should still be included
             # The conditions address cases where the monthly recurring goes from the middle of one month to the start of next and eventually crosses year boundaries (Gulf red snapper - 15th to the 1st of the following month or 10th to the 1st of the following month)
             # If the END_DAY_USE is a smaller value than the START_DAY_USE and the day of the date_sequence is a smaller value than the START_DAY_USE and the month of the date_sequence is not January, 
             # then the START_MONTH_expand should be one month less than the month of the date_sequence
             # If the END_DAY_USE is a smaller value than the START_DAY_USE and the day of the date_sequence is a smaller value than the START_DAY_USE but the month of the date_sequence is January,
             # then the START_MONTH_expand should be December
             # Otherwise, the START_MONTH_EXPAND should reflect the month of the date_sequence
             START_MONTH_expand = case_when(END_DAY_USE < START_DAY_USE & day(date_sequence) < START_DAY_USE & month(date_sequence) != 1 ~ month(date_sequence) - 1,
                                            END_DAY_USE < START_DAY_USE & day(date_sequence) < START_DAY_USE & month(date_sequence) == 1 ~ 12,
                                            TRUE ~ month(date_sequence)),
             # The conditions address cases where the monthly recurring goes from the middle of one month to the start of the next and eventually crosses year boundaries (Gulf red snapper - 15th to the 1st of the following month or 10th to the 1st of the following month)
             # If the END_DAY_USE is a smaller value than the START_DAY_USE and the day of the date_sequence is a smaller value than the START_DAY_USE and the month of the date_sequence is not January
             # then the END_MONTH_expand should be the month of the date_sequence
             # If the END_DAY_USE is a smaller value than the START_DAY_USE and the day of the date_sequence is greater than or equal to the START_DAY_USE value and the month of the date_sequence + 1 month is less than or equal to 12 (December), 
             # then the END_MONTH_expand should be the month of the date_sequence plus one month
             # If the END_MONTH_expand value is smaller than the START_DAY_USE and the day of the date_sequence is greater than or equal to the START_DAY_USE and the month of the date_sequence + 1 is equal to 13,
             # then the END_MONTH_expand should be January
             # Otherwise, the month of the date_sequence should be used
             END_MONTH_expand = case_when(END_DAY_USE < START_DAY_USE & day(date_sequence) < START_DAY_USE & month(date_sequence) != 1 ~ month(date_sequence),
                                          END_DAY_USE < START_DAY_USE & day(date_sequence) >= START_DAY_USE & month(date_sequence) + 1 <= 12 ~ month(date_sequence) + 1,
                                          END_DAY_USE < START_DAY_USE & day(date_sequence) >= START_DAY_USE & month(date_sequence) + 1 == 13 ~ 1,
                                          TRUE ~ month(date_sequence)),
             # Adjust END_YEAR_expand to address cases where the monthly recurring goes from the middle of one month to the start of the next and eventually crosses year boundaries (Gulf red snapper - 15th to the 1st of the following month or 10th to the 1st of the following month)
             # If the day of the date_sequence is greater than or equal to the START_DAY_USE and the END_MONTH is January & the month of the date_sequence is December, then the END_YEAR_expand should be changed to the following year
             # If the day of the date_sequence is less than the START_DAY_USE and the END_MONTH is January and the month of the date_sequence is December, then use the START_YEAR_expand
             # Otherwise, use the END_YEAR_expand
             END_YEAR_expand = case_when(day(date_sequence) >= START_DAY_USE & END_MONTH == 1 & month(date_sequence) == 12 ~ END_YEAR_expand + 1,
                                         day(date_sequence) < START_DAY_USE & END_MONTH == 1 & month(date_sequence) == 12 ~ START_YEAR_expand,
                                         TRUE ~ END_YEAR_expand)) %>%
      # only include months within the start/end month range
      # when monthly recurring, the start/end month range refers to the first and last month where the recurring closure applies
      # i.e. start month = 2 and end month = 11
      # mean recurring closures start in Feb and end in Nov
      # can't use END_MONTH_expand in filter for cases when the recurring closures spans 2 months (i.e. 15th -1st)
      #filter(START_MONTH_expand >= START_MONTH &
      #        START_MONTH_expand <= END_MONTH) %>%
      mutate(START_DATE_EXPAND = as.Date(paste(START_YEAR_expand, START_MONTH_expand, START_DAY_USE, sep = "-")),
             END_DATE_EXPAND = as.Date(paste(END_YEAR_expand, END_MONTH_expand, END_DAY_USE, sep = "-"))) %>%
      # Remove date_sequence records outside expand range
      filter(date_sequence == START_DATE_EXPAND |
               date_sequence == END_DATE_EXPAND |
               date_sequence > START_DATE_EXPAND & date_sequence < END_DATE_EXPAND) %>%
      select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, MANAGEMENT_TYPE_USE, date_sequence,  
             CLUSTER, REGULATION_ID, FR_CITATION, VALUE, VALUE_UNITS, VALUE_TYPE, VALUE_RATE) %>%
      arrange(date_sequence, desc(FR_CITATION)) %>%
      group_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, MANAGEMENT_TYPE_USE, date_sequence) %>%
      # Retain only most recent FR if there are 2 records with the same date_sequence
      slice(1) %>%
      rename(CLUSTER_monthly = "CLUSTER",
             REGULATION_ID_monthly = "REGULATION_ID",
             FR_CITATION_monthly = "FR_CITATION",
             VALUE_monthly = "VALUE",
             VALUE_UNITS_monthly = "VALUE_UNITS",
             VALUE_TYPE_monthly = "VALUE_TYPE",
             VALUE_RATE_monthly = "VALUE_RATE")
  } else {
    df_monthly <- data.frame(FMP = character(), COMMON_NAME_USE = character(), REGION = character(), ZONE_USE = character(), 
                            SECTOR_USE = character(), SUBSECTOR_USE = character(), MANAGEMENT_TYPE_USE = character(),
                            date_sequence = as.Date(character()), CLUSTER_monthly = numeric(), REGULATION_ID_monthly = numeric(),
                            FR_CITATION_monthly = character(), VALUE_monthly = character(), VALUE_UNITS_monthly = character(), 
                            VALUE_TYPE_monthly = character(), VALUE_RATE_monthly = character())
  }
  
  if('WEEKLY RECURRING' %in% mstats){
    # Weekly
    df_weekly <- filter(df, MANAGEMENT_STATUS_USE == 'WEEKLY RECURRING') %>%
      mutate(date_sequence = map2(START_DATE2, END_DATE2, seq, by = "days")) %>%
      unnest(date_sequence) %>%
      mutate(START_YEAR_expand = year(date_sequence),
             END_YEAR_expand = year(date_sequence)) %>%
      # function wday formats at Mon, Tue, etc.
      mutate(Expand_day_of_week = wday(date_sequence, label = TRUE)) %>%
      # need to get expanded days of the week to match
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
          TRUE ~ END_DAY_OF_WEEK_USE)) %>%
      # Retain on records within expand range
      filter(Expand_day_of_week >= START_DAY_OF_WEEK_EXPAND |
               Expand_day_of_week <= END_DAY_OF_WEEK_EXPAND) %>%
      select(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, MANAGEMENT_TYPE_USE, date_sequence,  
             CLUSTER, REGULATION_ID, FR_CITATION, VALUE, VALUE_UNITS, VALUE_TYPE, VALUE_RATE) %>%
      arrange(date_sequence, desc(FR_CITATION)) %>%
      group_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, MANAGEMENT_TYPE_USE, date_sequence) %>%
      # Retain only most recent FR if there are 2 records with the same date_sequence
      slice(1) %>%
      rename(CLUSTER_weekly = "CLUSTER",
             REGULATION_ID_weekly = "REGULATION_ID",
             FR_CITATION_weekly = "FR_CITATION",
             VALUE_weekly = "VALUE",
             VALUE_UNITS_weekly = "VALUE_UNITS",
             VALUE_TYPE_weekly = "VALUE_TYPE",
             VALUE_RATE_weekly = "VALUE_RATE")
  } else {
    df_weekly <- data.frame(FMP = character(), COMMON_NAME_USE = character(), REGION = character(), ZONE_USE = character(), 
                               SECTOR_USE = character(), SUBSECTOR_USE = character(), MANAGEMENT_TYPE_USE = character(),
                               date_sequence = as.Date(character()), CLUSTER_weekly = numeric(), REGULATION_ID_weekly = numeric(),
                               FR_CITATION_weekly = character(), VALUE_weekly = character(), VALUE_UNITS_weekly = character(), 
                               VALUE_TYPE_weekly = character(), VALUE_RATE_weekly = character())
  }
  
  # Daily - only applies to trip limits
  
  # Join in Wide Form
  # First clean up duplicates
  # Then limit variables and rename
  # Then create final VALUE fields based on all status events
  combine <- df_one %>%
    full_join(df_weekly,
              by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, MANAGEMENT_TYPE_USE, date_sequence)) %>%
    full_join(df_seasonal,
              by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, MANAGEMENT_TYPE_USE, date_sequence)) %>%
    full_join(df_monthly,
              by = join_by(FMP, COMMON_NAME_USE, REGION, ZONE_USE, SECTOR_USE, SUBSECTOR_USE, MANAGEMENT_TYPE_USE, date_sequence)) %>%
    # Remove records from empty dataframes
    filter(!is.na(FMP)) %>%
    # create final VALUE fields 
    ungroup() %>% 
    rowwise() %>%
    # For MANAGEMENT_TYPE_USE == "CLOSURE": On a specific date_sequence, if the most recent FR_CITATION applies to multiple closure types (i.e., one time closure and monthly recurring)
    # Then maintain the more restrictive value (CLOSE)
    # Otherwise, the most recent FR_CITATION should be maintained with associated value fields
    mutate(max_citation = pmax(FR_CITATION_one, FR_CITATION_seasonal, FR_CITATION_weekly, FR_CITATION_monthly, na.rm = TRUE),
           matched_values = list(c(
             if (isTRUE(FR_CITATION_one == max_citation)) VALUE_one else NULL,
             if (isTRUE(FR_CITATION_seasonal == max_citation)) VALUE_seasonal else NULL,
             if (isTRUE(FR_CITATION_weekly == max_citation)) VALUE_weekly else NULL,
             if (isTRUE(FR_CITATION_monthly == max_citation)) VALUE_monthly else NULL)),
           final_value = case_when(
             MANAGEMENT_TYPE_USE == "CLOSURE" & "CLOSE" %in% unlist(matched_values) ~ "CLOSE",
             TRUE ~ unlist(matched_values)[1]),
             FR_CITATION = case_when(
               final_value == VALUE_one ~ FR_CITATION_one,
               final_value == VALUE_seasonal ~ FR_CITATION_seasonal,
               final_value == VALUE_weekly ~ FR_CITATION_weekly,
               final_value == VALUE_monthly ~ FR_CITATION_monthly,
               TRUE ~ max_citation),
               VALUE = final_value,
               VALUE_UNITS = case_when(FR_CITATION == FR_CITATION_one ~ VALUE_UNITS_one,
                                       FR_CITATION == FR_CITATION_weekly ~ VALUE_UNITS_weekly,
                                       FR_CITATION == FR_CITATION_seasonal ~ VALUE_UNITS_seasonal,
                                       FR_CITATION == FR_CITATION_monthly ~ VALUE_UNITS_monthly),
               VALUE_TYPE = case_when(FR_CITATION == FR_CITATION_one ~ VALUE_TYPE_one,
                                      FR_CITATION == FR_CITATION_weekly ~ VALUE_TYPE_weekly,
                                      FR_CITATION == FR_CITATION_seasonal ~ VALUE_TYPE_seasonal,
                                      FR_CITATION == FR_CITATION_monthly ~ VALUE_TYPE_monthly),
               VALUE_RATE = case_when(FR_CITATION == FR_CITATION_one ~ VALUE_RATE_one,
                                      FR_CITATION == FR_CITATION_weekly ~ VALUE_RATE_weekly,
                                      FR_CITATION == FR_CITATION_seasonal ~ VALUE_RATE_seasonal,
                                      FR_CITATION == FR_CITATION_monthly ~ VALUE_RATE_monthly)) %>%
               ungroup()
  
  return(combine)
  
}