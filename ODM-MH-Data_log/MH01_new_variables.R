# Script 1
# Create new variables

# Overview: ####
  # Expand records with SECTOR = ALL to be COMMERCIAL and RECREATIONAL
  # Add "DETAILED" YES/NO field (from Google Sheets) based on MANAGEMENT_TYPE
  # Translate from old ZONE names to new ZONE names (from Google Sheets)
  # Create various new variables for processing

# Expand records with SECTOR = ALL to be COMMERCIAL and RECREATIONAL ####
# Results in mh_sect_expanded data frame 
mh_sect_expanded <- mh_cleaned %>%
  # CREATE: SECTOR_USE variable  
  # Rename "ALL" records to 'RECREATIONAL,COMMERCIAL'
  mutate(SECTOR_USE = case_when(MANAGEMENT_CATEGORY != "CATCH LIMITS" & SECTOR == 'ALL' ~ 'RECREATIONAL,COMMERCIAL',
                                TRUE ~ SECTOR)) %>%
  # Expand SECTOR_USE at the commas
  separate_rows(SECTOR_USE)

# Add "DETAILED" YES/NO field (from Google Sheets) based on MANAGEMENT_TYPE ####
# Read in file outlining whether a MANAGEMENT_TYPE is detailed (Y/N)
detailed_xref <- read.csv(here('ODM-MH-Data_log', 'data/raw', "mtype_detailed_xref.csv"),
                          stringsAsFactors = FALSE,
                          fileEncoding = 'Windows-1252') %>%
  select(-MANAGEMENT_CATEGORY)

  # CHECK: Run crosschecks to assess if all MH MANAGEMENT_TYPE classifications have been accounted for 
  # Are all MH MANAGEMENT_TYPE classifications in the Google Sheet table (BLANK if yes)?
  unique(mh_sect_expanded$MANAGEMENT_TYPE)[unique(mh_sect_expanded$MANAGEMENT_TYPE) %in% detailed_xref$MANAGEMENT_TYPE == FALSE]
  # Are all Google Sheet MANAGEMENT_TYPE classifications in MH (BLANK if yes)?
  detailed_xref$MANAGEMENT_TYPE[detailed_xref$MANAGEMENT_TYPE %in% mh_sect_expanded$MANAGEMENT_TYPE == FALSE]

  # Edit mh_sect_expanded to result in mh_sect_expanded2 data frame
  # CREATE: the variables of O_COMMON_NAME, O_SPECIES_AGGREGATE, O_SPECIES_GROUP to retain original species information
  # CREATE: the variables of SPP_TYPE and SPP_NAME to consolidate the information from O_COMMON_NAME, O_SPECIES_AGGREGATE, O_SPECIES_GROUP 
  # REFORMAT: move species common name, species aggregate name, and species group name into single field (SPP_NAME). This field should never be null
  mh_sect_expanded2 <- mh_sect_expanded %>%
    # CREATE: DETAILED field (YES/NO) to indicate whether or not the MANAGEMENT_TYPE is detailed 
    # Detailed MANAGEMENT_TYPE classifications are those regulations that are able to be captured more thoroughly within the fields of the database.
    left_join(detailed_xref, by = "MANAGEMENT_TYPE") %>%
    # CREATE: the variables of O_COMMON_NAME, O_SPECIES_AGGREGATE, O_SPECIES_GROUP to retain the original species information from the raw data
    mutate(O_COMMON_NAME = COMMON_NAME,
           O_SPECIES_AGGREGATE = SPECIES_AGGREGATE,
           O_SPECIES_GROUP = SPECIES_GROUP) %>%
    # CREATE: the variables of SPP_TYPE and SPP_NAME and transpose the species information contained in the O_COMMON_NAME, O_SPECIES_AGGREGATE, O_SPECIES_GROUP fields.
    # The SPP_TYPE field indicates whether the record relates to a SPECIES_AGGREGATE, SPECIES_GROUP, or individual species (COMMON_NAME)
    # The SPP_NAME field indicates the specific aggregate, group, or species name to which the record applies 
    pivot_longer(cols = c(COMMON_NAME, SPECIES_AGGREGATE, SPECIES_GROUP), names_to = "SPP_TYPE", values_to = "SPP_NAME") %>%
    # Remove records where SPP_NAME is null 
    filter(!is.na(SPP_NAME))

  # CHECK: that the number of REGULATION_ID and records remains the same between the mh_sect_expanded and mh_sect_expanded2 data frames
  n_distinct(mh_sect_expanded$REGULATION_ID) == n_distinct(mh_sect_expanded2$REGULATION_ID)
  nrow(mh_sect_expanded) == nrow(mh_sect_expanded2)

# Translate from old ZONE names to new ZONE names ####
# Read in file that outlines new ZONE names for all FMPs
# These ZONEs were cleaned up for consistency
area_xref <- read.csv(here('ODM-MH-Data_log', 'data/raw', "zone_name_xref.csv"),
                      stringsAsFactors = FALSE,
                      fileEncoding = 'Windows-1252') %>%
  # Create single variable for ZONE_USE
    mutate(ZONE_USE = case_when(NEW_ZONE_NAME == "" ~ ZONE,
                                TRUE ~ NEW_ZONE_NAME)) %>%
    select(ZONE, ZONE_USE)

  # CHECK: Run crosschecks to compare ZONE names between mh_sect_expanded and Google Sheet
  # Are all MH ZONE names in Google Sheet (BLANK if yes)
  unique(mh_sect_expanded2$ZONE)[unique(mh_sect_expanded2$ZONE) %in% area_xref$ZONE == FALSE]
  # Are all Google Sheet ZONE names in MH (BLANK if yes)?
  area_xref$ZONE[area_xref$ZONE %in% mh_sect_expanded2$ZONE == FALSE]
  # CHECK: How many zones were consolidated after the cleanup
  n_distinct(mh_sect_expanded2$ZONE)
  n_distinct(area_xref$ZONE_USE)

  # Standardize ZONE names
  # Results in mh_setup data frame
  # CREATE: variable of ZONE_USE to incorporate NEW_ZONE_NAME and create standard ZONE names  
  # This step is performed after the sector expansion and before the species expansion
  # because the species list contains duplicates which are addressed later in the mh_spp_expansion.R script
  mh_setup <- mh_sect_expanded2 %>%
    left_join(area_xref, by = c("ZONE" = "ZONE")) 
  
# Create various new variables for processing ####
# Results in the mh_newvar data frame
# CREATE: the variables of vol, page, MANAGEMENT_TYPE_USE, ADJUSTMENT, MANAGEMENT_STATUS_USE, REG_REMOVED
# CREATE: the variable of STATUS TYPE which categorizes the MANAGEMENT_STATUS_USE as GENERAL or COMPLEX 
mh_newvar <- mh_setup %>%
  # CREATE: vol and page but pulling out the volume and page number as separate fields from the FR_CITATION 
  # (currently a warning appears because page is NA for "81 FR 33150 B", but once fixed as a bug the warning should go away)
  # Volume and page are essential pieces to include for sorting
  mutate(vol = as.numeric(sub(" FR.*", "", FR_CITATION)),
         page = as.numeric(sub(".*FR ", "", FR_CITATION)),
         # CREATE: ADJUSTMENT variable to flag when the MANAGEMENT_TYPE contains the word "ADJUSTMENT" and remove "ADJUSTMENT" from the MANAGEMENT_TYPE name
         # ADJUSTMENT records are never redundant
         # Added " MANAGEMENT_CATEGORY == "TEMPORAL CONTROLS" & month(INEFFECTIVE_DATE) == 12 & day(INEFFECTIVE_DATE) == 31 ~ 0" due to error in CLUSTER 280
         # Without this addition, a closure within CLUSTER 280 that ended at the end of the calendar year was being flagged as an adjustment instead of ceasing to exist.
         ADJUSTMENT = case_when(str_detect(MANAGEMENT_TYPE, "ADJUSTMENT") ~ 1,
                                MANAGEMENT_CATEGORY == "TEMPORAL CONTROLS" & month(INEFFECTIVE_DATE) == 12 & day(INEFFECTIVE_DATE) == 31 ~ 0,
                                MANAGEMENT_TYPE == "REOPENING" & !is.na(INEFFECTIVE_DATE) ~ 1,
                                TRUE ~ 0),
         MANAGEMENT_TYPE_USE = case_when(str_detect(MANAGEMENT_TYPE, "ADJUSTMENT") ~ str_replace(MANAGEMENT_TYPE, " ADJUSTMENT", ""),
                                         TRUE ~ MANAGEMENT_TYPE),
         # Rename records with the MANAGEMENT_TYPE of REOPENING to the MANAGEMENT_TYPE of CLOSURE and add OPEN or CLOSED to the VALUE field
         # Although the MANAGEMENT_TYPE will be different from the raw data (since the raw data stays true to the FR Language)
         # this will assist in processing dates to accurately capture the time series of openings/closures in the fishery
         MANAGEMENT_TYPE_USE = case_when(MANAGEMENT_TYPE == "REOPENING" ~ "CLOSURE",
                                         TRUE ~ MANAGEMENT_TYPE_USE),
         VALUE= case_when(MANAGEMENT_TYPE == "CLOSURE" ~ "CLOSE",
                          MANAGEMENT_TYPE == "REOPENING" ~ "OPEN",
                          TRUE ~ VALUE),
         # CREATE: MANAGEMENT_STATUS_USE variable by transposing the MANAGEMENT_STATUS values 
         # Replace any NA values under MANAGEMENT_STATUS_USE as ONCE because  NA values can be complicated to process
         # Both MANAGEMENT_STATUS of NA and ONCE are meant to be processed the same way
         MANAGEMENT_STATUS_USE = case_when(is.na(MANAGEMENT_STATUS) ~ 'ONCE',
                                           TRUE ~ MANAGEMENT_STATUS),
         # CREATE: the variable of STATUS_TYPE with the values of SIMPLE or COMPLEX
         # A STATUS_TYPE of SIMPLE indicates a MANAGEMENT_STATUS_USE of ONCE
         # A STATUS_TYPE of COMPLEX indicates a MANAGEMENT_STATUS_USE that is RECURRING (SEASONAL, WEEKLY RECURRING, MONTHLY RECURRING, DAILY)
         STATUS_TYPE = case_when(MANAGEMENT_STATUS_USE == "ONCE" ~ "SIMPLE",
                                 MANAGEMENT_STATUS_USE %in% c("SEASONAL", "WEEKLY RECURRING", "MONTHLY RECURRING", "DAILY") ~ "RECURRING",
                                 TRUE ~ "COMPLEX"),
         # CREATE: the variable of REG_REMOVED to indicate when a regulation is "turned off"
         # A regulation is "turned off when the EFFECTIVE_DATE is equal to the INEFFECTIVE_DATE
         REG_REMOVED = case_when(EFFECTIVE_DATE == INEFFECTIVE_DATE ~ 1, TRUE ~ 0),
         # CREATE: the variables of GENERAL and COMPLEX to flag when a regulation has a STAUS_TYPE of SIMPLE or COMPLEX, respectively
         GENERAL = case_when(STATUS_TYPE == "SIMPLE" & is.na(VALUE) ~ 1, TRUE ~ 0),
         COMPLEX = case_when(STATUS_TYPE == "COMPLEX" ~ 1, TRUE ~ 0),
         # CREATE: variables to outline recurring start and end dates (this is to avoid the dual purpose of these fields)
         # CREATE: START_DAY_RECURRING = case_when(STATUS_TYPE == "RECURRING" ~ START_DAY),
         # CREATE: START_MONTH_RECURRING = case_when(STATUS_TYPE == "RECURRING" ~ START_MONTH),
         # CREATE: START_TIME_RECURRING = case_when(STATUS_TYPE == "RECURRING" ~ START_TIME),
         # CREATE: START_DAY_OF_WEEK_RECURRING = case_when(STATUS_TYPE == "RECURRING" ~ as.character(START_DAY_OF_WEEK)),
         # CREATE: END_DAY_RECURRING = case_when(STATUS_TYPE == "RECURRING" ~ END_DAY),
         # CREATE: END_MONTH_RECURRING = case_when(STATUS_TYPE == "RECURRING" ~ END_MONTH),
         # CREATE: END_TIME_RECURRING = case_when(STATUS_TYPE == "RECURRING" ~ END_TIME),
         # CREATE: END_DAY_OF_WEEK_RECURRING = case_when(STATUS_TYPE == "RECURRING" ~ END_DAY_OF_WEEK),
         
         # Format start and end day of week as ordered factors
         START_DAY_OF_WEEK = factor(START_DAY_OF_WEEK, levels = c("MONDAY", "TUESDAY", "WEDNESDAY", 
                                                  "THURSDAY", "FRIDAY", "SATURDAY", "SUNDAY"),
                                    ordered = TRUE),
         END_DAY_OF_WEEK = factor(END_DAY_OF_WEEK, levels = c("MONDAY", "TUESDAY", "WEDNESDAY", 
                                                                  "THURSDAY", "FRIDAY", "SATURDAY", "SUNDAY"),
                                    ordered = TRUE),
         # CREATE: START_DATE from the START_DAY, START_MONTH, and START_YEAR fields
         # The START_DATE field is only created using START_DAY, START_MONTH, and START_YEAR when
         # all three start day, month, year fields are provided and the management status is ONCE
         START_DATE = case_when(MANAGEMENT_STATUS_USE == "ONCE" &
                                  !is.na(START_DAY) &
                                  !is.na(START_MONTH) &
                                  !is.na(START_YEAR) ~ as.Date(paste(START_MONTH, START_DAY, START_YEAR, sep = "/"), "%m/%d/%Y"),
                                TRUE ~ EFFECTIVE_DATE),
         # Adjust the start date if its after the effective date so the start date = effective date
         START_DATE = case_when(START_DATE < EFFECTIVE_DATE ~ EFFECTIVE_DATE,
                                TRUE ~ START_DATE),
         # When the START_TIME is equal to "11:59:00 PM", the START_DATE should be pushed ahead by one day since
         # the regulation will be in effect for the entirety of that day.
         START_DATE = case_when(START_TIME == "11:59:00 PM" ~ START_DATE + 1,
                                TRUE ~ START_DATE),
         # Adjust the start day, time, and day of the week accordingly so when those fields are used for recurring 
         # the individual fields will be consistent
         START_DAY_USE = case_when(START_TIME == "11:59:00 PM" & !is.na(START_DAY) ~ START_DAY + 1,
                                   TRUE ~ START_DAY),
         # For start time, we remove the start time when "11:59:00 PM" because a null assumes the full day
         START_TIME_USE = case_when(START_TIME != "11:59:00 PM" ~ START_TIME),
         START_DAY_OF_WEEK_USE = case_when(START_TIME == "11:59:00 PM" & !is.na(START_DAY_OF_WEEK) ~ as.numeric(START_DAY_OF_WEEK) + 1,
                                           TRUE ~ as.numeric(START_DAY_OF_WEEK)),
         
         # CREATE: END_DATE from the END_DAY, END_MONTH, and END_YEAR fields
         # The END_DATE field is only created using END_DAY, END_MONTH, and END_YEAR when
         # all end year, month, day fields are provided and and the management status is ONCE
         END_DATE = case_when(MANAGEMENT_STATUS_USE == "ONCE" &
                                !is.na(END_DAY) &
                                !is.na(END_MONTH) &
                                !is.na(END_YEAR) ~ as.Date(paste(END_MONTH, END_DAY, END_YEAR, sep = "/"), "%m/%d/%Y"),
                              TRUE ~ INEFFECTIVE_DATE),
         # Added condition on 2/2/24 because of REG_ID 762
         END_DATE = case_when(MANAGEMENT_STATUS_USE == "ONCE" &
                                !is.na(END_DAY) &
                                !is.na(END_MONTH) &
                                !is.na(END_YEAR) &
                                as.Date(paste(END_MONTH, END_DAY, END_YEAR, sep = "/"), "%m/%d/%Y") > INEFFECTIVE_DATE ~ INEFFECTIVE_DATE,
                              TRUE ~ END_DATE),
         # For records with an END_TIME of "12:01:00 AM", the END_DATE should be reverted to one day prior.
         # This will infer that the regulation remained in place through the end of that day and not one minute into the next day.
         END_DATE = case_when(END_TIME == "12:01:00 AM" ~ END_DATE - 1,
                              TRUE ~ END_DATE),
         # Adjust the end day, time, and day of the week accordingly  
         # When end time is 12:01, use the day of end date because the year of February already factored in to determine if its the 28th or 29th
         END_DAY_USE = case_when(END_TIME == "12:01:00 AM" & !is.na(END_DATE) ~ as.numeric(day(END_DATE)),
                             TRUE ~ END_DAY),
         END_MONTH_USE = case_when(END_TIME == "12:01:00 AM" & !is.na(END_DATE) ~ as.numeric(month(END_DATE)),
                                 TRUE ~ END_MONTH),
         END_YEAR_USE = case_when(END_TIME == "12:01:00 AM" & END_MONTH == 1 & END_DAY == 1 ~ END_YEAR - 1,
                                  TRUE ~ END_YEAR),
         # Retain end time of 12:01 only for recurring regulations where the end day is the 1st
         # Otherwise remove 12:01 from end time or use the reported end time
         END_TIME_USE = case_when(END_TIME == "12:01:00 AM" & STATUS_TYPE == "RECURRING" & END_DAY == 1 ~ END_TIME,
                                  # Single regulation (REG_ID = 80) where end time is 11:59 PM
                                  END_TIME != "12:01:00 AM" & END_TIME != "11:59:00 PM" ~ END_TIME),
         # When the END_TIME is listed as "12:01:00 AM" and the end day of the week is not missing then revert to one day prior
         # TO be consistent, still use the condition when end day does not equal 1, but as of 12/30/2022 there were no end day of the weeks with an end day of 1
         END_DAY_OF_WEEK_USE = case_when(END_TIME == "12:01:00 AM" & END_DAY != 1 ~ as.numeric(END_DAY_OF_WEEK) - 1,
                                         TRUE ~ as.numeric(END_DAY_OF_WEEK)),
         # Format start and end day of week use to deal with 0 and 8 of 7 level factor
         START_DAY_OF_WEEK_USE = case_when(START_DAY_OF_WEEK_USE == 8 ~ 1,
                                           TRUE ~ START_DAY_OF_WEEK_USE),
         END_DAY_OF_WEEK_USE = case_when(END_DAY_OF_WEEK_USE == 0 ~ 7,
                                         TRUE ~ END_DAY_OF_WEEK_USE),
         # Format start and end day of week use as ordered factors
         START_DAY_OF_WEEK_USE = recode_factor(START_DAY_OF_WEEK_USE,
                                               `1` = "MONDAY", `2` = "TUESDAY", `3` = "WEDNESDAY", 
                                               `4` = "THURSDAY", `5` = "FRIDAY", `6` = "SATURDAY", `7` = "SUNDAY"),
         END_DAY_OF_WEEK_USE = recode_factor(END_DAY_OF_WEEK_USE,
                                             `1` = "MONDAY", `2` = "TUESDAY", `3` = "WEDNESDAY", 
                                             `4` = "THURSDAY", `5` = "FRIDAY", `6` = "SATURDAY", `7` = "SUNDAY"))
  