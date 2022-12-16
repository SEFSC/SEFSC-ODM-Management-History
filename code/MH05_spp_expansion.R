# Script 5
# Species Expansion

# Overview: ####
  # Prepare species related information for expansion
  # Perform expansion of species information for SPECIES_GROUP, SPECIES_AGGREGATE, and COMMON_NAME ALL selections for single records
  # Perform expansion of species information for SPECIES_GROUP, SPECIES_AGGREGATE, and COMMON_NAME ALL selections for multiple record
  # Join data frames that include species expansion information with data frame of non-DETAILED records
  # Adjust dates based on the dates that species were added and/or removed 
  # Export the data

# Prepare species related information for expansion ####

  # CHECK: species tables -- 5 duplicates still exist because they are species that were a part of an SPECIES_AGGREGATE, were removed from the SPECIES_AGGREGATE, and were later reinstated
  count_sp_info_use <- sp_info_use %>%
    group_by(FMP, SPP_TYPE, SPP_NAME, COMMON_NAME_USE) %>%
    summarize(N = n())

  # Break up species table into two tables
  # Table 1 - Cases when only a single record exists for a species in SPECIES_AGGREGATE or SPECIES_GROUP
  # Table 2 - Cases when more than one record exists for a species in SPECIES_AGGREGATE or SPECIES_GROUP
  sp_info_use_s <- sp_info_use %>%
    left_join(count_sp_info_use, by = c("FMP", "SPP_TYPE", "SPP_NAME", "COMMON_NAME_USE")) %>%
    filter(N == 1) %>% 
    select(-c(N, FMP_GROUP_ID, SUBGRP_NAME))
  sp_info_use_m <- sp_info_use %>%
    left_join(count_sp_info_use, by = c("FMP", "SPP_TYPE", "SPP_NAME", "COMMON_NAME_USE")) %>%
    filter(N == 2) %>% 
    select(-c(N, FMP_GROUP_ID, SUBGRP_NAME))

# Perform expansion of species information for SPECIES_GROUP, SPECIES_AGGREGATE, and COMMON_NAME ALL selections for single records ####
# Results in the mh_sp_expanded_y1 data frame
# Records are only expanded for MANAGEMENT_TYPE that are considered DETAILED
# Expansion is performed for cases where there is a single record for a species in a SPECIES_AGGREGATE or SPECIES_GROUP
mh_sp_expanded_y1 <- mh_dates %>%
  # Filter to remove non-DETAILED records
  filter(DETAILED == 'YES') %>%
  # Join to species list table by SPP_NAME, SPP_TYPE, and FMP
  full_join(., sp_info_use_s, by = c("FMP", "SPP_TYPE", "SPP_NAME")) %>%
  # Remove SPECIES_GROUP and SPECIES_AGGREGATE that do not appear in the data set or are regulated by FMPs that are not in the data set (i.e., HMS)
  filter(!is.na(REGULATION_ID)) %>%
  mutate(COMMON_NAME_USE = case_when(is.na(COMMON_NAME_USE) ~ SPP_NAME,
                                     TRUE ~ COMMON_NAME_USE),
         SPECIES_ITIS_USE = case_when(is.na(SPECIES_ITIS_USE) ~ as.character(SPECIES_ITIS),
                                      TRUE ~ SPECIES_ITIS_USE)) %>%
  # Remove species expansion date variables
  select(-c(SPECIES_ITIS, SCIENTIFIC_NAME))

# Perform expansion of species information for SPECIES_GROUP, SPECIES_AGGREGATE, and COMMON_NAME ALL selections for multiple records ####
# Results in the mh_sp_expanded_y2 data frame
# Records are only expanded for DETAILED MANAGEMENT_TYPEs
# Expansion is performed for cases where there are multiple records for a species in a SPECIES_AGGREGATE or SPECIES_GROUP
mh_sp_expanded_y2 <- mh_dates %>%
  # Filter to remove non-DETAILED records
  filter(DETAILED == 'YES') %>%
  # Join to species list table by SPP_NAME, SPP_TYPE, and FMP
  full_join(., sp_info_use_m, by = c("FMP", "SPP_TYPE", "SPP_NAME")) %>%
  # Remove SPECIES_GROUP and SPECIES_AGGREGATE that do not appear in the dataset or are regulated by FMPs that are not in the dataset (i.e. HMS)
  filter(!is.na(REGULATION_ID), !is.na(ADDED_SP_DATE)) %>%
  mutate(COMMON_NAME_USE = case_when(is.na(COMMON_NAME_USE) ~ SPP_NAME,
                                     TRUE ~ COMMON_NAME_USE),
         SPECIES_ITIS_USE = case_when(is.na(SPECIES_ITIS_USE) ~ as.character(SPECIES_ITIS),
                                      TRUE ~ SPECIES_ITIS_USE)) %>%
  # Remove species expansion date variables
  select(-c(SPECIES_ITIS, SCIENTIFIC_NAME))

# Join data frames that include species expansion information with data frame of non-DETAILED records
# Results in mh_sp_expanded_n data frame
# First create data frame of only non-DETAILED records
mh_sp_expanded_n <- mh_dates %>%
  # Filter original data frame to remove DETAILED records
  filter(DETAILED == 'NO') %>%
  # mutate field names to match those used in expansion
  mutate(COMMON_NAME_USE = SPP_NAME,
         SPECIES_ITIS_USE = as.character(SPECIES_ITIS)) %>%
  select(-SPECIES_ITIS)

  # Join expanded data frames with data frame of non-DETAILED records
  mh_sp_expanded <- mh_sp_expanded_y1 %>%
    bind_rows(mh_sp_expanded_y2, mh_sp_expanded_n)

# REMOVE expanded species that are no longer a part of that group
# ADJUST START or END DATES using SPECIES ADDED and REMOVED dates

# Adjust dates based on the dates that species were added and/or removed ####
# First perform check to make sure we understand all potential situations related to species added/removed dates
# Assign numbers to each situation
# ADDED_SP_DATE indicates the date that the species was added to the SPECIES_AGGREGATE or SPECIES_GROUP
# REMOVED_SP_DATE indicates the date that the species was removed from the SPECIES_AGGREGATE or SPECIES_GROUP
chk_spp <- mh_sp_expanded %>%
  ungroup() %>%
  mutate(chk_spp = case_when(REMOVED_SP_DATE < START_DATE_USE ~ 1,
                             REMOVED_SP_DATE < END_DATE & REMOVED_SP_DATE > START_DATE_USE ~ 2,
                             ADDED_SP_DATE > START_DATE_USE & END_DATE > ADDED_SP_DATE  ~ 3,
                             ADDED_SP_DATE > START_DATE_USE & END_DATE < ADDED_SP_DATE ~ 4,
                             REMOVED_SP_DATE == START_DATE_USE ~ 5,
                             ADDED_SP_DATE > START_DATE_USE ~ 6,
                             is.na(REMOVED_SP_DATE) ~ 6,
                             ADDED_SP_DATE > START_DATE_USE & REMOVED_SP_DATE < END_DATE ~ 6,
                             ADDED_SP_DATE < START_DATE_USE & END_DATE < REMOVED_SP_DATE ~ 6,
                             ADDED_SP_DATE == START_DATE_USE & END_DATE < REMOVED_SP_DATE ~ 6,
                             ADDED_SP_DATE < START_DATE_USE & REMOVED_SP_DATE == END_DATE ~ 6,
                             ADDED_SP_DATE == START_DATE_USE & REMOVED_SP_DATE == END_DATE ~ 6,
                             TRUE ~ 0),
         # Assign associated explanation to each number
         chk_reason = case_when(chk_spp == 1 ~ 'spp removed before reg started',
                                chk_spp == 2 ~ 'spp removed before end date',
                                chk_spp == 3 ~ 'spp added after start date but within range',
                                chk_spp == 4 ~ 'spp added after start date and outside range',
                                chk_spp == 5 ~ 'spp removed same day regulation started',
                                chk_spp == 6 ~ 'spp within range',
                                chk_spp == 0 ~ 'not considered')) %>%
  #filter(chk_spp != 0) %>%
  select(CLUSTER, REGULATION_ID, MANAGEMENT_TYPE, MANAGEMENT_TYPE_USE, SECTOR_USE, SUBSECTOR_USE, ZONE_USE, SPECIES_ITIS_USE, COMMON_NAME_USE,
         EFFECTIVE_DATE, INEFFECTIVE_DATE, ADDED_SP_DATE, REMOVED_SP_DATE, START_DATE_USE, END_DATE,
         chk_spp, chk_reason)

  # CHECK: do all records fall under one of these categories
  # Any records that do not meet these cases should be investigated
  chk_spp_sum <- chk_spp %>%
    group_by(chk_spp, chk_reason) %>%
    summarize(N = n()) %>%
    mutate(resolution = case_when(chk_spp == 1 ~ 'remove',
                                  chk_spp == 2 ~ 'adjust end date',
                                  chk_spp == 3 ~ 'adjust start date',
                                  chk_spp == 4 ~ 'remove',
                                  chk_spp == 5 ~ 'remove',
                                  chk_spp == 6 ~ 'no adjustment',
                                  chk_spp == 0 ~ 'manual check'))

  # Adjust START_DATE_USE and END_DATE information based on the ADDED_SP_DATE and REMOVED_SP_DATE
  # Results in the mh_expanded data frame
  mh_expanded <- mh_sp_expanded %>%
    # CREATE: the variable of RM_SPP (remove species) to indicate that the regulation does not apply to this species does not apply
    # A regulation does not apply to a particular species when:
    # The REMOVED_SP_DATE occurs prior to the START_DATE_USE of the regulation
    # The ADDED_SP_DATE occurs after the START_DATE_USE of the regulation and the END_DATE of the regulation is prior the the ADDED_SP_DATE
    # The REMOVED_SP_DATE is equal to the START_DATE_USE of the regulation
    mutate(RM_SPP = case_when(REMOVED_SP_DATE < START_DATE_USE ~ 1,
                              ADDED_SP_DATE > START_DATE_USE & END_DATE < ADDED_SP_DATE ~ 1,
                              REMOVED_SP_DATE == START_DATE_USE ~ 1,
                              TRUE ~ 0),
           # CREATE: the variable of IMP_START_DATE (0 or 1 field) to flag cases where the START_DATE_USE of the regulation should be adjusted
           # The START_DATE_USE should be adjusted when:
           # The ADDED_SP_DATE is after the START_DATE_USE of the regulation and the END_DATE of the regulation is after the ADDED_SP_DATE
           IMP_START_DATE = case_when(ADDED_SP_DATE > START_DATE_USE & END_DATE > ADDED_SP_DATE ~ 1,
                                      TRUE ~ 0),
           # CREATE: the variable of START_DATE2 to adjust the START_DATE_USE accordingly
           # When IMP_START_DATE is flagged (= 1) the ADDED_SP_DATE should be used as the START_DATE_USE
           # Otherwise the START_DATE_USE should be used for START_DATE2
           START_DATE2 = case_when(IMP_START_DATE == 1 ~ ADDED_SP_DATE,
                                  TRUE ~ START_DATE_USE),
           # CREATE: the variable of IMP_END_DATE (0 or 1 field) to flag cases where the END_DATE of the regulation should be adjusted
           # The END_DATE should be adjusted when:
           # The REMOVED_SP_DATE is prior to the END_DATE of the regulation and the REMOVED_SP_DATE is after the START_DATE_USE of the regulation
           IMP_END_DATE = case_when(REMOVED_SP_DATE < END_DATE & REMOVED_SP_DATE > START_DATE_USE ~ 1,
                                    TRUE ~ 0),
           # CREATE: the variable of END_DATE2 to adjust the END_DATE accordingly
           # When IMP_END_DATE is flagged (= 1) the REMOVED_SP_DATE should be used as the END_DATE
           # Otherwise the END_DATE should be used for END_DATE2
           END_DATE2 = case_when(IMP_END_DATE == 1 ~ REMOVED_SP_DATE,
                                TRUE ~ END_DATE)) 

# Export data sets ####
# Export mh_expanded
#write.csv(mh_expanded, here("data", "processed", paste0("mh_expanded_", format(Sys.Date(), "%Y%m%d"), ".csv")), row.names = FALSE)

  # Subset to fully processed component of mh_expanded
  # These components will not need to be removed one processing accommodates the necessary logic
  cluster_drop <- mh_expanded %>%
    filter(MULTI_REG == 1 | 
             MANAGEMENT_STATUS_USE %in% c("DELAYED", "WITHDRAWN") |
             STATUS_TYPE == "RECURRING")
  
  mh_analysis_ready <-  mh_expanded %>%
    filter(!CLUSTER %in% cluster_drop$CLUSTER,
           NEVER_IMPLEMENTED == 0) %>%
    select(CLUSTER, REGULATION_ID, FR_CITATION, FR_URL, FMP, ACTION, ACTION_TYPE, AMENDMENT_NUMBER, ACCOUNTABILITY_MEASURE,
           MANAGEMENT_CATEGORY, MANAGEMENT_TYPE_USE, MANAGEMENT_STATUS_USE, STATUS_TYPE, DETAILED,
           JURISDICTION, SECTOR_USE, SUBSECTOR_USE, REGION,ZONE_USE, JURISDICTIONAL_WATERS,
           COMMON_NAME_USE, SPP_TYPE, SPP_NAME, SPECIES_ITIS_USE,
           ADJUSTMENT, REVERSION, 
           EFFECTIVE_DATE, INEFFECTIVE_DATE, START_DATE2, START_TIME_USE, END_DATE2, END_TIME_USE, diff_days,
           VALUE, VALUE_UNITS,VALUE_TYPE,VALUE_RATE,
           MULTI_REG, MULTI_REG_CLUSTER, REG_REMOVED, FLAG)
  
  # Export mh_analysis_ready
  #write.csv(mh_analysis_ready, here("data", "processed", paste0("mh_analysis_ready_", format(Sys.Date(), "%Y%m%d"), ".csv")), row.names = FALSE)
  