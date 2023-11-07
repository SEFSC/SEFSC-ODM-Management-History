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
  full_join(., sp_info_use_s, by = join_by("FMP", "SPP_TYPE", "SPP_NAME"), relationship = "many-to-many") %>%
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
  full_join(., sp_info_use_m, by = join_by("FMP", "SPP_TYPE", "SPP_NAME"), relationship = "many-to-many") %>%
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
  
# # Expand SPP_NAME ALL fishing year
# #mh_fy_expanded <- mh_fy3 %>%
#   # Join to species list table by SPP_NAME, SPP_TYPE, and FMP
#   full_join(., sp_info_use_s, by = c("FMP", "SPP_TYPE", "SPP_NAME")) %>%
#   # Remove records from sp_info_use_s that do not apply to fishing year
#   filter(!is.na(SECTOR_USE)) %>%
#   mutate(COMMON_NAME_USE = case_when(is.na(COMMON_NAME_USE) ~ SPP_NAME,
#                                       TRUE ~ COMMON_NAME_USE),
#           SPECIES_ITIS_USE = case_when(is.na(SPECIES_ITIS_USE) ~ as.character(SPECIES_ITIS),
#                                       TRUE ~ SPECIES_ITIS_USE)) %>%
#   ungroup() %>%
#   # Remove species expansion date variables
#   # Remove cluster variable because when SPP_NAME = 'ALL' expanded it will have a different cluster ID from species specific fishing years
#   select(-c(NEW_CLUSTER, SPECIES_ITIS, SCIENTIFIC_NAME, ADDED_SP_DATE, REMOVED_SP_DATE, SPP_TYPE, SPP_NAME)) %>%
#   group_by(FMP, REGION, SECTOR_USE, SUBSECTOR_USE, ZONE_USE, SPECIES_ITIS_USE, COMMON_NAME_USE) %>%
#   arrange(EFFECTIVE_DATE_FY) %>%
#   mutate(reg_order = rank(EFFECTIVE_DATE_FY))
# 
# # Identify changes in fishing year
# # There are 55 clusters that change fishing year at one point in time
# # Most have 1 change, but South Atlantic greater amberjack and black sea bass have 3 fishing year starts over time
# chk_change <- mh_fy_expanded %>% group_by(FMP, REGION, SECTOR_USE, SUBSECTOR_USE, ZONE_USE, SPECIES_ITIS_USE, COMMON_NAME_USE) %>% 
#   summarize(N = n()) %>% filter(N != 1)
# 
# # Structure fishing year data to wide-form for joining to the rest of MH data
# mh_fy_expanded_w <- mh_fy_expanded %>%
#   pivot_wider(names_from = reg_order, values_from = c(EFFECTIVE_DATE_FY, FY)) %>%
#   ungroup() 
# 
# # CHECK fishing year where we have known 3 changes over time
# chk <- mh_fy_expanded %>% filter(FMP == 'SNAPPER-GROUPER FISHERY OF THE SOUTH ATLANTIC REGION', SECTOR_USE == 'RECREATIONAL', 
#                                  COMMON_NAME_USE == 'AMBERJACK, GREATER', ZONE_USE == 'ALL')
# # CHECK for duplicates 
# chk_dups <- mh_fy_expanded_w %>%
#   group_by(FMP, REGION, SECTOR_USE, SUBSECTOR_USE, ZONE_USE, SPECIES_ITIS_USE, COMMON_NAME_USE) %>%
#   summarise(nrecs = n())
# if(any(chk_dups$nrecs) > 1){stop("You have duplicate records in the fishing year dataset ready to join")}
# 
# # Subset FY expansion for the join
# # Fishing year for specific subsector and zone
# mh_fy_expanded_sz <- mh_fy_expanded_w %>% filter(SUBSECTOR_USE != 'ALL', ZONE_USE != 'ALL') %>%
#   rename(EFFECTIVE_DATE_FY_1_sz = "EFFECTIVE_DATE_FY_1",
#          EFFECTIVE_DATE_FY_2_sz = "EFFECTIVE_DATE_FY_2",
#          EFFECTIVE_DATE_FY_3_sz = "EFFECTIVE_DATE_FY_3",
#          FY_1_sz = "FY_1",
#          FY_2_sz = "FY_2",
#          FY_3_sz = "FY_3")
# # Fishing year for specific zone
# mh_fy_expanded_z <- mh_fy_expanded_w %>% filter(ZONE_USE != 'ALL', SUBSECTOR_USE == 'ALL') %>%
#   rename(EFFECTIVE_DATE_FY_1_z = "EFFECTIVE_DATE_FY_1",
#          EFFECTIVE_DATE_FY_2_z = "EFFECTIVE_DATE_FY_2",
#          EFFECTIVE_DATE_FY_3_z = "EFFECTIVE_DATE_FY_3",
#          FY_1_z = "FY_1",
#          FY_2_z = "FY_2",
#          FY_3_z = "FY_3") %>%
#   select(-SUBSECTOR_USE)
# # Fishing year for all zones and subsectors for management types where species expanded
# mh_fy_expanded_a <- mh_fy_expanded_w %>% filter(ZONE_USE == 'ALL', SUBSECTOR_USE == 'ALL') %>%
#   rename(EFFECTIVE_DATE_FY_1_a = "EFFECTIVE_DATE_FY_1",
#          EFFECTIVE_DATE_FY_2_a = "EFFECTIVE_DATE_FY_2",
#          EFFECTIVE_DATE_FY_3_a = "EFFECTIVE_DATE_FY_3",
#          FY_1_a = "FY_1",
#          FY_2_a = "FY_2",
#          FY_3_a = "FY_3") %>%
#   select(-c(SUBSECTOR_USE, ZONE_USE))
# # Fishing year for all zones and subsectors for management types where species NOT expanded
# mh_fy_a <- mh_fy3 %>% 
#   filter(ZONE_USE == 'ALL', SUBSECTOR_USE == 'ALL', SPP_NAME == 'ALL') %>%
#   ungroup() %>%
#   select(FMP, REGION, SECTOR_USE, SUBSECTOR_USE, ZONE_USE, SPP_NAME, EFFECTIVE_DATE_FY, FY) %>%
#   group_by(FMP, REGION, SECTOR_USE, SUBSECTOR_USE, ZONE_USE, SPP_NAME) %>%
#   arrange(EFFECTIVE_DATE_FY) %>%
#   mutate(reg_order = rank(EFFECTIVE_DATE_FY)) %>%
#   pivot_wider(names_from = reg_order, values_from = c(EFFECTIVE_DATE_FY, FY)) %>%
#   rename(EFFECTIVE_DATE_FY_1_all = "EFFECTIVE_DATE_FY_1",
#          EFFECTIVE_DATE_FY_2_all = "EFFECTIVE_DATE_FY_2",
#          FY_1_all = "FY_1",
#          FY_2_all = "FY_2") %>%
#   ungroup() %>%
#   select(-c(SUBSECTOR_USE, ZONE_USE, SPP_NAME))
#   
# # Add fishing year to all clusters 
# mh_expanded2 <- mh_expanded %>%
#   ungroup() %>%
#   # When specific to a subsector and zone
#   left_join(mh_fy_expanded_sz, by = c("REGION", "FMP", "SECTOR_USE", "ZONE_USE", "SUBSECTOR_USE",
#                                    "SPECIES_ITIS_USE", "COMMON_NAME_USE")) %>%
#   # When specific to a zone only
#   left_join(mh_fy_expanded_z, by = c("REGION", "FMP", "SECTOR_USE", "ZONE_USE",
#                                      "SPECIES_ITIS_USE", "COMMON_NAME_USE")) %>%
#   # When general to all zones and subsectors for expanded mtypes
#   left_join(mh_fy_expanded_a, by = c("REGION", "FMP", "SECTOR_USE",  
#                                      "SPECIES_ITIS_USE", "COMMON_NAME_USE")) %>%
#   # When general to all zones subsectors, and species for NOT expanded mtypes
#   left_join(mh_fy_a, by = c("REGION", "FMP", "SECTOR_USE")) %>%
#   # Take the first non-null value
#   mutate(EFFECTIVE_DATE_FY_1 = coalesce(EFFECTIVE_DATE_FY_1_sz, EFFECTIVE_DATE_FY_1_z, EFFECTIVE_DATE_FY_1_a, EFFECTIVE_DATE_FY_1_all),
#          EFFECTIVE_DATE_FY_2 = coalesce(EFFECTIVE_DATE_FY_2_sz, EFFECTIVE_DATE_FY_2_z, EFFECTIVE_DATE_FY_2_a, EFFECTIVE_DATE_FY_2_all),
#          EFFECTIVE_DATE_FY_3 = coalesce(EFFECTIVE_DATE_FY_3_sz, EFFECTIVE_DATE_FY_3_z, EFFECTIVE_DATE_FY_3_a),
#          FY_1 = coalesce(FY_1_sz, FY_1_z, FY_1_a, FY_1_all),
#          FY_2 = coalesce(FY_2_sz, FY_2_z, FY_2_a, FY_2_all),
#          FY_3 = coalesce(FY_3_sz, FY_3_z, FY_3_a)) %>%
#   # Remove unnecessary columns from join
#   select(-c(EFFECTIVE_DATE_FY_1_sz, EFFECTIVE_DATE_FY_1_z, EFFECTIVE_DATE_FY_1_a, EFFECTIVE_DATE_FY_1_all,
#             EFFECTIVE_DATE_FY_2_sz, EFFECTIVE_DATE_FY_2_z, EFFECTIVE_DATE_FY_2_a, EFFECTIVE_DATE_FY_2_all,
#             EFFECTIVE_DATE_FY_3_sz, EFFECTIVE_DATE_FY_3_z, EFFECTIVE_DATE_FY_3_a,
#             FY_1_sz, FY_1_z, FY_1_a,
#             FY_2_sz, FY_2_z, FY_2_a,
#             FY_3_sz, FY_3_z, FY_3_a))
#   
# 
# # CHECK: FMPs with a fishing year
# chk_fmp <- mh_fy3 %>% ungroup() %>% select(FMP) %>% distinct()
# # Identify records that are missing a fishing year for only the FMPs that have a reported fishing year
# chk_fy <- mh_expanded2 %>% filter(is.na(EFFECTIVE_DATE_FY_1)) %>% select(REGULATION_ID, FMP, REGION, SECTOR_USE, SPECIES_ITIS_USE, COMMON_NAME_USE, EFFECTIVE_DATE_FY_1, FY_1) %>%
#   group_by(FMP, COMMON_NAME_USE) %>%
#   summarise(N = n()) %>%
#   filter(FMP %in% chk_fmp)
# if(nrow(chk_fy) > 0){stop("Regulations missing a fishing year")}
# 
# # Export data sets ####
# # Export mh_expanded
# #write.csv(mh_expanded, here("data", "processed", paste0("mh_expanded_", format(Sys.Date(), "%Y%m%d"), ".csv")), row.names = FALSE)
# 
#   # Subset to fully processed component of mh_expanded
#   # These components will not need to be removed one processing accommodates the necessary logic
#   cluster_drop <- mh_expanded2 %>%
#     filter(MULTI_REG == 1 | 
#              MANAGEMENT_STATUS_USE %in% c("DELAYED", "WITHDRAWN") |
#              STATUS_TYPE == "RECURRING")
  
  mh_data_log <-  mh_expanded %>%
    #filter(!CLUSTER %in% cluster_drop$CLUSTER,
           #NEVER_IMPLEMENTED == 0) %>%
    select(CLUSTER, REGULATION_ID, FR_CITATION, FR_SECTION, FR_URL, FMP, ACTION, ACTION_TYPE, AMENDMENT_NUMBER, ACCOUNTABILITY_MEASURE,
           MANAGEMENT_CATEGORY, MANAGEMENT_TYPE, MANAGEMENT_TYPE_USE, MANAGEMENT_STATUS, MANAGEMENT_STATUS_USE, STATUS_TYPE, DETAILED,
           JURISDICTION, SECTOR, SECTOR_USE, SUBSECTOR, SUBSECTOR_USE, REGION, ZONE, ZONE_USE, JURISDICTIONAL_WATERS,
           COMMON_NAME_USE, SPP_TYPE, SPP_NAME, SPECIES_ITIS_USE,
           ADJUSTMENT, REVERSION, 
           EFFECTIVE_DATE, INEFFECTIVE_DATE, START_DATE2, END_DATE2, diff_days, 
           START_MONTH, START_DAY, START_DAY_USE, START_YEAR, START_TIME, START_TIME_USE, START_DAY_OF_WEEK, START_DAY_OF_WEEK_USE, 
           END_MONTH, END_MONTH_USE, END_DAY, END_DAY_USE, END_YEAR, END_YEAR_USE, END_TIME, END_TIME_USE, END_DAY_OF_WEEK, END_DAY_OF_WEEK_USE, 
           VALUE, VALUE_UNITS,VALUE_TYPE,VALUE_RATE,
           MULTI_REG_VALUE, MULTI_REG_FORECAST, MULTI_REG_CLOSURE, MULTI_REG_CLUSTER, REG_REMOVED, NEVER_IMPLEMENTED, FLAG)
           #EFFECTIVE_DATE_FY_1, FY_1, EFFECTIVE_DATE_FY_2, FY_2, EFFECTIVE_DATE_FY_3, FY_3)

  