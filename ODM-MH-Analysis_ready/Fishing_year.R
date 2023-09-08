# FISHING YEAR MH PROCESSING
# Process fishing year and incorporate into all clusters

# Load packages ####
#install.packages("librarian")
librarian::shelf(here, tidyverse)

# Read in MH Data Log
mh_data_log <- readRDS(here("ODM-MH-Data_log", "data", "results", "MH_AL_2023Aug15.RDS"))

# Recode regulations entered as FISHING SEASON to FISHING YEAR
# Filter for only fishing season management type
# Include REG_REMOVED == 0 because this helps us in getting an end date for fishing years that end, but we do not want to keep the record that "turns off" the regulation
mh_fs <- mh_data_log %>%
  filter(MANAGEMENT_TYPE_USE == 'FISHING SEASON', REG_REMOVED == 0)

# Identify cases that have a split fishing season - yes, we have split fishing season 
mh_fs_chk1 <- mh_fs %>% group_by(FR_CITATION, FMP, REGION, SPP_NAME, SECTOR_USE, SUBSECTOR_USE, ZONE_USE, EFFECTIVE_DATE) %>%
  summarise(N_seasons = n()) 
# Join to data
mh_fs2 <- mh_fs %>% 
  left_join(mh_fs_chk1, by = c("FR_CITATION", "REGION", "FMP", "EFFECTIVE_DATE", "SECTOR_USE", "SPP_NAME", "ZONE_USE",
                               "SUBSECTOR_USE"))

# Identify number of days within a fishing season 
mh_fs_ck2 <- mh_fs2 %>% 
  # Create as date field to subtract (the year is not important here)
  mutate(START_FY = case_when(!is.na(START_MONTH) & !is.na(START_DAY_USE) & is.na(START_YEAR) ~ as.Date(paste0("2023-", START_MONTH, "-", START_DAY_USE), "%Y-%m-%d"),
                              !is.na(START_MONTH) & !is.na(START_DAY_USE) & !is.na(START_YEAR) ~ as.Date(paste0(START_YEAR, "-", START_MONTH, "-", START_DAY_USE), "%Y-%m-%d")),
         END_FY = case_when(!is.na(END_MONTH_USE) & !is.na(END_DAY_USE) & is.na(END_YEAR) ~ as.Date(paste0("2023-", END_MONTH_USE, "-", END_DAY_USE), "%Y-%m-%d"),
                            !is.na(END_MONTH_USE) & !is.na(END_DAY_USE) & !is.na(END_YEAR) ~ as.Date(paste0(END_YEAR, "-", END_MONTH_USE, "-", END_DAY_USE), "%Y-%m-%d")),
         DIFF = case_when(END_MONTH_USE >= START_MONTH ~ END_FY - START_FY,
                          END_MONTH_USE < START_MONTH ~ START_FY - END_FY)) %>%
  filter(N_seasons == 1)

# Fishing season more than 365 days
fs_gt365 <- mh_fs_ck2 %>%
  filter(abs(DIFF) > 365) %>% select(REGULATION_ID, FR_CITATION, FR_URL) %>% distinct()

# Recode fishing season records as fishing year if 365 days
fs_to_fy <- mh_fs_ck2 %>% ungroup() %>%
  filter(DIFF == -364 | DIFF == -365 | DIFF == 1) %>%
  select(REGULATION_ID) %>% pull()

# Filter for only fishing year management type
# Reclass 8 regulations to FISHING YEAR even though entered at FISHING SEASON
# Include REG_REMOVED == 0 because this helps us in getting an end date for fishing years that end, but we do not want to keep the record that "turns off" the regulation
mh_fy <- mh_data_log %>% ungroup() %>%
  mutate(MANAGEMENT_TYPE_USE = case_when(REGULATION_ID %in% fs_to_fy ~ 'FISHING YEAR',
                                         TRUE ~ MANAGEMENT_TYPE_USE)) %>%
  filter(MANAGEMENT_TYPE_USE == 'FISHING YEAR', REG_REMOVED == 0) %>%
  # RECODE CLUSTER ID OF RECODED MANAGEMENT TYPES SO THE CLUSTERS MATCH
  # These are clusters that were entered as fishing season but really a fishing year
  mutate(NEW_CLUSTER = case_when(!REGULATION_ID %in% fs_to_fy & SPP_NAME != 'ALL' ~ CLUSTER)) %>%
  #arrange(START_DATE2) %>%
  group_by(MANAGEMENT_TYPE_USE, JURISDICTION, JURISDICTIONAL_WATERS, FMP, SECTOR_USE, SUBSECTOR_USE, REGION, COMMON_NAME_USE) %>%
  # first down and then up - this satisfies cases where first date needs a new cluster
  fill(NEW_CLUSTER, .direction = "downup") %>%
  ungroup()

# Identify clusters that have a split fishing year
# This is not actually a split fishing year because in the species expansion
# Royal red was expanded from ALL but in this FR, royal red had its own reg
# clusters 691 and 692 should be removed in data log for royal red
mh_fy_chk1 <- mh_fy %>% group_by(FR_CITATION, FMP, REGION, COMMON_NAME_USE, SECTOR_USE, SUBSECTOR_USE, ZONE_USE, EFFECTIVE_DATE) %>%
  summarise(N = n()) %>%
  filter(N > 1)
# STOP code if we have a split fishing year because the code needs to account for this
if((nrow(mh_fy_chk1) != 0 & !sum(str_detect(mh_fy_chk1$COMMON_NAME_USE, 'SHRIMP, ROYAL RED')))) { 
  stop("Fishing year split, please investigate")}

# Identify clusters that have a fishing year < 365 days
mh_fy_chk2 <- mh_fy %>%
  select(FMP, REGION, SPP_NAME, SECTOR_USE, SUBSECTOR_USE, ZONE_USE, START_MONTH, START_DAY_USE, END_MONTH_USE, END_DAY_USE) %>%
  # Create as date field to subtract (the year is not important here)
  mutate(START_FY = as.Date(paste0("2023-", START_MONTH, "-", START_DAY_USE), "%Y-%m-%d"),
         END_FY = as.Date(paste0("2023-", END_MONTH_USE, "-", END_DAY_USE), "%Y-%m-%d"),
         DIFF = START_FY - END_FY) %>%
  filter(DIFF != 1 & DIFF != 0 & DIFF != -364)
# STOP code if we have a split fishing year because the code needs to account for this
if(nrow(mh_fy_chk2) != 0) { stop("Fishing year not 365 days")}

mh_fy2 <- mh_fy %>% ungroup() %>%
  select(NEW_CLUSTER, FMP, REGION, COMMON_NAME_USE, SPECIES_ITIS_USE, SECTOR_USE, SUBSECTOR_USE, ZONE_USE, START_DATE2, START_MONTH, START_DAY_USE) %>%
  # Format start month and day as FY_MONTH, FY_DAY
  # Get month from number to month name abbreviation
  mutate(START_MONTH2 = format(as.Date(paste0("2021-", START_MONTH, "-01"), "%Y-%m-%d"), "%b"),
         FY = paste0(START_DAY_USE, "-", START_MONTH2))

# Consolidate into only meaningful changes
# Example cluster 226 South Atlantic back sea bass there are 4 FRs with the same rule (aka duplicates)
mh_fy3 <- mh_fy2 %>%
  ungroup() %>%
  group_by(NEW_CLUSTER, ZONE_USE, FY) %>%
  mutate(EFFECTIVE_DATE_FY = min(START_DATE2)) %>%
  select(NEW_CLUSTER, FMP, REGION, COMMON_NAME_USE, SPECIES_ITIS_USE, SECTOR_USE, SUBSECTOR_USE, ZONE_USE,
         EFFECTIVE_DATE_FY, FY) %>%
  distinct() %>%
  group_by(FMP, REGION, SECTOR_USE, SUBSECTOR_USE, ZONE_USE, SPECIES_ITIS_USE, COMMON_NAME_USE) %>%
  arrange(EFFECTIVE_DATE_FY) %>%
  mutate(reg_order = rank(EFFECTIVE_DATE_FY)) %>%
  # Remove clusters 691 and 692 because cause duplicates when species expansion
  filter(!NEW_CLUSTER %in% c(691, 692))

# Identify changes in fishing year
# There are 45 clusters that change fishing year at one point in time
# Most have 1 change, but South Atlantic greater amberjack and black sea bass have 3 fishing year starts over time
chk_change <- mh_fy3 %>% 
  group_by(FMP, REGION, SECTOR_USE, SUBSECTOR_USE, ZONE_USE, SPECIES_ITIS_USE, COMMON_NAME_USE) %>% 
  summarize(N = n()) %>% filter(N != 1)

# Structure fishing year data to wide-form for joining to the rest of MH data
mh_fy3_w <- mh_fy3 %>%
  pivot_wider(names_from = reg_order, values_from = c(EFFECTIVE_DATE_FY, FY)) %>%
  ungroup() 

# CHECK fishing year where we have known 3 changes over time
chk <- mh_fy3 %>% filter(FMP == 'SNAPPER-GROUPER FISHERY OF THE SOUTH ATLANTIC REGION', 
                         SECTOR_USE == 'RECREATIONAL', 
                         COMMON_NAME_USE == 'AMBERJACK, GREATER', ZONE_USE == 'ALL')
chk <- mh_fy3 %>% filter(FMP == 'SNAPPER-GROUPER FISHERY OF THE SOUTH ATLANTIC REGION', 
                         SECTOR_USE == 'RECREATIONAL', 
                         COMMON_NAME_USE == 'BASS, BLACK SEA', ZONE_USE == 'ALL')

# CHECK for duplicates 
chk_dups <- mh_fy3_w %>%
  group_by(FMP, REGION, SECTOR_USE, SUBSECTOR_USE, ZONE_USE, SPECIES_ITIS_USE, COMMON_NAME_USE) %>%
  summarise(nrecs = n())
if(any(chk_dups$nrecs) > 1){stop("You have duplicate records in the fishing year dataset ready to join")}

# Prep data for join to MH Data Log

# Subset FY expansion for the join
# Fishing year for specific subsector and zone
mh_fy_sz <- mh_fy3_w %>% 
  filter(SUBSECTOR_USE != 'ALL', ZONE_USE != 'ALL') %>%
  rename(EFFECTIVE_DATE_FY_1_sz = "EFFECTIVE_DATE_FY_1",
         EFFECTIVE_DATE_FY_2_sz = "EFFECTIVE_DATE_FY_2",
         EFFECTIVE_DATE_FY_3_sz = "EFFECTIVE_DATE_FY_3",
         FY_1_sz = "FY_1",
         FY_2_sz = "FY_2",
         FY_3_sz = "FY_3") %>%
  select(-c(NEW_CLUSTER))
# Fishing year for specific zone
mh_fy_z <- mh_fy3_w %>% 
  filter(ZONE_USE != 'ALL', SUBSECTOR_USE == 'ALL') %>%
  rename(EFFECTIVE_DATE_FY_1_z = "EFFECTIVE_DATE_FY_1",
         EFFECTIVE_DATE_FY_2_z = "EFFECTIVE_DATE_FY_2",
         EFFECTIVE_DATE_FY_3_z = "EFFECTIVE_DATE_FY_3",
         FY_1_z = "FY_1",
         FY_2_z = "FY_2",
         FY_3_z = "FY_3") %>%
  select(-c(NEW_CLUSTER, SUBSECTOR_USE))
# Fishing year for all zones and subsectors for management types where species expanded
mh_fy_a <- mh_fy3_w %>% 
  filter(ZONE_USE == 'ALL', SUBSECTOR_USE == 'ALL') %>%
  rename(EFFECTIVE_DATE_FY_1_a = "EFFECTIVE_DATE_FY_1",
         EFFECTIVE_DATE_FY_2_a = "EFFECTIVE_DATE_FY_2",
         EFFECTIVE_DATE_FY_3_a = "EFFECTIVE_DATE_FY_3",
         FY_1_a = "FY_1",
         FY_2_a = "FY_2",
         FY_3_a = "FY_3") %>%
  select(-c(NEW_CLUSTER, SUBSECTOR_USE, ZONE_USE))

# Add fishing year to all clusters 
mh_data_log2 <- mh_data_log %>%
  ungroup() %>%
  # When specific to a subsector and zone
  left_join(mh_fy_sz, by = join_by("REGION", "FMP", "SECTOR_USE", "ZONE_USE", "SUBSECTOR_USE",
                                   "SPECIES_ITIS_USE", "COMMON_NAME_USE")) %>%
  # When specific to a zone only
  left_join(mh_fy_z, by = join_by("REGION", "FMP", "SECTOR_USE", "ZONE_USE",
                                  "SPECIES_ITIS_USE", "COMMON_NAME_USE")) %>%
  # When general to all zones and subsectors for expanded mtypes
  left_join(mh_fy_a, by = join_by("REGION", "FMP", "SECTOR_USE",  
                                  "SPECIES_ITIS_USE", "COMMON_NAME_USE")) %>%
  # Take the first non-null value
  mutate(EFFECTIVE_DATE_FY_1 = coalesce(EFFECTIVE_DATE_FY_1_sz, EFFECTIVE_DATE_FY_1_z, EFFECTIVE_DATE_FY_1_a),
         EFFECTIVE_DATE_FY_2 = coalesce(EFFECTIVE_DATE_FY_2_sz, EFFECTIVE_DATE_FY_2_z, EFFECTIVE_DATE_FY_2_a),
         EFFECTIVE_DATE_FY_3 = coalesce(EFFECTIVE_DATE_FY_3_sz, EFFECTIVE_DATE_FY_3_z, EFFECTIVE_DATE_FY_3_a),
         FY_1 = coalesce(FY_1_sz, FY_1_z, FY_1_a),
         FY_2 = coalesce(FY_2_sz, FY_2_z, FY_2_a),
         FY_3 = coalesce(FY_3_sz, FY_3_z, FY_3_a)) %>%
  # Remove unnecessary columns from join
  select(-c(EFFECTIVE_DATE_FY_1_sz, EFFECTIVE_DATE_FY_1_z, EFFECTIVE_DATE_FY_1_a, 
            EFFECTIVE_DATE_FY_2_sz, EFFECTIVE_DATE_FY_2_z, EFFECTIVE_DATE_FY_2_a, 
            EFFECTIVE_DATE_FY_3_sz, EFFECTIVE_DATE_FY_3_z, EFFECTIVE_DATE_FY_3_a,
            FY_1_sz, FY_1_z, FY_1_a,
            FY_2_sz, FY_2_z, FY_2_a,
            FY_3_sz, FY_3_z, FY_3_a))

# CHECK: FMPs with a fishing year
chk_fmp <- mh_fy3 %>% ungroup() %>% select(FMP) %>% distinct()

# Identify records that are missing a fishing year for only the FMPs that have a reported fishing year
chk_fy <- mh_data_log2 %>% 
  filter(is.na(EFFECTIVE_DATE_FY_1)) %>% 
  select(REGULATION_ID, FMP, REGION, SECTOR_USE, SPECIES_ITIS_USE, COMMON_NAME_USE, EFFECTIVE_DATE_FY_1, FY_1) %>%
  group_by(FMP, COMMON_NAME_USE) %>%
  summarise(N = n()) %>%
  filter(FMP %in% chk_fmp)
if(nrow(chk_fy) > 0){stop("Regulations missing a fishing year")}

# Save as RDS file
if (!dir.exists(here("ODM-MH-Analysis_ready", "results"))){ dir.create(here("ODM-MH-Analysis_ready", "results")) }
saveRDS(mh_data_log2, here("ODM-MH-Analysis_ready", "results", paste0('MH_DL_wFY_', format(Sys.Date(), "%Y%b%d"), '.RDS')))

