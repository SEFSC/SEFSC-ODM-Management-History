# Process fishing year and fishing season

# FISHING SEASON MH PROCESSING -- NOT DONE, STILL NEED SOME THOUGHT

# Filter for only fishing season management type
# Include REG_REMOVED == 0 because this helps us in getting an end date for fishing years that end, but we do not want to keep the record that "turns off" the regulation
mh_fs <- mh_cluster_ids %>%
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
fs_to_fy <- mh_fs_ck2 %>%
  filter(DIFF == -364 | DIFF == -365 | DIFF == 1) %>%
  select(REGULATION_ID) %>% pull()

# All other fishing seasons < 365 days need to add closure for other days?

# FISHING YEAR MH PROCESSING

# Filter for only fishing year management type
# Include REG_REMOVED == 0 because this helps us in getting an end date for fishing years that end, but we do not want to keep the record that "turns off" the regulation
mh_fy <- mh_cluster_ids %>%
  mutate(MANAGEMENT_TYPE_USE = case_when(REGULATION_ID %in% fs_to_fy ~ 'FISHING YEAR',
                                         TRUE ~ MANAGEMENT_TYPE_USE)) %>%
  filter(MANAGEMENT_TYPE_USE == 'FISHING YEAR', REG_REMOVED == 0) %>%
  # RECODE CLUSTER ID OF RECODED MANAGEMENT TYPES SO THE CLUSTERS MATCH
  mutate(NEW_CLUSTER = case_when(!REGULATION_ID %in% fs_to_fy ~ CLUSTER)) %>%
  group_by(MANAGEMENT_TYPE_USE, JURISDICTION, JURISDICTIONAL_WATERS, FMP, SECTOR_USE, SUBSECTOR_USE, REGION, SPP_NAME) %>%
  fill(NEW_CLUSTER) %>%
  ungroup()

# Identify clusters that have a split fishing year - NONE
mh_fy_chk1 <- mh_fy %>% group_by(FR_CITATION, FMP, REGION, SPP_NAME, SECTOR_USE, SUBSECTOR_USE, ZONE_USE, EFFECTIVE_DATE) %>%
  summarise(N = n()) %>%
  filter(N > 1)
# STOP code if we have a split fishing year because the code needs to account for this
if(nrow(mh_fy_chk1) != 0) { stop("Fishing year split, please investigate")}

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
  select(NEW_CLUSTER, FMP, REGION, SPP_TYPE, SPP_NAME, SPECIES_ITIS, SECTOR_USE, SUBSECTOR_USE, ZONE_USE, START_DATE, START_MONTH, START_DAY_USE) %>%
  # Format start month and day as FY_MONTH, FY_DAY
  # Get month from number to month name abbreviation
  mutate(START_MONTH2 = format(as.Date(paste0("2021-", START_MONTH, "-01"), "%Y-%m-%d"), "%b"),
         FY = paste0(START_DAY_USE, "-", START_MONTH2))

# Consolidate into only meaningful changes
mh_fy3 <- mh_fy2 %>%
  ungroup() %>%
  group_by(NEW_CLUSTER, ZONE_USE, FY) %>%
  mutate(EFFECTIVE_DATE_FY = min(START_DATE)) %>%
  select(NEW_CLUSTER, FMP, REGION, SPP_TYPE, SPP_NAME, SPECIES_ITIS, SECTOR_USE, SUBSECTOR_USE, ZONE_USE,
         EFFECTIVE_DATE_FY, FY) %>%
  distinct() %>%
  # Remove clusters 691 and 692 because cause duplicates when species expansion
  filter(!NEW_CLUSTER %in% c(691, 692))

# Add a reopening to each closure cluster at the start of the fishing season or fishing year
