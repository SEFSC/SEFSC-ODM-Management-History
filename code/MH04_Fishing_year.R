# FISHING YEAR MH PROCESSING

# Filter for only fishing year management type
# Include REG_REMOVED == 0 because this helps us in getting an end date for fishing years that end, but we do not want to keep the record that "turns off" the regulation
mh_fy <- mh_cluster_ids %>%
  filter(MANAGEMENT_TYPE_USE == 'FISHING YEAR', REG_REMOVED == 0)

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
  select(CLUSTER, FMP, REGION, SPP_TYPE, SPP_NAME, SPECIES_ITIS, SECTOR_USE, SUBSECTOR_USE, ZONE_USE, START_DATE, START_MONTH, START_DAY_USE) %>%
  # Format start month and day as FY_MONTH, FY_DAY
  # Get month from number to month name abbreviation
  mutate(START_MONTH2 = format(as.Date(paste0("2021-", START_MONTH, "-01"), "%Y-%m-%d"), "%b"),
         FY = paste0(START_DAY_USE, "-", START_MONTH2))

# Consolidate into only meaningful changes
mh_fy3 <- mh_fy2 %>%
  group_by(CLUSTER, ZONE_USE, FY) %>%
  mutate(START_DATE_FY = min(START_DATE)) %>%
  select(CLUSTER, FMP, REGION, SPP_TYPE, SPP_NAME, SPECIES_ITIS, SECTOR_USE, SUBSECTOR_USE, ZONE_USE,
         START_DATE_FY, FY) %>%
  distinct() %>%
  group_by(CLUSTER, ZONE_USE) %>%
  arrange(START_DATE_FY) %>%
  mutate(reg_order = rank(START_DATE_FY))

# Identify changes in fishing year
# There are 12 cluster/zones that change fishing year at one point in time, but only changed once
chk_change <- mh_fy3 %>% group_by(CLUSTER, ZONE_USE) %>% summarize(N = n()) %>% filter(N != 1)

# Structure fishing year data to wide-form for joining to the rest of MH data
mh_fy_w <- mh_fy3 %>%
  pivot_wider(names_from = reg_order, values_from = c(START_DATE_FY, FY)) %>%
  ungroup() %>%
  # Retain only fields of interest for the join to the other clusters
  select(-CLUSTER)

# CHECK we have two fishing years for All CMP species for recreational sector
chk <- mh_fy_w %>% filter(FMP == 'COASTAL MIGRATORY PELAGIC RESOURCES', REGION == 'GULF OF MEXICO', SECTOR_USE == 'RECREATIONAL', SPP_NAME == 'ALL')

# FISHING SEASON MH PROCESSING -- NOT DONE, STILL NEED SOME THOUGHT

# Filter for only fishing season management type
# Include REG_REMOVED == 0 because this helps us in getting an end date for fishing years that end, but we do not want to keep the record that "turns off" the regulation
mh_fs <- mh_cluster_ids %>%
  filter(MANAGEMENT_TYPE_USE == 'FISHING SEASON', REG_REMOVED == 0)

# Identify cases that have a split fishing season - yes, we have split fishing season 
mh_fs2 <- mh_fs %>% group_by(FR_CITATION, FMP, REGION, SPP_NAME, SECTOR_USE, SUBSECTOR_USE, ZONE_USE, EFFECTIVE_DATE) %>%
  summarise(N_seasons = n()) 
# Join to data
mh_fs3 <- mh_fs %>% 
  left_join(mh_fs2, by = c("FR_CITATION", "REGION", "FMP", "EFFECTIVE_DATE", "SECTOR_USE", "SPP_NAME", "ZONE_USE",
                           "SUBSECTOR_USE"))

# Identify records with a fishing season that is less than 1 year and not a split fishing season
mh_fs4 <- mh_fs3 %>% 
  select(FMP, REGION, SECTOR_USE, SUBSECTOR_USE, SPP_NAME, ZONE_USE, EFFECTIVE_DATE, START_MONTH, START_DAY, START_YEAR, START_TIME,
         INEFFECTIVE_DATE, END_MONTH, END_DAY, END_YEAR, END_TIME, N_seasons)

