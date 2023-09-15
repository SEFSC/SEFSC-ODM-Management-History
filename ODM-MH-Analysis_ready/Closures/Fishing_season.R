# FISHING SEASON MH PROCESSING 
### NOT DONE, STILL NEED SOME THOUGHT ###

# Filter for only fishing season management type
# Include REG_REMOVED == 0 because this helps us in getting an end date for fishing years that end, but we do not want to keep the record that "turns off" the regulation
mh_fs <- mh_data_log %>%
  filter(MANAGEMENT_TYPE_USE == 'FISHING SEASON', REG_REMOVED == 0)

# Identify cases that have a split fishing season - yes, we have split fishing season 
mh_fs_chk1 <- mh_fs %>% ungroup() %>%
  group_by(FR_CITATION, FMP, REGION, COMMON_NAME_USE, SECTOR_USE, SUBSECTOR_USE, ZONE_USE, EFFECTIVE_DATE) %>%
  summarise(N_seasons = n()) 
# Join to data
mh_fs2 <- mh_fs %>% 
  left_join(mh_fs_chk1, by = c("FR_CITATION", "REGION", "FMP", "EFFECTIVE_DATE", "SECTOR_USE", 
                               "COMMON_NAME_USE", "ZONE_USE", "SUBSECTOR_USE"))

# Check spiny lobster - had no fishing year because has fishing season?
# Include fishing season in "CLOSURE" story and process there
chk <- mh_data_log %>% 
  filter(COMMON_NAME_USE == 'LOBSTER, CARIBBEAN SPINY', is.na(FY_1),
         MANAGEMENT_TYPE_USE == 'FISHING SEASON') %>%
  select(CLUSTER, FR_CITATION, MANAGEMENT_TYPE_USE, MANAGEMENT_STATUS_USE, COMMON_NAME_USE,
         FMP, REGION, SECTOR_USE, SUBSECTOR_USE, ZONE_USE,
         EFFECTIVE_DATE, INEFFECTIVE_DATE, START_DATE2, END_DATE2, 
         START_MONTH, START_DAY_USE, START_DAY_OF_WEEK_USE, START_TIME_USE, START_YEAR,
         END_MONTH_USE, END_DAY_USE, END_DAY_OF_WEEK_USE, END_TIME_USE, END_YEAR) %>%
  arrange(REGION, SECTOR_USE, SUBSECTOR_USE, START_DATE2)

# Check red snapper that has 2 fishing seasons
# Because these are multi-regs (same FR) the logic for start date2 and end date2 is not working
chk <- mh_data_log %>% 
  filter(COMMON_NAME_USE == 'SNAPPER, RED', REGION == 'GULF OF MEXICO',
         MANAGEMENT_TYPE_USE == 'FISHING SEASON') %>%
  select(CLUSTER, FR_CITATION, MANAGEMENT_TYPE_USE, MANAGEMENT_STATUS_USE, COMMON_NAME_USE,
         FMP, REGION, SECTOR_USE, SUBSECTOR_USE, ZONE_USE,
         EFFECTIVE_DATE, INEFFECTIVE_DATE, START_DATE2, END_DATE2, 
         START_MONTH, START_DAY_USE, START_DAY_OF_WEEK_USE, START_TIME_USE, START_YEAR,
         END_MONTH_USE, END_DAY_USE, END_DAY_OF_WEEK_USE, END_TIME_USE, END_YEAR,
         EFFECTIVE_DATE_FY_1, EFFECTIVE_DATE_FY_2, EFFECTIVE_DATE_FY_3,
         FY_1, FY_2, FY_3) %>%
  arrange(REGION, SECTOR_USE, SUBSECTOR_USE, START_DATE2)

# Identify number of days within a fishing season 
mh_fs_ck2 <- mh_fs2 %>% 
  # Create as date field to subtract (the year is not important here)
  mutate(START_FS = case_when(!is.na(START_MONTH) & !is.na(START_DAY_USE) & is.na(START_YEAR) ~ as.Date(paste0("2023-", START_MONTH, "-", START_DAY_USE), "%Y-%m-%d"),
                              !is.na(START_MONTH) & !is.na(START_DAY_USE) & !is.na(START_YEAR) ~ as.Date(paste0(START_YEAR, "-", START_MONTH, "-", START_DAY_USE), "%Y-%m-%d")),
         END_FS = case_when(!is.na(END_MONTH_USE) & !is.na(END_DAY_USE) & is.na(END_YEAR_USE) ~ as.Date(paste0("2023-", END_MONTH_USE, "-", END_DAY_USE), "%Y-%m-%d"),
                            !is.na(END_MONTH_USE) & !is.na(END_DAY_USE) & !is.na(END_YEAR_USE) ~ as.Date(paste0(END_YEAR_USE, "-", END_MONTH_USE, "-", END_DAY_USE), "%Y-%m-%d")),
         DIFF = case_when(END_MONTH_USE >= START_MONTH ~ END_FS - START_FS,
                          END_MONTH_USE < START_MONTH ~ START_FS - END_FS)) %>%
  filter(N_seasons == 1)

# Recode fishing season records as fishing year if 365 days
fs_to_fy <- mh_fs_ck2 %>% ungroup() %>%
  filter(abs(DIFF) %in% c(364, 365, 1)) %>%
  select(REGULATION_ID) %>% pull()

# Check for cases where there is a single fishing season and its less than 365 days
# These are cases where the inverse is then considered a closure
fs_lt365 <- mh_fs_ck2 %>%
  filter(abs(DIFF) < 365, !REGULATION_ID %in% fs_to_fy) %>% 
  select(CLUSTER, REGULATION_ID, FR_CITATION, FR_URL, 
         REGION, ZONE_USE, COMMON_NAME_USE, SECTOR_USE, SUBSECTOR_USE,
         START_DATE2, END_DATE2, START_FS, END_FS, DIFF) %>% 
  distinct()

# Fishing season more than 365 days
# Only one regulation - Gulf red drum and that is how the FR is worded
fs_gt365 <- mh_fs_ck2 %>%
  filter(abs(DIFF) > 365, !REGULATION_ID %in% fs_to_fy) %>% 
  select(CLUSTER, REGULATION_ID, FR_CITATION, FR_URL, 
         REGION, ZONE_USE, COMMON_NAME_USE, SECTOR_USE, SUBSECTOR_USE,
         START_DATE2, END_DATE2, START_FS, END_FS, DIFF) %>% 
  distinct()
chk <- mh_data_log %>% filter(REGULATION_ID == 3486)

# Fishing season missing diff days
# Some just have start fishing season 
# For the red snapper fishing season, that means it ends at the end of the fishing year
# Others don't have fishing year and are missing key date fields because they are certain days of the week
# All of those are rec Caribbean Spiny Lobster in the Gulf and SATL
fs_nodiff <- mh_fs_ck2 %>%
  filter(is.na(DIFF)) %>% 
  select(CLUSTER, REGULATION_ID, FR_CITATION, FR_URL, 
         REGION, ZONE_USE, COMMON_NAME_USE, SECTOR_USE, SUBSECTOR_USE,
         EFFECTIVE_DATE, INEFFECTIVE_DATE, START_DATE2, END_DATE2, 
         START_DAY, START_DAY_USE, START_MONTH, START_TIME_USE, START_YEAR, START_DAY_OF_WEEK_USE, START_FS, 
         END_DAY, END_DAY_USE, END_MONTH, END_MONTH_USE, END_TIME, END_TIME_USE, END_YEAR, END_YEAR_USE, END_DAY_OF_WEEK_USE, END_FS, DIFF,
         EFFECTIVE_DATE_FY_1, EFFECTIVE_DATE_FY_2, EFFECTIVE_DATE_FY_3,
         FY_1, FY_2, FY_3) %>% 
  distinct()

# Remove these FRs since already added to fishing year
mh_fs3 <- mh_fs2 %>%
  filter(!REGULATION_ID %in% fs_to_fy)

# Filter for species of interest
fs_spp <- mh_fs3 %>%
  filter(COMMON_NAME_USE == spp, REGION == region)

# Format
