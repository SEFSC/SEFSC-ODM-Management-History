# FISHING SEASON MH PROCESSING 
### NOT DONE, STILL NEED SOME THOUGHT ###

# Load packages ####
#install.packages("librarian")
librarian::shelf(here, tidyverse)

# Read in MH Data Log
mh_data_log <- readRDS(here("ODM-MH-Data_log", "data", "results", "MH_AL_2023Aug15.RDS"))

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

# All other fishing seasons < 365 days need to add closure for other days?
