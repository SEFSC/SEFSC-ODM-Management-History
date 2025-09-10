##### Catch Limits ####
# Code to filter to all Catch Limit regulations for a species of interest

##### Load packages ####
librarian::shelf(here, tidyverse, gt, flextable, officer, dplyr, gt, officer, lubridate, htmltools, rmarkdown, tidyr)

#### Load data ####
# Load the MH Data Log
mh <- readRDS(here("ODM-MH-Data_log", "data", "results", "MH_DL_2025Sep10.RDS"))

#### Define species and region of interest ####
species = c('SNAPPER, CUBERA', 'AMBERJACK, GREATER', 'AMBERJACK, LESSER', 'JACK, ALMACO', 'SNAPPER, SILK', 'SNAPPER, QUEEN', 'SNAPPER, BLACKFIN', 'WENCHMAN',
            'DRUM, RED', 'COBIA', 'GROUPER, GAG', 'SNAPPER, GRAY', 'MACKEREL, KING', 'GROUPER, YELLOWEDGE', 'SNAPPER, LANE', 'GROUPER, RED', 'SNAPPER, RED', 
            'SCAMP', 'MACKEREL, SPANISH','HOGFISH', 'SNAPPER, MUTTON', 'SNAPPER, YELLOWTAIL', 'SNAPPER, VERMILION', 'TRIGGERFISH, GRAY', 'TRIGGERFISH, QUEEN')
region = 'GULF OF MEXICO'
sector = 'RECREATIONAL'

#### ACLs ####
# Filter to species and region of interest
# Remove non-detailed regulations and those that were never implemented
# Filter to only include records that apply to the entire region (ZONE_USE = ALL) and sector of interest
ACLs <- mh %>%
  filter(COMMON_NAME_USE %in% species,
         REGION %in% region,
         SECTOR == sector,
         DETAILED == "YES",
         NEVER_IMPLEMENTED %in% c(0, NA),
         REG_REMOVED == 0,
         MANAGEMENT_TYPE_USE == "ACL") %>%
  arrange(CLUSTER, START_DATE2) %>%
  mutate(Species = case_when(str_detect(COMMON_NAME_USE, ",") ~ {parts <- str_split_fixed(COMMON_NAME_USE, ",", 2)
  str_to_title(str_trim(parts[,2])) %>% paste(str_to_title(str_trim(parts[,1])))},
                             TRUE ~ str_to_title(COMMON_NAME_USE)),
         Fishery = str_to_title(paste0(SECTOR_USE, " ", SUBSECTOR_USE)),
         `Region Affected` = str_to_title(paste0(REGION, " ", ZONE_USE)),
         `First Year in Effect` = year(START_DATE2),
         `Fishing Year Effective Date` = format(START_DATE2, "%m/%d/%Y"),
         `FR Notice Effetive Date` = format(EFFECTIVE_DATE, "%m/%d/%Y"),
         `FR Notice Ineffective Date` = format(INEFFECTIVE_DATE, "%m/%d/%Y"),
         ACL = case_when(MANAGEMENT_TYPE_USE == "ACL" ~ paste0(VALUE, " ", tolower(VALUE_UNITS),ifelse(!is.na(VALUE_TYPE), paste0(" (", tolower(VALUE_TYPE), ")"), "")),
                         TRUE ~ NA_character_),
         `FR Reference` = case_when(MANAGEMENT_TYPE_USE == "ACL" ~ FR_CITATION,
                                TRUE ~ NA_character_),
         `FR URL` = case_when(MANAGEMENT_TYPE_USE == "ACL" ~ FR_URL,
                          TRUE ~ NA_character_),
         `Amendment Number or Rule Type` = case_when(!is.na(AMENDMENT_NUMBER) & !is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE, " ", AMENDMENT_NUMBER)),
                                                     !is.na(AMENDMENT_NUMBER) &  is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", AMENDMENT_NUMBER)),
                                                     is.na(AMENDMENT_NUMBER) & !is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE)),
                                                     is.na(AMENDMENT_NUMBER) &  is.na(ACTION_TYPE) ~ str_to_title(ACTION)),
         `Cluster` = CLUSTER) %>%
  select(CLUSTER, Species, Fishery, `Region Affected`,
        `First Year in Effect`, `Fishing Year Effective Date`, `FR Notice Effetive Date`, `FR Notice Ineffective Date`, `ACL`,
         `FR Reference`, `FR URL`, `Amendment Number or Rule Type`) %>%
  arrange(Species, CLUSTER, `First Year in Effect`)

# Quota
Quotas <- mh %>%
  filter(COMMON_NAME_USE %in% species,
         REGION %in% region,
         SECTOR == sector,
         DETAILED == "YES",
         NEVER_IMPLEMENTED %in% c(0, NA),
         REG_REMOVED == 0,
         MANAGEMENT_TYPE_USE == "QUOTA") %>%
  arrange(CLUSTER, START_DATE2) %>%
  mutate(Species = case_when(str_detect(COMMON_NAME_USE, ",") ~ {parts <- str_split_fixed(COMMON_NAME_USE, ",", 2)
  str_to_title(str_trim(parts[,2])) %>% paste(str_to_title(str_trim(parts[,1])))},
  TRUE ~ str_to_title(COMMON_NAME_USE)),
  Fishery = str_to_title(paste0(SECTOR_USE, " ", SUBSECTOR_USE)),
  `Region Affected` = str_to_title(paste0(REGION, " ", ZONE_USE)),
  `First Year in Effect` = year(START_DATE2),
  `Fishing Year Effective Date` = format(START_DATE2, "%m/%d/%Y"),
  `FR Notice Effetive Date` = format(EFFECTIVE_DATE, "%m/%d/%Y"),
  `FR Notice Ineffective Date` = format(INEFFECTIVE_DATE, "%m/%d/%Y"),
  Quota = case_when(MANAGEMENT_TYPE_USE == "QUOTA" ~ paste0(VALUE, " ", tolower(VALUE_UNITS),ifelse(!is.na(VALUE_TYPE), paste0(" (", tolower(VALUE_TYPE), ")"), "")),
                  TRUE ~ NA_character_),
  `FR Reference` = case_when(MANAGEMENT_TYPE_USE == "QUOTA" ~ FR_CITATION,
                             TRUE ~ NA_character_),
  `FR URL` = case_when(MANAGEMENT_TYPE_USE == "QUOTA" ~ FR_URL,
                       TRUE ~ NA_character_),
  `Amendment Number or Rule Type` = case_when(!is.na(AMENDMENT_NUMBER) & !is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE, " ", AMENDMENT_NUMBER)),
                                              !is.na(AMENDMENT_NUMBER) &  is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", AMENDMENT_NUMBER)),
                                              is.na(AMENDMENT_NUMBER) & !is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE)),
                                              is.na(AMENDMENT_NUMBER) &  is.na(ACTION_TYPE) ~ str_to_title(ACTION)),
  `Cluster` = CLUSTER) %>%
  select(CLUSTER, Species, Fishery, `Region Affected`,
         `First Year in Effect`, `Fishing Year Effective Date`, `FR Notice Effetive Date`, `FR Notice Ineffective Date`, `Quota`,
         `FR Reference`, `FR URL`, `Amendment Number or Rule Type`) %>%
  arrange(Species, CLUSTER, `First Year in Effect`)

# ACT
ACTs <- mh %>%
  filter(COMMON_NAME_USE %in% species,
         REGION %in% region,
         SECTOR == sector,
         DETAILED == "YES",
         NEVER_IMPLEMENTED %in% c(0, NA),
         MANAGEMENT_TYPE_USE == "ACT") %>%
  arrange(CLUSTER, START_DATE2) %>%
  mutate(Species = case_when(str_detect(COMMON_NAME_USE, ",") ~ {parts <- str_split_fixed(COMMON_NAME_USE, ",", 2)
         str_to_title(str_trim(parts[,2])) %>% paste(str_to_title(str_trim(parts[,1])))},
         TRUE ~ str_to_title(COMMON_NAME_USE)),
         Fishery = str_to_title(paste0(SECTOR_USE, " ", SUBSECTOR_USE)),
         `Region Affected` = str_to_title(paste0(REGION, " ", ZONE_USE)),
         `First Year in Effect` = year(START_DATE2),
         `Fishing Year Effective Date` = format(START_DATE2, "%m/%d/%Y"),
         `FR Notice Effetive Date` = format(EFFECTIVE_DATE, "%m/%d/%Y"),
         `FR Notice Ineffective Date` = format(INEFFECTIVE_DATE, "%m/%d/%Y"),
         ACT = case_when(MANAGEMENT_TYPE_USE == "ACT" ~ paste0(VALUE, " ", tolower(VALUE_UNITS),ifelse(!is.na(VALUE_TYPE), paste0(" (", tolower(VALUE_TYPE), ")"), "")),
                           TRUE ~ NA_character_),
         `FR Reference` = case_when(MANAGEMENT_TYPE_USE == "ACT" ~ FR_CITATION,
                                    TRUE ~ NA_character_),
         `FR URL` = case_when(MANAGEMENT_TYPE_USE == "ACT" ~ FR_URL,
                              TRUE ~ NA_character_),
         `Amendment Number or Rule Type` = case_when(!is.na(AMENDMENT_NUMBER) & !is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE, " ", AMENDMENT_NUMBER)),
                                                     !is.na(AMENDMENT_NUMBER) &  is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", AMENDMENT_NUMBER)),
                                                     is.na(AMENDMENT_NUMBER) & !is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE)),
                                                     is.na(AMENDMENT_NUMBER) &  is.na(ACTION_TYPE) ~ str_to_title(ACTION)),
         `Cluster` = CLUSTER) %>%
  select(CLUSTER, Species, Fishery, `Region Affected`,
         `First Year in Effect`, `Fishing Year Effective Date`, `FR Notice Effetive Date`, `FR Notice Ineffective Date`, `ACT`,
         `FR Reference`, `FR URL`, `Amendment Number or Rule Type`) %>%
  arrange(Species, CLUSTER, `First Year in Effect`)

# TAC
TACs <- mh %>%
  filter(COMMON_NAME_USE %in% species,
         REGION %in% region,
         SECTOR == sector,
         DETAILED == "YES",
         NEVER_IMPLEMENTED %in% c(0, NA),
         REG_REMOVED == 0,
         MANAGEMENT_TYPE_USE == "TAC") %>%
  arrange(CLUSTER, START_DATE2) %>%
  mutate(Species = case_when(str_detect(COMMON_NAME_USE, ",") ~ {parts <- str_split_fixed(COMMON_NAME_USE, ",", 2)
  str_to_title(str_trim(parts[,2])) %>% paste(str_to_title(str_trim(parts[,1])))},
  TRUE ~ str_to_title(COMMON_NAME_USE)),
  Fishery = str_to_title(paste0(SECTOR_USE, " ", SUBSECTOR_USE)),
  `Region Affected` = str_to_title(paste0(REGION, " ", ZONE_USE)),
  `First Year in Effect` = year(START_DATE2),
  `Fishing Year Effective Date` = format(START_DATE2, "%m/%d/%Y"),
  `FR Notice Effetive Date` = format(EFFECTIVE_DATE, "%m/%d/%Y"),
  `FR Notice Ineffective Date` = format(INEFFECTIVE_DATE, "%m/%d/%Y"),
  TAC = case_when(MANAGEMENT_TYPE_USE == "TAC" ~ paste0(VALUE, " ", tolower(VALUE_UNITS),ifelse(!is.na(VALUE_TYPE), paste0(" (", tolower(VALUE_TYPE), ")"), "")),
                  TRUE ~ NA_character_),
  `FR Reference` = case_when(MANAGEMENT_TYPE_USE == "TAC" ~ FR_CITATION,
                             TRUE ~ NA_character_),
  `FR URL` = case_when(MANAGEMENT_TYPE_USE == "TAC" ~ FR_URL,
                       TRUE ~ NA_character_),
  `Amendment Number or Rule Type` = case_when(!is.na(AMENDMENT_NUMBER) & !is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE, " ", AMENDMENT_NUMBER)),
                                              !is.na(AMENDMENT_NUMBER) &  is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", AMENDMENT_NUMBER)),
                                              is.na(AMENDMENT_NUMBER) & !is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE)),
                                              is.na(AMENDMENT_NUMBER) &  is.na(ACTION_TYPE) ~ str_to_title(ACTION)),
  `Cluster` = CLUSTER) %>%
  select(CLUSTER, Species, Fishery, `Region Affected`,
         `First Year in Effect`, `Fishing Year Effective Date`, `FR Notice Effetive Date`, `FR Notice Ineffective Date`, `TAC`,
         `FR Reference`, `FR URL`, `Amendment Number or Rule Type`) %>%
  arrange(Species, CLUSTER, `First Year in Effect`)
