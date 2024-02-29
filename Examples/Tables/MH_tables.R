# CREATE TABLES FOR BAG LIMITS AND SIZE LIMITS FOR EASIER VISUALIZATION

# Load packages ####
#install.packages("librarian")
librarian::shelf(here, tidyverse, gt, flextable, officer, dplyr, gt)

# RUN MH CODE
here::i_am('Examples/Tables/MH_tables.R')
source(here('ODM-MH-Data_log', 'main_MH_prep.R'))

# Current clusters in the test data set are very simple stories
# Recreational size limits for South Atlantic gray triggerfish = cluster 952 - has different zones
# Commercial size limits for South Atlantic red snapper = cluster 365 - has reg removed flag
# Commercial and recreational size limits for Gulf red snapper = cluster 199 & 220
# Recreational bag limits for Gulf red snapper = cluster 128
# Recreational crew bag limits for Gulf red snapper = cluster 200
# Commercial trip limits for South Atlantic gray triggerfish (all gears and trawl gear) = cluster 1904 & 951
# Commercial and recreational size limits for South Atlantic gray triggerfish = cluster 241 & 952
# Recreational bag limits for South Atlantic gray triggerfish = cluster 1994
# Recreational closures for South Atlantic gray triggerfish = cluster 1527
# Recreational and commercial size limits for Gulf Spanish mackerel - 19 & 20
# Commercial one-time closures for Gulf Spanish mackerel - 1268, 1267, 1266, 1265, 1210, 1098
# Recreational one-time closures for Gulf Spanish mackerel - 1097
# Recreational and commercial prohibited gear for Gulf Spanish mackerel - 51, 52, 640, 641, 642, 643, 70, 71, 552, 553
# Commercial gill net specifications for Gulf Spanish mackerel - 577
# Recreational and commercial landed intact for Gulf Spanish mackerel - 519 & 520
# Recreational bag limits Gulf red grouper - 162
# Recreational bag limits Gulf groupers combined - 164
# Commercial size limit Gulf red grouper - 165
# Recreational size limit Gulf red grouper - 465

mh_tables_use <- readRDS(here("ODM-MH-Data_log", "data", "results", "MH_DL_2023SEP08.RDS"))

# Gulf red grouper tables
spp = 'GROUPER, RED'
region = 'GULF OF MEXICO'

# Table for Bag Limits
#Filter data and combine column information
bag_tab1 <- mh_tables_use %>%
  filter(COMMON_NAME_USE == spp, REGION == region, MANAGEMENT_TYPE_USE == 'BAG LIMIT', SPP_TYPE == 'COMMON_NAME') %>%
  filter(NEVER_IMPLEMENTED == 0) %>%
  arrange(FR_CITATION) %>%
  ungroup() %>%
  mutate(START_DATE1 = format(START_DATE2, "%m/%d/%Y"),
         END_DATE1 = format(END_DATE2, "%m/%d/%Y"),
         SECTOR1 = str_to_title(paste0(SECTOR_USE, "\n", SUBSECTOR_USE)),
         ZONE1 = str_to_title(paste0(REGION, "\n", ZONE_USE)),
         VALUE1 = case_when(FLAG == 'YES' ~ paste0(VALUE, " ", tolower(VALUE_UNITS), "*"),
                            FLAG == 'NO' ~ paste0(VALUE, " ", tolower(VALUE_UNITS))),
         ACTION1 = case_when(is.na(AMENDMENT_NUMBER) & !is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE)),
                            is.na(AMENDMENT_NUMBER) & is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION)),
                            !is.na(AMENDMENT_NUMBER) & is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", AMENDMENT_NUMBER)),
                            !is.na(AMENDMENT_NUMBER) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE, " ", AMENDMENT_NUMBER))),
         START_YEAR1 = format(START_DATE2, "%Y"))

#Create table
bag_tab2 <- bag_tab1 %>%
  select(COMMON_NAME_USE, SECTOR1, ZONE1, MANAGEMENT_TYPE_USE, START_YEAR1, START_DATE1, END_DATE1, VALUE1, FR_CITATION, ACTION1)%>%
  do(tab = flextable(.[1:10]) %>%
       set_header_labels(SECTOR1 = "Fishery",
                         ZONE1 = "Region Affected",
                         START_YEAR1 = "Start Year",
                         START_DATE1 = "Effective Date",
                         END_DATE1 = "End Date",
                         VALUE = "Bag Limit Per Person/Day",
                         FR_CITATION = "FR Reference(s)",
                         ACTION1 = "Amendment Number or Rule Type",
                         COMMON_NAME_USE = "Species",
                         MANAGEMENT_TYPE_USE = "Management Type") %>%
  merge_v(j = 1, part = "body") %>%
  merge_v(j = 2, part = "body") %>%
  merge_v(j = 3, part = "body") %>%
  merge_v(j = 4, part = "body") %>%
    theme_box() %>%
  hline_top(part = "header", border = fp_border(color = "black", width = 2)) %>%
  hline_bottom(part = "header", border = fp_border(color = "black", width = 2)) %>%
  fontsize(part = "all", size = 12) %>%
  font(part = "all", fontname = "Times New Roman") %>%
  align(part = "all", align = "center") %>%
  set_caption(paste0(bag_tab1$REGION, " ", bag_tab1$COMMON_NAME_USE, " ", bag_tab1$MANAGEMENT_TYPE_USE, collapse = ",")))

bag_tab2$tab[[1]]

#Table for Recreational Size Limits
#Filter data and combine column information
size_tab1 <- mh_tables_use %>%
  filter(COMMON_NAME_USE == spp, REGION == region, MANAGEMENT_CATEGORY == 'SELECTIVITY CONTROLS', SECTOR_USE == 'RECREATIONAL') %>%
  filter(NEVER_IMPLEMENTED == 0) %>%
  ungroup() %>%
  mutate(START_DATE2 = format(START_DATE2, "%m/%d/%Y"),
         END_DATE2 = format(END_DATE2, "%m/%d/%Y"),
         SECTOR2 = str_to_title(paste0(SECTOR_USE, "\n", SUBSECTOR_USE)),
         ZONE2 = str_to_title(paste0(REGION, "\n", ZONE_USE)),
         VALUE2 = case_when(FLAG == 'YES' ~ paste0(VALUE, " ", tolower(VALUE_UNITS), "*"),
                            FLAG == 'NO' ~ paste0(VALUE, " ", tolower(VALUE_UNITS))),
         ACTION2 = case_when(is.na(AMENDMENT_NUMBER) & !is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE)),
                             is.na(AMENDMENT_NUMBER) & is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION)),
                             !is.na(AMENDMENT_NUMBER) & is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", AMENDMENT_NUMBER)),
                             !is.na(AMENDMENT_NUMBER) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE, " ", AMENDMENT_NUMBER))),
         START_YEAR2 = format(START_DATE2, "%Y"),
         VALUE_TYPE2 = case_when(MANAGEMENT_TYPE_USE == 'MINIMUM SIZE LIMIT' ~ paste0("Minimum", str_to_title(VALUE_TYPE)),
                                 MANAGEMENT_TYPE_USE == 'MAXIMUM SIZE LIMIT' ~ paste0("Maximum", str_to_title(VALUE_TYPE))))
        
        
#Create table
size_tab2 <- size_tab1 %>%
  select(COMMON_NAME_USE, SECTOR2, ZONE2, START_YEAR2, START_DATE2, END_DATE2, VALUE2, VALUE_TYPE2, FR_CITATION, ACTION2)%>%
  do(tab = flextable(.[1:10]) %>%
       set_header_labels(SECTOR2 = "Fishery",
                         ZONE2 = "Region Affected",
                         START_YEAR2 = "Start Year",
                         START_DATE2 = "Effective Date",
                         END_DATE2 = "End Date",
                         VALUE2 = "Size Limit",
                         VALUE_TYPE2 = "Length Type",
                         FR_CITATION = "FR Reference(s)",
                         ACTION2 = "Amendment Number or Rule Type",
                         COMMON_NAME_USE = "Species"))

size_tab2$tab[[1]]

#Table for Commercial Size Limits
#Filter data and combine column information
size_tab3 <- mh_tables_use %>%
  filter(COMMON_NAME_USE == spp, REGION == region, MANAGEMENT_CATEGORY == 'SELECTIVITY CONTROLS', SECTOR_USE == 'COMMERCIAL') %>%
  filter(NEVER_IMPLEMENTED == 0) %>%
  ungroup() %>%
  mutate(START_DATE3 = format(START_DATE2, "%m/%d/%Y"),
         END_DATE3 = format(END_DATE2, "%m/%d/%Y"),
         SECTOR3 = str_to_title(paste0(SECTOR_USE, "\n", SUBSECTOR_USE)),
         ZONE3 = str_to_title(paste0(REGION, "\n", ZONE_USE)),
         VALUE3 = case_when(FLAG == 'YES' ~ paste0(VALUE, " ", tolower(VALUE_UNITS), "*"),
                            FLAG == 'NO' ~ paste0(VALUE, " ", tolower(VALUE_UNITS))),
         ACTION3 = case_when(is.na(AMENDMENT_NUMBER) & !is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE)),
                             is.na(AMENDMENT_NUMBER) & is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION)),
                             !is.na(AMENDMENT_NUMBER) & is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", AMENDMENT_NUMBER)),
                             !is.na(AMENDMENT_NUMBER) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE, " ", AMENDMENT_NUMBER))),
         START_YEAR3 = format(START_DATE2, "%Y"),
         VALUE_TYPE3 = case_when(MANAGEMENT_TYPE_USE == 'MINIMUM SIZE LIMIT' ~ paste0("Minimum", str_to_title(VALUE_TYPE)),
                                 MANAGEMENT_TYPE_USE == 'MAXIMUM SIZE LIMIT' ~ paste0("Maximum", str_to_title(VALUE_TYPE))))


#Create table
size_tab4 <- size_tab3 %>%
  select(COMMON_NAME_USE, SECTOR3, ZONE3, START_YEAR3, START_DATE3, END_DATE3, VALUE3, VALUE_TYPE3, FR_CITATION, ACTION3)%>%
  do(tab = flextable(.[1:10]) %>%
       set_header_labels(SECTOR3 = "Fishery",
                         ZONE3 = "Region Affected",
                         START_YEAR3 = "Start Year",
                         START_DATE3 = "Effective Date",
                         END_DATE3 = "End Date",
                         VALUE3 = "Size Limit",
                         VALUE_TYPE3 = "Length Type",
                         FR_CITATION = "FR Reference(s)",
                         ACTION3 = "Amendment Number or Rule Type",
                         COMMON_NAME_USE = "Species"))

size_tab4$tab[[1]]


# Gulf red snapper tables

spp1 = 'SNAPPER, RED'
region1 = 'GULF OF MEXICO'

# Table for Bag Limits
#Filter data and combine column information
bag_tab5 <- mh_tables_use %>%
  filter(COMMON_NAME_USE == spp1, REGION == region1, MANAGEMENT_TYPE_USE == 'BAG LIMIT') %>%
  filter(NEVER_IMPLEMENTED == 0) %>%
  ungroup() %>%
  mutate(START_DATE4 = format(START_DATE2, "%m/%d/%Y"),
         END_DATE4 = format(END_DATE2, "%m/%d/%Y"),
         SECTOR4 = str_to_title(paste0(SECTOR_USE, "\n", SUBSECTOR_USE)),
         ZONE4 = str_to_title(paste0(REGION, "\n", ZONE_USE)),
         VALUE4 = case_when(FLAG == 'YES' ~ paste0(VALUE, " ", tolower(VALUE_UNITS), "*"),
                            FLAG == 'NO' ~ paste0(VALUE, " ", tolower(VALUE_UNITS))),
         ACTION4 = case_when(is.na(AMENDMENT_NUMBER) & !is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE)),
                             is.na(AMENDMENT_NUMBER) & is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION)),
                             !is.na(AMENDMENT_NUMBER) & is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", AMENDMENT_NUMBER)),
                             !is.na(AMENDMENT_NUMBER) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE, " ", AMENDMENT_NUMBER))),
         START_YEAR4 = format(START_DATE2, "%Y"))

#Create table
bag_tab6 <- bag_tab5 %>%
  select(COMMON_NAME_USE, SECTOR4, ZONE4, MANAGEMENT_TYPE_USE, START_YEAR4, START_DATE4, END_DATE4, VALUE4, FR_CITATION, ACTION4)%>%
  group_by(COMMON_NAME_USE, ZONE5, SECTOR5, MANAGEMENT_TYPE_USE) %>%
  do(tab = flextable(.[1:10]) %>%
       set_header_labels(SECTOR4 = "Fishery",
                         ZONE4 = "Region Affected",
                         START_YEAR4 = "Start Year",
                         START_DATE4 = "Effective Date",
                         END_DATE4 = "End Date",
                         VALUE4 = "Bag Limit Per Person/Day",
                         FR_CITATION = "FR Reference(s)",
                         ACTION4 = "Amendment Number or Rule Type",
                         COMMON_NAME_USE = "Species",
                         MANAGEMENT_TYPE_USE = "Management Type"))

bag_tab6$tab[[1]]

# Table for Crew Bag Limits
#Filter data and combine column information
crewbag_tab <- mh_tables_use %>%
  filter(COMMON_NAME_USE == spp1, REGION == region1, MANAGEMENT_TYPE_USE == 'CREW BAG LIMIT') %>%
  filter(NEVER_IMPLEMENTED == 0) %>%
  ungroup() %>%
  mutate(START_DATE0 = format(START_DATE2, "%m/%d/%Y"),
         END_DATE0 = format(END_DATE2, "%m/%d/%Y"),
         SECTOR0 = str_to_title(paste0(SECTOR_USE, "\n", SUBSECTOR_USE)),
         ZONE0 = str_to_title(paste0(REGION, "\n", ZONE_USE)),
         VALUE0 = case_when(FLAG == 'YES' ~ paste0(VALUE, " ", tolower(VALUE_UNITS), "*"),
                            FLAG == 'NO' ~ paste0(VALUE, " ", tolower(VALUE_UNITS))),
         ACTION0 = case_when(is.na(AMENDMENT_NUMBER) & !is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE)),
                             is.na(AMENDMENT_NUMBER) & is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION)),
                             !is.na(AMENDMENT_NUMBER) & is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", AMENDMENT_NUMBER)),
                             !is.na(AMENDMENT_NUMBER) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE, " ", AMENDMENT_NUMBER))),
         START_YEAR0 = format(START_DATE2, "%Y"))

#Create table
crewbag_tab1 <- crewbag_tab %>%
  select(COMMON_NAME_USE, SECTOR0, ZONE0, MANAGEMENT_TYPE_USE, START_YEAR0, START_DATE0, END_DATE0, VALUE0, FR_CITATION, ACTION0)%>%
  group_by(COMMON_NAME_USE, ZONE0, SECTOR0, MANAGEMENT_TYPE_USE) %>%
  do(tab = flextable(.[1:10]) %>%
       set_header_labels(SECTOR0 = "Fishery",
                         ZONE0 = "Region Affected",
                         START_YEAR0 = "Start Year",
                         START_DATE0 = "Effective Date",
                         END_DATE0 = "End Date",
                         VALUE0 = "Bag Limit Per Person/Day",
                         FR_CITATION = "FR Reference(s)",
                         ACTION0 = "Amendment Number or Rule Type",
                         COMMON_NAME_USE = "Species",
                         MANAGEMENT_TYPE_USE = "Management Type"))

crewbag_tab1$tab[[1]]


#Table for Recreational Size Limits
#Filter data and combine column information
size_tab7 <- mh_tables_use %>%
  filter(COMMON_NAME_USE == spp1, REGION == region1, MANAGEMENT_CATEGORY == 'SELECTIVITY CONTROLS', SECTOR_USE == 'RECREATIONAL') %>%
  filter(NEVER_IMPLEMENTED == 0) %>%
  ungroup() %>%
  mutate(START_DATE5 = format(START_DATE2, "%m/%d/%Y"),
         END_DATE5 = format(END_DATE2, "%m/%d/%Y"),
         SECTOR5 = str_to_title(paste0(SECTOR_USE, "\n", SUBSECTOR_USE)),
         ZONE5 = str_to_title(paste0(REGION, "\n", ZONE_USE)),
         VALUE5 = case_when(FLAG == 'YES' ~ paste0(VALUE, " ", tolower(VALUE_UNITS), "*"),
                            FLAG == 'NO' ~ paste0(VALUE, " ", tolower(VALUE_UNITS))),
         ACTION5 = case_when(is.na(AMENDMENT_NUMBER) & !is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE)),
                             is.na(AMENDMENT_NUMBER) & is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION)),
                             !is.na(AMENDMENT_NUMBER) & is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", AMENDMENT_NUMBER)),
                             !is.na(AMENDMENT_NUMBER) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE, " ", AMENDMENT_NUMBER))),
         START_YEAR5 = format(START_DATE2, "%Y"),
         VALUE_TYPE5 = case_when(MANAGEMENT_TYPE_USE == 'MINIMUM SIZE LIMIT' ~ paste0("Minimum", str_to_title(VALUE_TYPE)),
                                 MANAGEMENT_TYPE_USE == 'MAXIMUM SIZE LIMIT' ~ paste0("Maximum", str_to_title(VALUE_TYPE))))


#Create table
size_tab8 <- size_tab7 %>%
  select(COMMON_NAME_USE, SECTOR5, ZONE5, START_YEAR5, START_DATE5, END_DATE5, VALUE5, VALUE_TYPE5, FR_CITATION, ACTION5)%>%
  do(tab = flextable(.[1:10]) %>%
       set_header_labels(SECTOR5 = "Fishery",
                         ZONE5 = "Region Affected",
                         START_YEAR5 = "Start Year",
                         START_DATE5 = "Effective Date",
                         END_DATE5 = "End Date",
                         VALUE5 = "Size Limit",
                         VALUE_TYPE5 = "Length Type",
                         FR_CITATION = "FR Reference(s)",
                         ACTION5 = "Amendment Number or Rule Type",
                         COMMON_NAME_USE = "Species"))

size_tab8$tab[[1]]


#Table for Commercial Size Limits
#Filter data and combine column information
size_tab9 <- mh_tables_use %>%
  filter(COMMON_NAME_USE == spp1, REGION == region1, MANAGEMENT_CATEGORY == 'SELECTIVITY CONTROLS', SECTOR_USE == 'COMMERCIAL') %>%
  filter(NEVER_IMPLEMENTED == 0) %>%
  ungroup() %>%
  mutate(START_DATE6 = format(START_DATE2, "%m/%d/%Y"),
         END_DATE6 = format(END_DATE2, "%m/%d/%Y"),
         SECTOR6 = str_to_title(paste0(SECTOR_USE, "\n", SUBSECTOR_USE)),
         ZONE6 = str_to_title(paste0(REGION, "\n", ZONE_USE)),
         VALUE6 = case_when(FLAG == 'YES' ~ paste0(VALUE, " ", tolower(VALUE_UNITS), "*"),
                            FLAG == 'NO' ~ paste0(VALUE, " ", tolower(VALUE_UNITS))),
         ACTION6 = case_when(is.na(AMENDMENT_NUMBER) & !is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE)),
                             is.na(AMENDMENT_NUMBER) & is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION)),
                             !is.na(AMENDMENT_NUMBER) & is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", AMENDMENT_NUMBER)),
                             !is.na(AMENDMENT_NUMBER) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE, " ", AMENDMENT_NUMBER))),
         START_YEAR6 = format(START_DATE2, "%Y"),
         VALUE_TYPE6 = case_when(MANAGEMENT_TYPE_USE == 'MINIMUM SIZE LIMIT' ~ paste0("Minimum", str_to_title(VALUE_TYPE)),
                                 MANAGEMENT_TYPE_USE == 'MAXIMUM SIZE LIMIT' ~ paste0("Maximum", str_to_title(VALUE_TYPE))))


#Create table
size_tab10 <- size_tab9 %>%
  select(COMMON_NAME_USE, SECTOR6, ZONE6, START_YEAR6, START_DATE6, END_DATE6, VALUE6, VALUE_TYPE6, FR_CITATION, ACTION6)%>%
  do(tab = flextable(.[1:10]) %>%
       set_header_labels(SECTOR6 = "Fishery",
                         ZONE6 = "Region Affected",
                         START_YEAR6 = "Start Year",
                         START_DATE6 = "Effective Date",
                         END_DATE6 = "End Date",
                         VALUE6 = "Size Limit",
                         VALUE_TYPE6 = "Length Type",
                         FR_CITATION = "FR Reference(s)",
                         ACTION6 = "Amendment Number or Rule Type",
                         COMMON_NAME_USE = "Species"))

size_tab10$tab[[1]]


# Gulf Spanish mackerel tables

spp2 = 'MACKEREL, SPANISH'
region2 = 'GULF OF MEXICO'
zone2 = 'GULF MIGRATORY GROUP SPANISH MACKEREL'
zone02 = 'ALL'

# Table for Bag Limits
#Filter data and combine column information
bag_tab11 <- mh_tables_use %>%
  filter(COMMON_NAME_USE == spp2, REGION == region2, ZONE_USE == zone2, MANAGEMENT_TYPE_USE == 'BAG LIMIT') %>%
  filter(NEVER_IMPLEMENTED == 0) %>%
  ungroup() %>%
  mutate(START_DATE7 = format(START_DATE2, "%m/%d/%Y"),
         END_DATE7 = format(END_DATE2, "%m/%d/%Y"),
         SECTOR7 = str_to_title(paste0(SECTOR_USE, "\n", SUBSECTOR_USE)),
         ZONE7 = str_to_title(paste0(REGION, "\n", ZONE_USE)),
         VALUE7 = case_when(FLAG == 'YES' ~ paste0(VALUE, " ", tolower(VALUE_UNITS), "*"),
                            FLAG == 'NO' ~ paste0(VALUE, " ", tolower(VALUE_UNITS))),
         ACTION7 = case_when(is.na(AMENDMENT_NUMBER) & !is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE)),
                             is.na(AMENDMENT_NUMBER) & is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION)),
                             !is.na(AMENDMENT_NUMBER) & is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", AMENDMENT_NUMBER)),
                             !is.na(AMENDMENT_NUMBER) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE, " ", AMENDMENT_NUMBER))),
         START_YEAR7 = format(START_DATE2, "%Y"))

#Create table
bag_tab12 <- bag_tab11 %>%
  select(COMMON_NAME_USE, SECTOR7, ZONE7, MANAGEMENT_TYPE_USE, START_YEAR7, START_DATE7, END_DATE7, VALUE7, FR_CITATION, ACTION7)%>%
  arrange(ZONE7) %>%
  do(tab = flextable(.[1:10]) %>%
       set_header_labels(SECTOR7 = "Fishery",
                         ZONE7 = "Region Affected",
                         START_YEAR7 = "Start Year",
                         START_DATE7 = "Effective Date",
                         END_DATE7 = "End Date",
                         VALUE7 = "Bag Limit Per Person/Day",
                         FR_CITATION = "FR Reference(s)",
                         ACTION7 = "Amendment Number or Rule Type",
                         COMMON_NAME_USE = "Species",
                         MANAGEMENT_TYPE_USE = "Management Type"))

bag_tab12$tab[[1]]

#Table for Recreational Size Limits
#Filter data and combine column information
size_tab13 <- mh_tables_use %>%
  filter(COMMON_NAME_USE == spp2, REGION == region2, MANAGEMENT_CATEGORY == 'SELECTIVITY CONTROLS', SECTOR_USE == 'RECREATIONAL') %>%
  filter(NEVER_IMPLEMENTED == 0) %>%
  ungroup() %>%
  mutate(START_DATE8 = format(START_DATE2, "%m/%d/%Y"),
         END_DATE8 = format(END_DATE2, "%m/%d/%Y"),
         SECTOR8 = str_to_title(paste0(SECTOR_USE, "\n", SUBSECTOR_USE)),
         ZONE8 = str_to_title(paste0(REGION, "\n", ZONE_USE)),
         VALUE8 = case_when(FLAG == 'YES' ~ paste0(VALUE, " ", tolower(VALUE_UNITS), "*"),
                            FLAG == 'NO' ~ paste0(VALUE, " ", tolower(VALUE_UNITS))),
         ACTION8 = case_when(is.na(AMENDMENT_NUMBER) & !is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE)),
                             is.na(AMENDMENT_NUMBER) & is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION)),
                             !is.na(AMENDMENT_NUMBER) & is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", AMENDMENT_NUMBER)),
                             !is.na(AMENDMENT_NUMBER) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE, " ", AMENDMENT_NUMBER))),
         START_YEAR8 = format(START_DATE2, "%Y"),
         VALUE_TYPE8 = case_when(MANAGEMENT_TYPE_USE == 'MINIMUM SIZE LIMIT' ~ paste0("Minimum", str_to_title(VALUE_TYPE)),
                                 MANAGEMENT_TYPE_USE == 'MAXIMUM SIZE LIMIT' ~ paste0("Maximum", str_to_title(VALUE_TYPE))))


#Create table
size_tab14 <- size_tab13 %>%
  select(COMMON_NAME_USE, SECTOR8, ZONE8, START_YEAR8, START_DATE8, END_DATE8, VALUE8, VALUE_TYPE8, FR_CITATION, ACTION8)%>%
  do(tab = flextable(.[1:10]) %>%
       set_header_labels(SECTOR8 = "Fishery",
                         ZONE8 = "Region Affected",
                         START_YEAR8 = "Start Year",
                         START_DATE8 = "Effective Date",
                         END_DATE8 = "End Date",
                         VALUE8 = "Size Limit",
                         VALUE_TYPE8 = "Length Type",
                         FR_CITATION = "FR Reference(s)",
                         ACTION8 = "Amendment Number or Rule Type",
                         COMMON_NAME_USE = "Species"))

size_tab14$tab[[1]]


#Table for Commercial Size Limits
#Filter data and combine column information
size_tab15 <- mh_tables_use %>%
  filter(COMMON_NAME_USE == spp2, REGION == region2, MANAGEMENT_CATEGORY == 'SELECTIVITY CONTROLS', SECTOR_USE == 'COMMERCIAL') %>%
  filter(NEVER_IMPLEMENTED == 0) %>%
  ungroup() %>%
  mutate(START_DATE9 = format(START_DATE2, "%m/%d/%Y"),
         END_DATE9 = format(END_DATE2, "%m/%d/%Y"),
         SECTOR9 = str_to_title(paste0(SECTOR_USE, "\n", SUBSECTOR_USE)),
         ZONE9 = str_to_title(paste0(REGION, "\n", ZONE_USE)),
         VALUE9 = case_when(FLAG == 'YES' ~ paste0(VALUE, " ", tolower(VALUE_UNITS), "*"),
                            FLAG == 'NO' ~ paste0(VALUE, " ", tolower(VALUE_UNITS))),
         ACTION9 = case_when(is.na(AMENDMENT_NUMBER) & !is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE)),
                             is.na(AMENDMENT_NUMBER) & is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION)),
                             !is.na(AMENDMENT_NUMBER) & is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", AMENDMENT_NUMBER)),
                             !is.na(AMENDMENT_NUMBER) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE, " ", AMENDMENT_NUMBER))),
         START_YEAR9 = format(START_DATE2, "%Y"),
         VALUE_TYPE9 = case_when(MANAGEMENT_TYPE_USE == 'MINIMUM SIZE LIMIT' ~ paste0("Minimum", str_to_title(VALUE_TYPE)),
                                 MANAGEMENT_TYPE_USE == 'MAXIMUM SIZE LIMIT' ~ paste0("Maximum", str_to_title(VALUE_TYPE))))


#Create table
size_tab16 <- size_tab15 %>%
  select(COMMON_NAME_USE, SECTOR9, ZONE9, START_YEAR9, START_DATE9, END_DATE9, VALUE9, VALUE_TYPE9, FR_CITATION, ACTION9)%>%
  do(tab = flextable(.[1:10]) %>%
       set_header_labels(SECTOR9 = "Fishery",
                         ZONE9 = "Region Affected",
                         START_YEAR9 = "Start Year",
                         START_DATE9 = "Effective Date",
                         END_DATE9 = "End Date",
                         VALUE9 = "Size Limit",
                         VALUE_TYPE9 = "Length Type",
                         FR_CITATION = "FR Reference(s)",
                         ACTION9 = "Amendment Number or Rule Type",
                         COMMON_NAME_USE = "Species"))

size_tab16$tab[[1]]


# Table for Recreational Quota
#Filter data and combine column information
recquo_tab27 <- mh_tables_use %>%
  filter(COMMON_NAME_USE == spp2, REGION == region2, MANAGEMENT_TYPE_USE == 'QUOTA', SECTOR_USE == 'RECREATIONAL') %>%
  filter(NEVER_IMPLEMENTED == 0, REG_REMOVED == 0) %>%
  ungroup() %>%
  mutate(START_DATE15 = format(START_DATE2, "%m/%d/%Y"),
         END_DATE15 = format(END_DATE2, "%m/%d/%Y"),
         SECTOR15 = str_to_title(paste0(SECTOR_USE, "\n", SUBSECTOR_USE)),
         ZONE15 = str_to_title(paste0(REGION, "\n", ZONE_USE)),
         VALUE15 = case_when(FLAG == 'YES' ~ paste0(VALUE, " ", tolower(VALUE_UNITS), "*"),
                             FLAG == 'NO' ~ paste0(VALUE, " ", tolower(VALUE_UNITS))),
         ACTION15 = case_when(is.na(AMENDMENT_NUMBER) & !is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE)),
                              is.na(AMENDMENT_NUMBER) & is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION)),
                              !is.na(AMENDMENT_NUMBER) & is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", AMENDMENT_NUMBER)),
                              !is.na(AMENDMENT_NUMBER) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE, " ", AMENDMENT_NUMBER))),
         START_YEAR15 = format(START_DATE2, "%Y"))

#Create table
recquo_tab28 <- recquo_tab27 %>%
  select(COMMON_NAME_USE, SECTOR15, ZONE15, MANAGEMENT_TYPE_USE, START_YEAR15, START_DATE15, END_DATE15, VALUE15, VALUE_TYPE, FR_CITATION, ACTION15)%>%
  arrange(ZONE15) %>%
  do(tab = flextable(.[1:10]) %>%
       set_header_labels(SECTOR15 = "Fishery",
                         ZONE15 = "Region Affected",
                         START_YEAR15 = "Start Year",
                         START_DATE15 = "Effective Date",
                         END_DATE15 = "End Date",
                         VALUE15 = "Quota",
                         VALUE_TYPE = "Weight Type",
                         FR_CITATION = "FR Reference(s)",
                         ACTION15 = "Amendment Number or Rule Type",
                         COMMON_NAME_USE = "Species",
                         MANAGEMENT_TYPE_USE = "Management Type"))

recquo_tab28$tab[[1]]


# South Atlantic gray triggerfish tables

spp3 = 'TRIGGERFISH, GRAY'
region3 = 'SOUTH ATLANTIC'

# Table for Bag Limits
#Filter data and combine column information
bag_tab17 <- mh_tables_use %>%
  filter(COMMON_NAME_USE == spp3, REGION == region3, MANAGEMENT_TYPE_USE == 'BAG LIMIT') %>%
  filter(NEVER_IMPLEMENTED == 0) %>%
  ungroup() %>%
  mutate(START_DATE10 = format(START_DATE2, "%m/%d/%Y"),
         END_DATE10 = format(END_DATE2, "%m/%d/%Y"),
         SECTOR10 = str_to_title(paste0(SECTOR_USE, "\n", SUBSECTOR_USE)),
         ZONE10 = str_to_title(paste0(REGION, "\n", ZONE_USE)),
         VALUE10 = case_when(FLAG == 'YES' ~ paste0(VALUE, " ", tolower(VALUE_UNITS), "*"),
                            FLAG == 'NO' ~ paste0(VALUE, " ", tolower(VALUE_UNITS))),
         ACTION10 = case_when(is.na(AMENDMENT_NUMBER) & !is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE)),
                             is.na(AMENDMENT_NUMBER) & is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION)),
                             !is.na(AMENDMENT_NUMBER) & is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", AMENDMENT_NUMBER)),
                             !is.na(AMENDMENT_NUMBER) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE, " ", AMENDMENT_NUMBER))),
         START_YEAR10 = format(START_DATE2, "%Y"))

#Create table
bag_tab18 <- bag_tab17 %>%
  select(COMMON_NAME_USE, SECTOR10, ZONE10, MANAGEMENT_TYPE_USE, START_YEAR10, START_DATE10, END_DATE10, VALUE10, FR_CITATION, ACTION10)%>%
  arrange(ZONE10) %>%
  do(tab = flextable(.[1:10]) %>%
       set_header_labels(SECTOR10 = "Fishery",
                         ZONE10 = "Region Affected",
                         START_YEAR10 = "Start Year",
                         START_DATE10 = "Effective Date",
                         END_DATE10 = "End Date",
                         VALUE10 = "Bag Limit Per Person/Day",
                         FR_CITATION = "FR Reference(s)",
                         ACTION10 = "Amendment Number or Rule Type",
                         COMMON_NAME_USE = "Species",
                         MANAGEMENT_TYPE_USE = "Management Type"))

bag_tab18$tab[[1]]

#Table for Recreational Size Limits
#Filter data and combine column information
size_tab19 <- mh_tables_use %>%
  filter(COMMON_NAME_USE == spp3, REGION == region3, MANAGEMENT_CATEGORY == 'SELECTIVITY CONTROLS', SECTOR_USE == 'RECREATIONAL') %>%
  filter(NEVER_IMPLEMENTED == 0) %>%
  ungroup() %>%
  mutate(START_DATE11 = format(START_DATE2, "%m/%d/%Y"),
         END_DATE11 = format(END_DATE2, "%m/%d/%Y"),
         SECTOR11 = str_to_title(paste0(SECTOR_USE, "\n", SUBSECTOR_USE)),
         ZONE11 = str_to_title(paste0(REGION, "\n", ZONE_USE)),
         VALUE11 = case_when(FLAG == 'YES' ~ paste0(VALUE, " ", tolower(VALUE_UNITS), "*"),
                            FLAG == 'NO' ~ paste0(VALUE, " ", tolower(VALUE_UNITS))),
         ACTION11 = case_when(is.na(AMENDMENT_NUMBER) & !is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE)),
                             is.na(AMENDMENT_NUMBER) & is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION)),
                             !is.na(AMENDMENT_NUMBER) & is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", AMENDMENT_NUMBER)),
                             !is.na(AMENDMENT_NUMBER) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE, " ", AMENDMENT_NUMBER))),
         START_YEAR11 = format(START_DATE2, "%Y"),
         VALUE_TYPE11 = case_when(MANAGEMENT_TYPE_USE == 'MINIMUM SIZE LIMIT' ~ paste0("Minimum", str_to_title(VALUE_TYPE)),
                                 MANAGEMENT_TYPE_USE == 'MAXIMUM SIZE LIMIT' ~ paste0("Maximum", str_to_title(VALUE_TYPE))))


#Create table
size_tab20 <- size_tab19 %>%
  select(COMMON_NAME_USE, SECTOR11, ZONE11, START_YEAR11, START_DATE11, END_DATE11, VALUE11, VALUE_TYPE11, FR_CITATION, ACTION11)%>%
  arrange(ZONE11)%>%
  do(tab = flextable(.[1:10]) %>%
       set_header_labels(SECTOR11 = "Fishery",
                         ZONE11 = "Region Affected",
                         START_YEAR11 = "Start Year",
                         START_DATE11 = "Effective Date",
                         END_DATE11 = "End Date",
                         VALUE11 = "Size Limit",
                         VALUE_TYPE11 = "Length Type",
                         FR_CITATION = "FR Reference(s)",
                         ACTION11 = "Amendment Number or Rule Type",
                         COMMON_NAME_USE = "Species"))

size_tab20$tab[[1]]


#Table for Commercial Size Limits
#Filter data and combine column information
size_tab21 <- mh_tables_use %>%
  filter(COMMON_NAME_USE == spp3, REGION == region3, MANAGEMENT_CATEGORY == 'SELECTIVITY CONTROLS', SECTOR_USE == 'COMMERCIAL') %>%
  filter(NEVER_IMPLEMENTED == 0) %>%
  ungroup() %>%
  mutate(START_DATE12 = format(START_DATE2, "%m/%d/%Y"),
         END_DATE12 = format(END_DATE2, "%m/%d/%Y"),
         SECTOR12 = str_to_title(paste0(SECTOR_USE, "\n", SUBSECTOR_USE)),
         ZONE12 = str_to_title(paste0(REGION, "\n", ZONE_USE)),
         VALUE12 = case_when(FLAG == 'YES' ~ paste0(VALUE, " ", tolower(VALUE_UNITS), "*"),
                            FLAG == 'NO' ~ paste0(VALUE, " ", tolower(VALUE_UNITS))),
         ACTION12 = case_when(is.na(AMENDMENT_NUMBER) & !is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE)),
                             is.na(AMENDMENT_NUMBER) & is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION)),
                             !is.na(AMENDMENT_NUMBER) & is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", AMENDMENT_NUMBER)),
                             !is.na(AMENDMENT_NUMBER) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE, " ", AMENDMENT_NUMBER))),
         START_YEAR12 = format(START_DATE2, "%Y"),
         VALUE_TYPE12 = case_when(MANAGEMENT_TYPE_USE == 'MINIMUM SIZE LIMIT' ~ paste0("Minimum", str_to_title(VALUE_TYPE)),
                                 MANAGEMENT_TYPE_USE == 'MAXIMUM SIZE LIMIT' ~ paste0("Maximum", str_to_title(VALUE_TYPE))))


#Create table
size_tab22 <- size_tab21 %>%
  select(COMMON_NAME_USE, SECTOR12, ZONE12, START_YEAR12, START_DATE12, END_DATE12, VALUE12, VALUE_TYPE12, FR_CITATION, ACTION12)%>%
  arrange(ZONE12)%>%
  do(tab = flextable(.[1:10]) %>%
       set_header_labels(SECTOR12 = "Fishery",
                         ZONE12 = "Region Affected",
                         START_YEAR12 = "Start Year",
                         START_DATE12 = "Effective Date",
                         END_DATE12 = "End Date",
                         VALUE12 = "Size Limit",
                         VALUE_TYPE12 = "Length Type",
                         FR_CITATION = "FR Reference(s)",
                         ACTION12 = "Amendment Number or Rule Type",
                         COMMON_NAME_USE = "Species"))

size_tab22$tab[[1]]

#Table for Commercial Trip Limits
#Filter data and combine column information
bag_tab23 <- mh_tables_use %>%
  filter(COMMON_NAME_USE == spp3, REGION == region3, MANAGEMENT_TYPE_USE == 'TRIP LIMIT') %>%
  filter(NEVER_IMPLEMENTED == 0) %>%
  ungroup() %>%
  mutate(START_DATE13 = format(START_DATE2, "%m/%d/%Y"),
         END_DATE13 = format(END_DATE2, "%m/%d/%Y"),
         SECTOR13 = str_to_title(paste0(SECTOR_USE, "\n", SUBSECTOR_USE)),
         ZONE13 = str_to_title(paste0(REGION, "\n", ZONE_USE)),
         VALUE13 = case_when(FLAG == 'YES' ~ paste0(VALUE, " ", tolower(VALUE_UNITS), "*"),
                             FLAG == 'NO' ~ paste0(VALUE, " ", tolower(VALUE_UNITS))),
         ACTION13 = case_when(is.na(AMENDMENT_NUMBER) & !is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE)),
                              is.na(AMENDMENT_NUMBER) & is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION)),
                              !is.na(AMENDMENT_NUMBER) & is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", AMENDMENT_NUMBER)),
                              !is.na(AMENDMENT_NUMBER) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE, " ", AMENDMENT_NUMBER))),
         START_YEAR13 = format(START_DATE2, "%Y"))

#Create table
bag_tab24 <- bag_tab23 %>%
  select(COMMON_NAME_USE, SECTOR13, ZONE13, MANAGEMENT_TYPE_USE, START_YEAR13, START_DATE13, END_DATE13, VALUE13, FR_CITATION, ACTION13)%>%
  do(tab = flextable(.[1:10]) %>%
       set_header_labels(SECTOR13 = "Fishery",
                         ZONE13 = "Region Affected",
                         START_YEAR13 = "Start Year",
                         START_DATE13 = "Effective Date",
                         END_DATE13 = "End Date",
                         VALUE13 = "Bag Limit Per Person/Day",
                         FR_CITATION = "FR Reference(s)",
                         ACTION13 = "Amendment Number or Rule Type",
                         COMMON_NAME_USE = "Species",
                         MANAGEMENT_TYPE_USE = "Management Type"))

bag_tab24$tab[[1]]

# Table for Recreational ACL
#Filter data and combine column information
recacl_tab25 <- mh_tables_use %>%
  filter(COMMON_NAME_USE == spp3, REGION == region3, MANAGEMENT_TYPE_USE == 'ACL', SECTOR_USE == 'RECREATIONAL') %>%
  filter(NEVER_IMPLEMENTED == 0) %>%
  ungroup() %>%
  mutate(START_DATE14 = format(START_DATE2, "%m/%d/%Y"),
         END_DATE14 = format(END_DATE2, "%m/%d/%Y"),
         SECTOR14 = str_to_title(paste0(SECTOR_USE, "\n", SUBSECTOR_USE)),
         ZONE14 = str_to_title(paste0(REGION, "\n", ZONE_USE)),
         VALUE14 = case_when(FLAG == 'YES' ~ paste0(VALUE, " ", tolower(VALUE_UNITS), "*"),
                             FLAG == 'NO' ~ paste0(VALUE, " ", tolower(VALUE_UNITS))),
         ACTION14 = case_when(is.na(AMENDMENT_NUMBER) & !is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE)),
                              is.na(AMENDMENT_NUMBER) & is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION)),
                              !is.na(AMENDMENT_NUMBER) & is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", AMENDMENT_NUMBER)),
                              !is.na(AMENDMENT_NUMBER) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE, " ", AMENDMENT_NUMBER))),
         START_YEAR14 = format(START_DATE2, "%Y"))

#Create table
recacl_tab26 <- recacl_tab25 %>%
  select(COMMON_NAME_USE, SECTOR14, ZONE14, MANAGEMENT_TYPE_USE, START_YEAR14, START_DATE14, END_DATE14, VALUE14, VALUE_TYPE, FR_CITATION, ACTION14)%>%
  arrange(ZONE14) %>%
  do(tab = flextable(.[1:10]) %>%
       set_header_labels(SECTOR14 = "Fishery",
                         ZONE14 = "Region Affected",
                         START_YEAR14 = "Start Year",
                         START_DATE14 = "Effective Date",
                         END_DATE14 = "End Date",
                         VALUE14 = "ACL",
                         VALUE_TYPE = "Weight Type",
                         FR_CITATION = "FR Reference(s)",
                         ACTION14 = "Amendment Number or Rule Type",
                         COMMON_NAME_USE = "Species",
                         MANAGEMENT_TYPE_USE = "Management Type"))

recacl_tab26$tab[[1]]
