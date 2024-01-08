# Caribbean Yellowtail
librarian::shelf(here, tidyverse, gt, flextable, officer, dplyr, gt, officer)

mh_tables_use <- readRDS(here("ODM-MH-Data_log", "data", "results", "MH_DL_2023Nov30.RDS"))

here::i_am('Examples/Tables/MH_tables.R')
source(here('ODM-MH-Data_log', 'main_MH_prep.R'))

# Gulf red grouper tables
spp = 'SNAPPER, YELLOWTAIL'
region = 'CARIBBEAN'

#### Yelllowtail snapper ####
# Table for Bag Limits
#Filter data and combine column information
cytsbag_tab1 <- mh_data_log %>%
  filter(COMMON_NAME_USE == spp, REGION == region, MANAGEMENT_TYPE_USE == 'BAG LIMIT') %>%
  filter(NEVER_IMPLEMENTED == 0) %>%
  arrange(FR_CITATION) %>%
  ungroup() %>%
  mutate(START_DATE1 = format(START_DATE2, "%m/%d/%Y"),
         END_DATE1 = paste0("Present"),
         SECTOR1 = str_to_title(paste0(SECTOR_USE, "\n", SUBSECTOR_USE)),
         ZONE1 = str_to_title(paste0(REGION, "\n", ZONE_USE)),
         VALUE1 = case_when(FLAG == 'YES' ~ paste0(VALUE, " ", tolower(VALUE_RATE), "*"),
                            FLAG == 'NO' ~ paste0(VALUE, " ", tolower(VALUE_RATE))),
         ACTION1 = case_when(is.na(AMENDMENT_NUMBER) & !is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE)),
                             is.na(AMENDMENT_NUMBER) & is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION)),
                             !is.na(AMENDMENT_NUMBER) & is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", AMENDMENT_NUMBER)),
                             !is.na(AMENDMENT_NUMBER) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE, " ", AMENDMENT_NUMBER))),
         START_YEAR1 = format(START_DATE2, "%Y"),
         COMMON_NAME_USE = str_to_title(paste0(COMMON_NAME_USE)),
         MANAGEMENT_TYPE_USE = str_to_title(paste0(MANAGEMENT_TYPE_USE)))

#Create table
cytsbag_tab2 <- cytsbag_tab1 %>%
  select(COMMON_NAME_USE, SECTOR1, ZONE1, MANAGEMENT_TYPE_USE, START_YEAR1, START_DATE1, END_DATE1, VALUE1, FR_CITATION, ACTION1)%>%
  do(tab = flextable(.[1:10]) %>%
       set_header_labels(SECTOR1 = "Fishery",
                         ZONE1 = "Region Affected",
                         START_YEAR1 = "Start Year",
                         START_DATE1 = "Effective Date",
                         END_DATE1 = "End Date",
                         VALUE1 = "Bag Limit",
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
       align(part = "all", align = "center"))
  
     
cytsbag_tab2$tab[[1]]
print(cytsbag_tab2$tab[[1]], preview = "docx")

#Table for Commercial Size Limits
#Filter data and combine column information
cytssize_tab2 <- mh_data_log %>%
  filter(COMMON_NAME_USE == spp, REGION == region, MANAGEMENT_CATEGORY == 'SELECTIVITY CONTROLS', SECTOR_USE == 'COMMERCIAL') %>%
  filter(NEVER_IMPLEMENTED == 0) %>%
  ungroup() %>%
  mutate(START_DATE4 = format(START_DATE2, "%m/%d/%Y"),
         END_DATE4 = if_else(END_DATE2 == as.Date("2021-12-31"), "Present", format(END_DATE2, "%m/%d/%Y")),
         SECTOR4 = str_to_title(paste0(SECTOR, "\n", SUBSECTOR_USE)),
         ZONE4 = str_to_title(paste0(REGION, "\n", ZONE_USE)),
         VALUE4 = case_when(FLAG == 'YES' ~ paste0(VALUE, " ", tolower(VALUE_UNITS), "*"),
                            FLAG == 'NO' ~ paste0(VALUE, " ", tolower(VALUE_UNITS))),
         ACTION4 = case_when(is.na(AMENDMENT_NUMBER) & !is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE)),
                             is.na(AMENDMENT_NUMBER) & is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION)),
                             !is.na(AMENDMENT_NUMBER) & is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", AMENDMENT_NUMBER)),
                             !is.na(AMENDMENT_NUMBER) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE, " ", AMENDMENT_NUMBER))),
         START_YEAR4 = format(START_DATE2, "%Y"),
         VALUE_TYPE4 = case_when(MANAGEMENT_TYPE_USE == 'MINIMUM SIZE LIMIT' ~ paste0("Minimum", " ", str_to_title(VALUE_TYPE)),
                                 MANAGEMENT_TYPE_USE == 'MAXIMUM SIZE LIMIT' ~ paste0("Maximum", " ", str_to_title(VALUE_TYPE))),
         COMMON_NAME_USE = str_to_title(paste0(COMMON_NAME_USE)),
         MANAGEMENT_TYPE_USE = str_to_title(paste0(MANAGEMENT_TYPE_USE)))


#Create table
cytssize_tab21 <- cytssize_tab2 %>%
  select(COMMON_NAME_USE, SECTOR4, ZONE4, START_YEAR4, START_DATE4, END_DATE4, VALUE4, VALUE_TYPE4, FR_CITATION, ACTION4)%>%
  arrange(START_DATE4) %>%
  do(tab = flextable(.[1:10]) %>%
       set_header_labels(SECTOR4 = "Fishery",
                         ZONE4 = "Region Affected",
                         START_YEAR4 = "Start Year",
                         START_DATE4 = "Effective Date",
                         END_DATE4 = "End Date",
                         VALUE4 = "Size Limit",
                         VALUE_TYPE4 = "Length Type",
                         FR_CITATION = "FR Reference(s)",
                         ACTION3 = "Amendment Number or Rule Type",
                         COMMON_NAME_USE = "Species") %>%
       merge_v(j = 1, part = "body") %>%
       merge_v(j = 2, part = "body") %>%
       merge_v(j = 3, part = "body") %>%
       merge_v(j = 4, part = "body") %>%
       theme_box() %>%
       hline_top(part = "header", border = fp_border(color = "black", width = 2)) %>%
       hline_bottom(part = "header", border = fp_border(color = "black", width = 2)) %>%
       fontsize(part = "all", size = 12) %>%
       font(part = "all", fontname = "Times New Roman") %>%
       align(part = "all", align = "center"))


cytssize_tab21$tab[[1]]
print(cytssize_tab21$tab[[1]], preview = "docx")

#Prohibited gear regulations
cytsgear_tab <- mh_data_log %>%
  filter(COMMON_NAME_USE == spp, REGION == region, MANAGEMENT_TYPE_USE == 'PROHIBITED GEAR', SECTOR_USE == 'COMMERCIAL') %>%
  filter(NEVER_IMPLEMENTED == 0) %>%
  ungroup() %>%
  mutate(START_DATE1 = format(START_DATE2, "%m/%d/%Y"),
         END_DATE1 = if_else(END_DATE2 == as.Date("2021-12-31"), "Present", format(END_DATE2, "%m/%d/%Y")),
         SECTOR1 = str_to_title(paste0(SECTOR_USE)),
         ZONE1 = str_to_title(paste0(REGION, "\n", ZONE_USE)),
         SUBSECTOR1 = case_when(FLAG == 'YES' ~ str_to_title(paste0(SUBSECTOR_USE, "*")),
                            FLAG == 'NO' ~ str_to_title(paste0(SUBSECTOR_USE))),
         ACTION1 = case_when(is.na(AMENDMENT_NUMBER) & !is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE)),
                             is.na(AMENDMENT_NUMBER) & is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION)),
                             !is.na(AMENDMENT_NUMBER) & is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", AMENDMENT_NUMBER)),
                             !is.na(AMENDMENT_NUMBER) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE, " ", AMENDMENT_NUMBER))),
         START_YEAR1 = format(START_DATE2, "%Y"),
         COMMON_NAME_USE = str_to_title(paste0(COMMON_NAME_USE)),
         MANAGEMENT_TYPE_USE = str_to_title(paste0(MANAGEMENT_TYPE_USE)))


#Create table
cytsgear_tab1 <- cytsgear_tab %>%
  select(COMMON_NAME_USE, ZONE1, SECTOR1, SUBSECTOR1, START_YEAR1, START_DATE1, END_DATE1, FR_CITATION, ACTION1)%>%
  arrange(SUBSECTOR1, START_DATE1) %>%
  do(tab = flextable(.[1:9]) %>%
       set_header_labels(SECTOR1 = "Fishery",
                         ZONE1 = "Region Affected",
                         SUBSECTOR1 = "Gear Type",
                         START_YEAR1 = "Start Year",
                         START_DATE1 = "Effective Date",
                         END_DATE1 = "End Date",
                         FR_CITATION = "FR Reference(s)",
                         ACTION1 = "Amendment Number or Rule Type",
                         COMMON_NAME_USE = "Species") %>%
       merge_v(j = 1, part = "body") %>%
       merge_v(j = 2, part = "body") %>%
       merge_v(j = 3, part = "body") %>%
       merge_v(j = 4, part = "body") %>%
       theme_box() %>%
       hline_top(part = "header", border = fp_border(color = "black", width = 2)) %>%
       hline_bottom(part = "header", border = fp_border(color = "black", width = 2)) %>%
       fontsize(part = "all", size = 12) %>%
       font(part = "all", fontname = "Times New Roman") %>%
       align(part = "all", align = "center"))


cytsgear_tab1$tab[[1]]
print(cytsgear_tab1$tab[[1]], preview = "docx")

#Prohibited gear regulations
cytsgear_tab <- mh_data_log %>%
  filter(COMMON_NAME_USE == spp, REGION == region, MANAGEMENT_TYPE_USE == 'PROHIBITED GEAR', SECTOR_USE == 'RECREATIONAL') %>%
  filter(NEVER_IMPLEMENTED == 0) %>%
  ungroup() %>%
  mutate(START_DATE1 = format(START_DATE2, "%m/%d/%Y"),
         END_DATE1 = if_else(END_DATE2 == as.Date("2021-12-31"), "Present", format(END_DATE2, "%m/%d/%Y")),
         SECTOR1 = str_to_title(paste0(SECTOR_USE)),
         ZONE1 = str_to_title(paste0(REGION, "\n", ZONE_USE)),
         SUBSECTOR1 = case_when(FLAG == 'YES' ~ str_to_title(paste0(SUBSECTOR_USE, "*")),
                                FLAG == 'NO' ~ str_to_title(paste0(SUBSECTOR_USE))),
         ACTION1 = case_when(is.na(AMENDMENT_NUMBER) & !is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE)),
                             is.na(AMENDMENT_NUMBER) & is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION)),
                             !is.na(AMENDMENT_NUMBER) & is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", AMENDMENT_NUMBER)),
                             !is.na(AMENDMENT_NUMBER) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE, " ", AMENDMENT_NUMBER))),
         START_YEAR1 = format(START_DATE2, "%Y"),
         COMMON_NAME_USE = str_to_title(paste0(COMMON_NAME_USE)),
         MANAGEMENT_TYPE_USE = str_to_title(paste0(MANAGEMENT_TYPE_USE)))


#Create table
cytsgear_tab1 <- cytsgear_tab %>%
  select(COMMON_NAME_USE, ZONE1, SECTOR1, SUBSECTOR1, START_YEAR1, START_DATE1, END_DATE1, FR_CITATION, ACTION1)%>%
  arrange(SUBSECTOR1, START_DATE1) %>%
  do(tab = flextable(.[1:9]) %>%
       set_header_labels(SECTOR1 = "Fishery",
                         ZONE1 = "Region Affected",
                         SUBSECTOR1 = "Gear Type",
                         START_YEAR1 = "Start Year",
                         START_DATE1 = "Effective Date",
                         END_DATE1 = "End Date",
                         FR_CITATION = "FR Reference(s)",
                         ACTION1 = "Amendment Number or Rule Type",
                         COMMON_NAME_USE = "Species") %>%
       merge_v(j = 1, part = "body") %>%
       merge_v(j = 2, part = "body") %>%
       merge_v(j = 3, part = "body") %>%
       merge_v(j = 4, part = "body") %>%
       theme_box() %>%
       hline_top(part = "header", border = fp_border(color = "black", width = 2)) %>%
       hline_bottom(part = "header", border = fp_border(color = "black", width = 2)) %>%
       fontsize(part = "all", size = 12) %>%
       font(part = "all", fontname = "Times New Roman") %>%
       align(part = "all", align = "center"))


cytsgear_tab1$tab[[1]]
print(cytsgear_tab1$tab[[1]], preview = "docx")

#ACL regulations commercial
cytsacl_tab <- mh_data_log %>%
  filter(COMMON_NAME_USE == spp, REGION == region, MANAGEMENT_TYPE_USE == 'ACL') %>%
  filter(NEVER_IMPLEMENTED == 0, ZONE_USE != "ST. CROIX MANAGEMENT AREA") %>%
  ungroup() %>%
  mutate(START_DATE1 = format(START_DATE2, "%m/%d/%Y"),
         END_DATE1 = if_else(END_DATE2 == as.Date("2021-12-31"), "Present", format(END_DATE2, "%m/%d/%Y")),
         SECTOR1 = str_to_title(paste0(SECTOR, "\n", SUBSECTOR)),
         ZONE1 = str_to_title(paste0(REGION, "\n", ZONE_USE)),
         VALUE1 = case_when(FLAG == 'YES' ~ paste0(VALUE, " ", tolower(VALUE_UNITS), "*"),
                            FLAG == 'NO' ~ paste0(VALUE, " ", tolower(VALUE_UNITS))),
         ACTION1 = case_when(is.na(AMENDMENT_NUMBER) & !is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE)),
                             is.na(AMENDMENT_NUMBER) & is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION)),
                             !is.na(AMENDMENT_NUMBER) & is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", AMENDMENT_NUMBER)),
                             !is.na(AMENDMENT_NUMBER) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE, " ", AMENDMENT_NUMBER))),
         START_YEAR1 = format(START_DATE2, "%Y"),
         COMMON_NAME_USE = str_to_title(paste0(COMMON_NAME_USE)),
         MANAGEMENT_TYPE_USE = str_to_title(paste0(MANAGEMENT_TYPE_USE)))


#Create table
cytsacl_tab1 <- cytsacl_tab %>%
  select(COMMON_NAME_USE, SECTOR1, ZONE1, START_YEAR1, START_DATE1, END_DATE1, VALUE1, FR_CITATION, ACTION1)%>%
  distinct()%>%
  arrange(START_DATE1, SECTOR1) %>%
  do(tab = flextable(.[1:9]) %>%
       set_header_labels(SECTOR1 = "Fishery",
                         ZONE1 = "Region Affected",
                         VALUE1 = "ACL (round weight)",
                         START_YEAR1 = "Start Year",
                         START_DATE1 = "Effective Date",
                         END_DATE1 = "End Date",
                         FR_CITATION = "FR Reference(s)",
                         ACTION1 = "Amendment Number or Rule Type",
                         COMMON_NAME_USE = "Species") %>%
       merge_v(j = 1, part = "body") %>%
       merge_v(j = 2, part = "body") %>%
       merge_v(j = 3, part = "body") %>%
       merge_v(j = 4, part = "body") %>%
       merge_v(j = 5, part = "body") %>%
       merge_v(j = 6, part = "body") %>%
       theme_box() %>%
       hline_top(part = "header", border = fp_border(color = "black", width = 2)) %>%
       hline_bottom(part = "header", border = fp_border(color = "black", width = 2)) %>%
       fontsize(part = "all", size = 12) %>%
       font(part = "all", fontname = "Times New Roman") %>%
       align(part = "all", align = "center"))


cytsacl_tab1$tab[[1]]
print(cytsacl_tab1$tab[[1]], preview = "docx")

#Closure regulations commercial
cytsclos_tab <- closures_remove_multi %>%
  filter(COMMON_NAME_USE == spp, REGION == region, MANAGEMENT_TYPE_USE == 'CLOSURE') %>%
  filter(NEVER_IMPLEMENTED == 0) %>%
  group_by(VALUE, ZONE1) %>%
  ungroup() %>%
  mutate(START_DATE1 = format(START_DATE2, "%m/%d/%Y"),
         END_DATE1 = if_else(END_DATE2 == as.Date("2021-12-31"), "Present", format(END_DATE2, "%m/%d/%Y")),
         SECTOR1 = str_to_title(paste0(SECTOR, "\n", SUBSECTOR)),
         ZONE1 = str_to_title(paste0(REGION, "\n", ZONE_USE)),
         VALUE1 = case_when(FLAG == 'YES' ~ paste0(VALUE, " ", tolower(VALUE_UNITS), "*"),
                            FLAG == 'NO' ~ paste0(VALUE, " ", tolower(VALUE_UNITS))),
         ACTION1 = case_when(is.na(AMENDMENT_NUMBER) & !is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE)),
                             is.na(AMENDMENT_NUMBER) & is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION)),
                             !is.na(AMENDMENT_NUMBER) & is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", AMENDMENT_NUMBER)),
                             !is.na(AMENDMENT_NUMBER) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE, " ", AMENDMENT_NUMBER))),
         START_YEAR1 = format(START_DATE2, "%Y"),
         COMMON_NAME_USE = str_to_title(paste0(COMMON_NAME_USE)),
         MANAGEMENT_TYPE_USE = str_to_title(paste0(MANAGEMENT_TYPE_USE)))


#Create table
cytsacl_tab1 <- cytsacl_tab %>%
  select(COMMON_NAME_USE, SECTOR1, ZONE1, START_YEAR1, START_DATE1, END_DATE1, VALUE1, FR_CITATION, ACTION1)%>%
  distinct()%>%
  arrange(START_DATE1, SECTOR1) %>%
  do(tab = flextable(.[1:9]) %>%
       set_header_labels(SECTOR1 = "Fishery",
                         ZONE1 = "Region Affected",
                         VALUE1 = "ACL (round weight)",
                         START_YEAR1 = "Start Year",
                         START_DATE1 = "Effective Date",
                         END_DATE1 = "End Date",
                         FR_CITATION = "FR Reference(s)",
                         ACTION1 = "Amendment Number or Rule Type",
                         COMMON_NAME_USE = "Species") %>%
       merge_v(j = 1, part = "body") %>%
       merge_v(j = 2, part = "body") %>%
       merge_v(j = 3, part = "body") %>%
       merge_v(j = 4, part = "body") %>%
       merge_v(j = 5, part = "body") %>%
       merge_v(j = 6, part = "body") %>%
       theme_box() %>%
       hline_top(part = "header", border = fp_border(color = "black", width = 2)) %>%
       hline_bottom(part = "header", border = fp_border(color = "black", width = 2)) %>%
       fontsize(part = "all", size = 12) %>%
       font(part = "all", fontname = "Times New Roman") %>%
       align(part = "all", align = "center"))


cytsacl_tab1$tab[[1]]


# Gulf red grouper tables
spp = 'PARROTFISH, STOPLIGHT'
region = 'CARIBBEAN'


cyslpbag_tab1 <- mh_data_log %>%
  filter(COMMON_NAME_USE == spp, REGION == region, MANAGEMENT_TYPE_USE == "BAG LIMIT") %>%
  filter(NEVER_IMPLEMENTED == 0) %>%
  arrange(FR_CITATION) %>%
  ungroup() %>%
  mutate(START_DATE1 = format(START_DATE2, "%m/%d/%Y"),
         END_DATE1 = paste0("Present"),
         SECTOR1 = str_to_title(paste0(SECTOR_USE, "\n", SUBSECTOR_USE)),
         ZONE1 = str_to_title(paste0(REGION, "\n", ZONE_USE)),
         VALUE1 = case_when(FLAG == 'YES' ~ paste0(VALUE, " ", tolower(VALUE_RATE), "*"),
                            FLAG == 'NO' ~ paste0(VALUE, " ", tolower(VALUE_RATE))),
         ACTION1 = case_when(is.na(AMENDMENT_NUMBER) & !is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE)),
                             is.na(AMENDMENT_NUMBER) & is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION)),
                             !is.na(AMENDMENT_NUMBER) & is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", AMENDMENT_NUMBER)),
                             !is.na(AMENDMENT_NUMBER) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE, " ", AMENDMENT_NUMBER))),
         START_YEAR1 = format(START_DATE2, "%Y"),
         COMMON_NAME_USE = str_to_title(paste0(COMMON_NAME_USE)),
         MANAGEMENT_TYPE_USE = str_to_title(paste0(MANAGEMENT_TYPE_USE)))

#Create table
cyslpbag_tab2 <- cyslpbag_tab1 %>%
  select(COMMON_NAME_USE, SECTOR1, ZONE1, MANAGEMENT_TYPE_USE, START_YEAR1, START_DATE1, END_DATE1, VALUE1, FR_CITATION, ACTION1)%>%
  do(tab = flextable(.[1:10]) %>%
       set_header_labels(SECTOR1 = "Fishery",
                         ZONE1 = "Region Affected",
                         START_YEAR1 = "Start Year",
                         START_DATE1 = "Effective Date",
                         END_DATE1 = "End Date",
                         VALUE1 = "Bag Limit",
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
       align(part = "all", align = "center"))


cyslpbag_tab2$tab[[1]]
print(cyslpbag_tab2$tab[[1]], preview = "docx")

#Table for Commercial Size Limits
#Filter data and combine column information
cyslpsize_tab2 <- mh_data_log %>%
  filter(COMMON_NAME_USE == spp, REGION == region, MANAGEMENT_CATEGORY == 'SELECTIVITY CONTROLS') %>%
  filter(NEVER_IMPLEMENTED == 0) %>%
  ungroup() %>%
  mutate(START_DATE4 = format(START_DATE2, "%m/%d/%Y"),
         END_DATE4 = if_else(END_DATE2 == as.Date("2021-12-31"), "Present", format(END_DATE2, "%m/%d/%Y")),
         SECTOR4 = str_to_title(paste0(SECTOR, "\n", SUBSECTOR_USE)),
         ZONE4 = str_to_title(paste0(REGION, "\n", ZONE_USE)),
         VALUE4 = case_when(FLAG == 'YES' ~ paste0(VALUE, " ", tolower(VALUE_UNITS), "*"),
                            FLAG == 'NO' ~ paste0(VALUE, " ", tolower(VALUE_UNITS))),
         ACTION4 = case_when(is.na(AMENDMENT_NUMBER) & !is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE)),
                             is.na(AMENDMENT_NUMBER) & is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION)),
                             !is.na(AMENDMENT_NUMBER) & is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", AMENDMENT_NUMBER)),
                             !is.na(AMENDMENT_NUMBER) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE, " ", AMENDMENT_NUMBER))),
         START_YEAR4 = format(START_DATE2, "%Y"),
         VALUE_TYPE4 = case_when(MANAGEMENT_TYPE_USE == 'MINIMUM SIZE LIMIT' ~ paste0("Minimum", " ", str_to_title(VALUE_TYPE)),
                                 MANAGEMENT_TYPE_USE == 'MAXIMUM SIZE LIMIT' ~ paste0("Maximum", " ", str_to_title(VALUE_TYPE))),
         COMMON_NAME_USE = str_to_title(paste0(COMMON_NAME_USE)),
         MANAGEMENT_TYPE_USE = str_to_title(paste0(MANAGEMENT_TYPE_USE)))


#Create table
cyslpsize_tab21 <- cyslpsize_tab2 %>%
  select(COMMON_NAME_USE, SECTOR4, ZONE4, START_YEAR4, START_DATE4, END_DATE4, VALUE4, VALUE_TYPE4, FR_CITATION, ACTION4)%>%
  arrange(START_DATE4) %>%
  distinct() %>%
  do(tab = flextable(.[1:10]) %>%
       set_header_labels(SECTOR4 = "Fishery",
                         ZONE4 = "Region Affected",
                         START_YEAR4 = "Start Year",
                         START_DATE4 = "Effective Date",
                         END_DATE4 = "End Date",
                         VALUE4 = "Size Limit",
                         VALUE_TYPE4 = "Length Type",
                         FR_CITATION = "FR Reference(s)",
                         ACTION4 = "Amendment Number or Rule Type",
                         COMMON_NAME_USE = "Species") %>%
       merge_v(j = 1, part = "body") %>%
       merge_v(j = 2, part = "body") %>%
       merge_v(j = 3, part = "body") %>%
       merge_v(j = 4, part = "body") %>%
       theme_box() %>%
       hline_top(part = "header", border = fp_border(color = "black", width = 2)) %>%
       hline_bottom(part = "header", border = fp_border(color = "black", width = 2)) %>%
       fontsize(part = "all", size = 12) %>%
       font(part = "all", fontname = "Times New Roman") %>%
       align(part = "all", align = "center"))


cyslpsize_tab21$tab[[1]]
print(cyslpsize_tab21$tab[[1]], preview = "docx")

#Prohibited gear regulations
cyslpgear_tab <- mh_data_log %>%
  filter(COMMON_NAME_USE == spp, REGION == region, MANAGEMENT_TYPE_USE == 'PROHIBITED GEAR', SECTOR_USE == 'COMMERCIAL') %>%
  filter(NEVER_IMPLEMENTED == 0) %>%
  ungroup() %>%
  mutate(START_DATE1 = format(START_DATE2, "%m/%d/%Y"),
         END_DATE1 = if_else(END_DATE2 == as.Date("2021-12-31"), "Present", format(END_DATE2, "%m/%d/%Y")),
         SECTOR1 = str_to_title(paste0(SECTOR_USE)),
         ZONE1 = str_to_title(paste0(REGION, "\n", ZONE_USE)),
         SUBSECTOR1 = case_when(FLAG == 'YES' ~ str_to_title(paste0(SUBSECTOR_USE, "*")),
                                FLAG == 'NO' ~ str_to_title(paste0(SUBSECTOR_USE))),
         ACTION1 = case_when(is.na(AMENDMENT_NUMBER) & !is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE)),
                             is.na(AMENDMENT_NUMBER) & is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION)),
                             !is.na(AMENDMENT_NUMBER) & is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", AMENDMENT_NUMBER)),
                             !is.na(AMENDMENT_NUMBER) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE, " ", AMENDMENT_NUMBER))),
         START_YEAR1 = format(START_DATE2, "%Y"),
         COMMON_NAME_USE = str_to_title(paste0(COMMON_NAME_USE)),
         MANAGEMENT_TYPE_USE = str_to_title(paste0(MANAGEMENT_TYPE_USE)))


#Create table
cyslpgear_tab1 <- cyslpgear_tab %>%
  select(COMMON_NAME_USE, ZONE1, SECTOR1, SUBSECTOR1, START_YEAR1, START_DATE1, END_DATE1, FR_CITATION, ACTION1)%>%
  arrange(SUBSECTOR1, START_DATE1) %>%
  do(tab = flextable(.[1:9]) %>%
       set_header_labels(SECTOR1 = "Fishery",
                         ZONE1 = "Region Affected",
                         SUBSECTOR1 = "Gear Type",
                         START_YEAR1 = "Start Year",
                         START_DATE1 = "Effective Date",
                         END_DATE1 = "End Date",
                         FR_CITATION = "FR Reference(s)",
                         ACTION1 = "Amendment Number or Rule Type",
                         COMMON_NAME_USE = "Species") %>%
       merge_v(j = 1, part = "body") %>%
       merge_v(j = 2, part = "body") %>%
       merge_v(j = 3, part = "body") %>%
       merge_v(j = 4, part = "body") %>%
       theme_box() %>%
       hline_top(part = "header", border = fp_border(color = "black", width = 2)) %>%
       hline_bottom(part = "header", border = fp_border(color = "black", width = 2)) %>%
       fontsize(part = "all", size = 12) %>%
       font(part = "all", fontname = "Times New Roman") %>%
       align(part = "all", align = "center"))


cyslpgear_tab1$tab[[1]]
print(cyslpgear_tab1$tab[[1]], preview = "docx")

#Prohibited gear regulations
cyslpgear_tab2 <- mh_data_log %>%
  filter(COMMON_NAME_USE == spp, REGION == region, MANAGEMENT_TYPE_USE == 'PROHIBITED GEAR', SECTOR_USE == 'RECREATIONAL') %>%
  filter(NEVER_IMPLEMENTED == 0) %>%
  ungroup() %>%
  mutate(START_DATE1 = format(START_DATE2, "%m/%d/%Y"),
         END_DATE1 = if_else(END_DATE2 == as.Date("2021-12-31"), "Present", format(END_DATE2, "%m/%d/%Y")),
         SECTOR1 = str_to_title(paste0(SECTOR_USE)),
         ZONE1 = str_to_title(paste0(REGION, "\n", ZONE_USE)),
         SUBSECTOR1 = case_when(FLAG == 'YES' ~ str_to_title(paste0(SUBSECTOR_USE, "*")),
                                FLAG == 'NO' ~ str_to_title(paste0(SUBSECTOR_USE))),
         ACTION1 = case_when(is.na(AMENDMENT_NUMBER) & !is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE)),
                             is.na(AMENDMENT_NUMBER) & is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION)),
                             !is.na(AMENDMENT_NUMBER) & is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", AMENDMENT_NUMBER)),
                             !is.na(AMENDMENT_NUMBER) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE, " ", AMENDMENT_NUMBER))),
         START_YEAR1 = format(START_DATE2, "%Y"),
         COMMON_NAME_USE = str_to_title(paste0(COMMON_NAME_USE)),
         MANAGEMENT_TYPE_USE = str_to_title(paste0(MANAGEMENT_TYPE_USE)))


#Create table
cyslpgear_tab22 <- cyslpgear_tab2 %>%
  select(COMMON_NAME_USE, ZONE1, SECTOR1, SUBSECTOR1, START_YEAR1, START_DATE1, END_DATE1, FR_CITATION, ACTION1)%>%
  arrange(SUBSECTOR1, START_DATE1) %>%
  do(tab = flextable(.[1:9]) %>%
       set_header_labels(SECTOR1 = "Fishery",
                         ZONE1 = "Region Affected",
                         SUBSECTOR1 = "Gear Type",
                         START_YEAR1 = "Start Year",
                         START_DATE1 = "Effective Date",
                         END_DATE1 = "End Date",
                         FR_CITATION = "FR Reference(s)",
                         ACTION1 = "Amendment Number or Rule Type",
                         COMMON_NAME_USE = "Species") %>%
       merge_v(j = 1, part = "body") %>%
       merge_v(j = 2, part = "body") %>%
       merge_v(j = 3, part = "body") %>%
       merge_v(j = 4, part = "body") %>%
       theme_box() %>%
       hline_top(part = "header", border = fp_border(color = "black", width = 2)) %>%
       hline_bottom(part = "header", border = fp_border(color = "black", width = 2)) %>%
       fontsize(part = "all", size = 12) %>%
       font(part = "all", fontname = "Times New Roman") %>%
       align(part = "all", align = "center"))


cyslpgear_tab22$tab[[1]]
print(cyslpgear_tab22$tab[[1]], preview = "docx")

#ACL regulations commercial
cyslpacl_tab <- mh_data_log %>%
  filter(COMMON_NAME_USE == spp, REGION == region, MANAGEMENT_TYPE_USE == 'ACL') %>%
  filter(NEVER_IMPLEMENTED == 0, ZONE_USE != "ST. THOMAS/ST. JOHN MANAGEMENT AREA", ZONE_USE != "PUERTO RICO MANAGEMENT AREA") %>%
  ungroup() %>%
  mutate(START_DATE1 = format(START_DATE2, "%m/%d/%Y"),
         END_DATE1 = if_else(END_DATE2 == as.Date("2021-12-31"), "Present", format(END_DATE2, "%m/%d/%Y")),
         SECTOR1 = str_to_title(paste0(SECTOR, "\n", SUBSECTOR)),
         ZONE1 = str_to_title(paste0(REGION, "\n", ZONE_USE)),
         VALUE1 = case_when(FLAG == 'YES' ~ paste0(VALUE, " ", tolower(VALUE_UNITS), "*"),
                            FLAG == 'NO' ~ paste0(VALUE, " ", tolower(VALUE_UNITS))),
         ACTION1 = case_when(is.na(AMENDMENT_NUMBER) & !is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE)),
                             is.na(AMENDMENT_NUMBER) & is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION)),
                             !is.na(AMENDMENT_NUMBER) & is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", AMENDMENT_NUMBER)),
                             !is.na(AMENDMENT_NUMBER) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE, " ", AMENDMENT_NUMBER))),
         START_YEAR1 = format(START_DATE2, "%Y"),
         COMMON_NAME_USE = str_to_title(paste0(COMMON_NAME_USE)),
         MANAGEMENT_TYPE_USE = str_to_title(paste0(MANAGEMENT_TYPE_USE)))


#Create table
cyslpacl_tab1 <- cyslpacl_tab %>%
  select(COMMON_NAME_USE, SECTOR1, ZONE1, START_YEAR1, START_DATE1, END_DATE1, VALUE1, FR_CITATION, ACTION1)%>%
  distinct()%>%
  arrange(START_DATE1, SECTOR1) %>%
  do(tab = flextable(.[1:9]) %>%
       set_header_labels(SECTOR1 = "Fishery",
                         ZONE1 = "Region Affected",
                         VALUE1 = "ACL (round weight)",
                         START_YEAR1 = "Start Year",
                         START_DATE1 = "Effective Date",
                         END_DATE1 = "End Date",
                         FR_CITATION = "FR Reference(s)",
                         ACTION1 = "Amendment Number or Rule Type",
                         COMMON_NAME_USE = "Species") %>%
       merge_v(j = 1, part = "body") %>%
       merge_v(j = 2, part = "body") %>%
       merge_v(j = 3, part = "body") %>%
       merge_v(j = 4, part = "body") %>%
       merge_v(j = 5, part = "body") %>%
       merge_v(j = 6, part = "body") %>%
       theme_box() %>%
       hline_top(part = "header", border = fp_border(color = "black", width = 2)) %>%
       hline_bottom(part = "header", border = fp_border(color = "black", width = 2)) %>%
       fontsize(part = "all", size = 12) %>%
       font(part = "all", fontname = "Times New Roman") %>%
       align(part = "all", align = "center"))


cyslpacl_tab1$tab[[1]]

print(cyslpacl_tab1$tab[[1]], preview = "docx")

#### Yellowtail snapper closures ####
# Select species and region ####
spp <- 'SNAPPER, YELLOWTAIL'
region <- 'CARIBBEAN'

# Seasonal Closures ####
# Filter dataset to seasonal closure regulations for the specified species and region
Seasonal_closures <- mh_data_log %>%
  filter(MANAGEMENT_TYPE_USE == 'CLOSURE', MANAGEMENT_STATUS_USE == 'SEASONAL', NEVER_IMPLEMENTED == 0, REG_REMOVED == 0, COMMON_NAME_USE == spp, REGION == region, !(ZONE_USE == "MUTTON SNAPPER SPAWNING AGGREGATION REEF FISH FISHERY MANAGEMENT AREA"), !(ZONE_USE == "RED HIND SPAWNING AGGREGATION EAST OF ST. CROIX REEF FISH FISHERY MANAGEMENT AREA"))

#Change start and end dates
Seasonal_closures <- Seasonal_closures %>%
  mutate(END_DATE2 = case_when(ZONE_USE == "RED HIND SPAWNING AGGREGATION WEST OF PUERTO RICO REEF FISH MANAGEMENT AREA" ~ as.Date("1996-12-06"),
                                       TRUE ~ as.Date(END_DATE2)))

Seasonal_closures <- Seasonal_closures %>%
  mutate(END_DATE2 = case_when(ZONE_USE == "RED HIND SPAWNING AGGREGATION WEST OF PUERTO RICO - BAJO DE SICO" ~ as.Date("2010-12-01"),
                               TRUE ~ as.Date(END_DATE2)))
  
# Create date_sequence to expand dates between EFFECTIVE_DATE and END_DATE2 
expand_seasonal <- Seasonal_closures %>%
  mutate(date_sequence = map2(EFFECTIVE_DATE, END_DATE2, seq, by = "days")) %>%
  unnest(date_sequence)

# Add START_YEAR_expand and END_YEAR_expand to extract the year from the date_sequence field
expand_seasonal_year <- expand_seasonal %>%
  rowwise() %>%
  mutate(START_YEAR_expand = year(date_sequence),
         END_YEAR_expand = year(date_sequence))

# Create START_DATE_EXPAND and END_DATE_EXPAND using start/end day and month information and the expanded year
expand_seasonal_date <- expand_seasonal_year %>%
  mutate(START_DATE_EXPAND = as.Date(paste(START_YEAR_expand, START_MONTH, START_DAY, sep = "-")),
         END_DATE_EXPAND = as.Date(paste(END_YEAR_expand, END_MONTH, END_DAY, sep = "-")))

# Create START_DATE_EXPAND2 for cases where the duration of the closure goes from the end of the year to the beginning of the next year
expand_seasonal_date <- expand_seasonal_date %>%
  mutate(START_DATE_EXPAND2 = case_when(START_DATE_EXPAND > END_DATE_EXPAND ~ START_DATE_EXPAND - lubridate::years(1),
                                        TRUE ~ START_DATE_EXPAND))

# Create END_DATE_EXPAND2 for cases where the duration of the closure goes from the end of the year to the beginning of the next year
expand_seasonal_date <- expand_seasonal_date %>%
  mutate(END_DATE_EXPAND2 = case_when(START_DATE_EXPAND > END_DATE_EXPAND ~ END_DATE_EXPAND + lubridate::years(1), 
                                      TRUE ~ END_DATE_EXPAND))

# Create START_DATE_EXPAND_FINAL to choose the correct START_DATE_EXPAND for each set of circumstances
expand_seasonal_date <- expand_seasonal_date %>%
  mutate(START_DATE_EXPAND_FINAL = case_when(START_DATE_EXPAND > END_DATE_EXPAND & date_sequence <= START_DATE_EXPAND ~ START_DATE_EXPAND2,
                                             TRUE ~ START_DATE_EXPAND))

# Create END_DATE_EXPAND_FINAL to choose correct END_DATE_EXPAND for each set of circumstances
expand_seasonal_date <- expand_seasonal_date %>%
  mutate(END_DATE_EXPAND_FINAL = case_when(START_DATE_EXPAND > END_DATE_EXPAND & date_sequence >= START_DATE_EXPAND & date_sequence >= END_DATE_EXPAND ~ END_DATE_EXPAND2,
                                           TRUE ~ END_DATE_EXPAND))

# Remove records where date_sequence is prior to the START_DATE_EXPAND and date_sequence is after the END_DATE_EXPAND
remove_seasonal_date <- expand_seasonal_date %>%
  filter(date_sequence >= START_DATE_EXPAND_FINAL,
         END_DATE_EXPAND_FINAL >= date_sequence)

add_closure_value_seasonal <- remove_seasonal_date %>%
  mutate(CLOSE_OPEN = VALUE)

# One Time Closures ####
# Filter dataset to One Time regulations
One_closures <- mh_data_log %>%
  filter(MANAGEMENT_TYPE_USE == 'CLOSURE', MANAGEMENT_STATUS_USE == 'ONCE', NEVER_IMPLEMENTED == 0, REG_REMOVED == 0, COMMON_NAME_USE == spp, REGION == region, !(ZONE_USE == "MUTTON SNAPPER SPAWNING AGGREGATION REEF FISH FISHERY MANAGEMENT AREA"), !(ZONE_USE == "RED HIND SPAWNING AGGREGATION EAST OF ST. CROIX REEF FISH FISHERY MANAGEMENT AREA"))

#Change start and end dates
One_closures <- One_closures %>%
  mutate(END_DATE2 = case_when(ZONE_USE == "RED HIND SPAWNING AGGREGATION WEST OF PUERTO RICO REEF FISH MANAGEMENT AREA" ~ as.Date("1996-12-06"),
                               TRUE ~ as.Date(END_DATE2)))

One_closures <- One_closures %>%
  mutate(END_DATE2 = case_when(ZONE_USE == "RED HIND SPAWNING AGGREGATION WEST OF PUERTO RICO - BAJO DE SICO" ~ as.Date("2010-12-01"),
                               TRUE ~ as.Date(END_DATE2)))

# Expand dates between EFFECTIVE_DATE and END_DATE2
expand_one <- One_closures %>%
  filter(END_DATE2 >= START_DATE2) %>%
  mutate(date_sequence = map2(START_DATE2, END_DATE2, seq, by = "1 day")) %>%
  unnest(date_sequence)

# Create CLOSE_OPEN field to indicate that expanded records are related to Closures or Openings
add_closure_value_one <- expand_one %>%
  mutate(CLOSE_OPEN = VALUE)

# Translate EFFECTIVE_DATE and END_DATE2 to START_DATE_EXPAND_FINAL and END_DATE_EXPAND_FINAL
add_closure_value_one <- add_closure_value_one %>%
  mutate(START_DATE_EXPAND_FINAL = EFFECTIVE_DATE,
         END_DATE_EXPAND_FINAL = END_DATE2)

# Join Closure Types Together ####
# Combine closure data frames
Combined_closures <- bind_rows(add_closure_value_seasonal, add_closure_value_one)

# Sort the data frame by date_sequence
sort_closures <- Combined_closures %>%
  arrange(date_sequence)

# Create Multi_expand to indicate whether records have the same date_sequence 
sort_closures_multi <- sort_closures %>%
  group_by(ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence) %>%
  mutate(Multi_expand = ifelse(n() > 1, 1, 0)) %>%
  ungroup()

# Keep information from newest FR 
closures_remove_multi <- sort_closures_multi %>%
  arrange(date_sequence, desc(FR_CITATION)) %>%
  group_by(ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence) %>%
  slice(1) %>%
  ungroup()

# Remove extra processing fields from dataset
closures_remove_fields <- sort_closures_multi %>%
  select(-START_YEAR_expand, -END_YEAR_expand, -START_DATE_EXPAND, -END_DATE_EXPAND, -Multi_expand)

#Start year expand final should match date sequence
start_year <- closures_remove_fields %>%
  mutate(START_DATE_EXPAND_FINAL = update(START_DATE_EXPAND_FINAL, year = year(date_sequence)))

# Collapse rows
collapse <- start_year %>%
  arrange(CLUSTER, REGULATION_ID, FR_CITATION, FR_SECTION, FR_URL, FMP, ACTION, ACTION_TYPE, 
          AMENDMENT_NUMBER, ACCOUNTABILITY_MEASURE, MANAGEMENT_CATEGORY, MANAGEMENT_TYPE, 
          MANAGEMENT_TYPE_USE, MANAGEMENT_STATUS, MANAGEMENT_STATUS_USE, STATUS_TYPE, DETAILED,
          JURISDICTION, SECTOR, SECTOR_USE, SUBSECTOR, SUBSECTOR_USE, REGION, ZONE, ZONE_USE, 
          JURISDICTIONAL_WATERS, COMMON_NAME_USE, SPP_TYPE, SPP_NAME, SPECIES_ITIS_USE, ADJUSTMENT,
          REVERSION, EFFECTIVE_DATE, INEFFECTIVE_DATE, START_DATE2, END_DATE2, diff_days, START_MONTH,
          START_DAY, START_DAY_USE, START_YEAR, START_TIME, START_TIME_USE, START_DAY_OF_WEEK, 
          START_DAY_OF_WEEK_USE, END_MONTH, END_MONTH_USE, END_DAY, END_DAY_USE, END_YEAR, END_YEAR_USE,
          END_TIME, END_TIME_USE, END_DAY_OF_WEEK, END_DAY_OF_WEEK_USE, VALUE, VALUE_UNITS, VALUE_TYPE, VALUE_RATE,
          MULTI_REG_VALUE, MULTI_REG_FORECAST, MULTI_REG_CLOSURE, MULTI_REG_CLUSTER, REG_REMOVED,
          NEVER_IMPLEMENTED, FLAG, START_DATE_EXPAND_FINAL, END_DATE_EXPAND_FINAL, CLOSE_OPEN) %>%
  group_by(CLUSTER, REGULATION_ID, FR_CITATION, FR_SECTION, FR_URL, FMP, ACTION, ACTION_TYPE, 
           AMENDMENT_NUMBER, ACCOUNTABILITY_MEASURE, MANAGEMENT_CATEGORY, MANAGEMENT_TYPE, 
           MANAGEMENT_TYPE_USE, MANAGEMENT_STATUS, MANAGEMENT_STATUS_USE, STATUS_TYPE, DETAILED,
           JURISDICTION, SECTOR, SECTOR_USE, SUBSECTOR, SUBSECTOR_USE, REGION, ZONE, ZONE_USE, 
           JURISDICTIONAL_WATERS, COMMON_NAME_USE, SPP_TYPE, SPP_NAME, SPECIES_ITIS_USE, ADJUSTMENT,
           REVERSION, EFFECTIVE_DATE, INEFFECTIVE_DATE, START_DATE2, END_DATE2, diff_days, START_MONTH,
           START_DAY, START_DAY_USE, START_YEAR, START_TIME, START_TIME_USE, START_DAY_OF_WEEK, 
           START_DAY_OF_WEEK_USE, END_MONTH, END_MONTH_USE, END_DAY, END_DAY_USE, END_YEAR, END_YEAR_USE,
           END_TIME, END_TIME_USE, END_DAY_OF_WEEK, END_DAY_OF_WEEK_USE, VALUE, VALUE_UNITS, VALUE_TYPE, VALUE_RATE,
           MULTI_REG_VALUE, MULTI_REG_FORECAST, MULTI_REG_CLOSURE, MULTI_REG_CLUSTER, REG_REMOVED,
           NEVER_IMPLEMENTED, FLAG, START_DATE_EXPAND_FINAL, END_DATE_EXPAND_FINAL, CLOSE_OPEN) %>%
  summarise(date_range_start = min(date_sequence),
            date_range_end = max(date_sequence))
  
# Create EFFECTIVE_YEAR field to indicate the effective year of the closure
closures_effective_year <- collapse %>%
  mutate(EFFECTIVE_YEAR = year(date_range_start)) %>%
  ungroup()

cytsclosure_tab <- closures_effective_year %>%
  mutate(START_DATE1 = format(date_range_start, "%m/%d/%Y"),
         END_DATE1 = format(date_range_end, "%m/%d/%Y"),
         SECTOR1 = str_to_title(paste0(SECTOR, "\n", SUBSECTOR)),
         ZONE1 = str_to_title(paste0(REGION, "\n", ZONE_USE)),
         VALUE1 = case_when(FLAG == 'YES' ~ str_to_title(paste0(VALUE, "*")),
                            FLAG == 'NO' ~ str_to_title(paste0(VALUE))),
         ACTION1 = case_when(is.na(AMENDMENT_NUMBER) & !is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE)),
                             is.na(AMENDMENT_NUMBER) & is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION)),
                             !is.na(AMENDMENT_NUMBER) & is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", AMENDMENT_NUMBER)),
                             !is.na(AMENDMENT_NUMBER) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE, " ", AMENDMENT_NUMBER))),
         START_YEAR1 = format(date_range_start, "%Y"),
         COMMON_NAME_USE = str_to_title(paste0(COMMON_NAME_USE)),
         MANAGEMENT_TYPE_USE = str_to_title(paste0(MANAGEMENT_TYPE_USE)))


#Create table
cytsclosure_tab1 <- cytsclosure_tab %>%
  select(COMMON_NAME_USE, ZONE1, SECTOR1, START_YEAR1, START_DATE1, END_DATE1, VALUE1, FR_CITATION, ACTION1)%>%
  distinct()%>%
  arrange(ZONE1, SECTOR1, START_YEAR1, START_DATE1) %>%
  do(tab = flextable(.[1:9]) %>%
       set_header_labels(SECTOR1 = "Fishery",
                         ZONE1 = "Region Affected",
                         VALUE1 = "Closure",
                         START_YEAR1 = "Start Year",
                         START_DATE1 = "Effective Date",
                         END_DATE1 = "End Date",
                         FR_CITATION = "FR Reference(s)",
                         ACTION1 = "Amendment Number or Rule Type",
                         COMMON_NAME_USE = "Species") %>%
       merge_v(j = 1, part = "body") %>%
       merge_v(j = 2, part = "body") %>%
       merge_v(j = 3, part = "body") %>%
       merge_v(j = 4, part = "body") %>%
       merge_v(j = 5, part = "body") %>%
       merge_v(j = 6, part = "body") %>%
       theme_box() %>%
       hline_top(part = "header", border = fp_border(color = "black", width = 2)) %>%
       hline_bottom(part = "header", border = fp_border(color = "black", width = 2)) %>%
       fontsize(part = "all", size = 12) %>%
       font(part = "all", fontname = "Times New Roman") %>%
       align(part = "all", align = "center"))


cytsclosure_tab1$tab[[1]]

print(cytsclosure_tab1$tab[[1]], preview = "docx")

spp = 'PARROTFISH, STOPLIGHT'
region = 'CARIBBEAN'

# Seasonal Closures ####
# Filter dataset to seasonal closure regulations for the specified species and region
Seasonal_closures <- mh_data_log %>%
  filter(MANAGEMENT_TYPE_USE == 'CLOSURE', MANAGEMENT_STATUS_USE == 'SEASONAL', NEVER_IMPLEMENTED == 0, REG_REMOVED == 0, COMMON_NAME_USE == spp, REGION == region, ZONE_USE %in% c("MUTTON SNAPPER SPAWNING AGGREGATION REEF FISH FISHERY MANAGEMENT AREA", "RED HIND SPAWNING AGGREGATION EAST OF ST. CROIX REEF FISH FISHERY MANAGEMENT AREA"))

# Create date_sequence to expand dates between EFFECTIVE_DATE and END_DATE2 
expand_seasonal <- Seasonal_closures %>%
  mutate(date_sequence = map2(EFFECTIVE_DATE, END_DATE2, seq, by = "days")) %>%
  unnest(date_sequence)

# Add START_YEAR_expand and END_YEAR_expand to extract the year from the date_sequence field
expand_seasonal_year <- expand_seasonal %>%
  rowwise() %>%
  mutate(START_YEAR_expand = year(date_sequence),
         END_YEAR_expand = year(date_sequence))

# Create START_DATE_EXPAND and END_DATE_EXPAND using start/end day and month information and the expanded year
expand_seasonal_date <- expand_seasonal_year %>%
  mutate(START_DATE_EXPAND = as.Date(paste(START_YEAR_expand, START_MONTH, START_DAY, sep = "-")),
         END_DATE_EXPAND = as.Date(paste(END_YEAR_expand, END_MONTH, END_DAY, sep = "-")))

# Create START_DATE_EXPAND2 for cases where the duration of the closure goes from the end of the year to the beginning of the next year
expand_seasonal_date <- expand_seasonal_date %>%
  mutate(START_DATE_EXPAND2 = case_when(START_DATE_EXPAND > END_DATE_EXPAND ~ START_DATE_EXPAND - lubridate::years(1),
                                        TRUE ~ START_DATE_EXPAND))

# Create END_DATE_EXPAND2 for cases where the duration of the closure goes from the end of the year to the beginning of the next year
expand_seasonal_date <- expand_seasonal_date %>%
  mutate(END_DATE_EXPAND2 = case_when(START_DATE_EXPAND > END_DATE_EXPAND ~ END_DATE_EXPAND + lubridate::years(1), 
                                      TRUE ~ END_DATE_EXPAND))

# Create START_DATE_EXPAND_FINAL to choose the correct START_DATE_EXPAND for each set of circumstances
expand_seasonal_date <- expand_seasonal_date %>%
  mutate(START_DATE_EXPAND_FINAL = case_when(START_DATE_EXPAND > END_DATE_EXPAND & date_sequence <= START_DATE_EXPAND ~ START_DATE_EXPAND2,
                                             TRUE ~ START_DATE_EXPAND))

# Create END_DATE_EXPAND_FINAL to choose correct END_DATE_EXPAND for each set of circumstances
expand_seasonal_date <- expand_seasonal_date %>%
  mutate(END_DATE_EXPAND_FINAL = case_when(START_DATE_EXPAND > END_DATE_EXPAND & date_sequence >= START_DATE_EXPAND & date_sequence >= END_DATE_EXPAND ~ END_DATE_EXPAND2,
                                           TRUE ~ END_DATE_EXPAND))

# Remove records where date_sequence is prior to the START_DATE_EXPAND and date_sequence is after the END_DATE_EXPAND
remove_seasonal_date <- expand_seasonal_date %>%
  filter(date_sequence >= START_DATE_EXPAND_FINAL,
         END_DATE_EXPAND_FINAL >= date_sequence)

add_closure_value_seasonal <- remove_seasonal_date %>%
  mutate(CLOSE_OPEN = VALUE)

# One Time Closures ####
# Filter dataset to One Time regulations
One_closures <- mh_data_log %>%
  filter(MANAGEMENT_TYPE_USE == 'CLOSURE', MANAGEMENT_STATUS_USE == 'ONCE', NEVER_IMPLEMENTED == 0, REG_REMOVED == 0, COMMON_NAME_USE == spp, REGION == region, ZONE_USE %in% c("MUTTON SNAPPER SPAWNING AGGREGATION REEF FISH FISHERY MANAGEMENT AREA", "RED HIND SPAWNING AGGREGATION EAST OF ST. CROIX REEF FISH FISHERY MANAGEMENT AREA"))

# Expand dates between EFFECTIVE_DATE and END_DATE2
expand_one <- One_closures %>%
  filter(END_DATE2 >= START_DATE2) %>%
  mutate(date_sequence = map2(START_DATE2, END_DATE2, seq, by = "1 day")) %>%
  unnest(date_sequence)

# Create CLOSE_OPEN field to indicate that expanded records are related to Closures or Openings
add_closure_value_one <- expand_one %>%
  mutate(CLOSE_OPEN = VALUE)

# Translate EFFECTIVE_DATE and END_DATE2 to START_DATE_EXPAND_FINAL and END_DATE_EXPAND_FINAL
add_closure_value_one <- add_closure_value_one %>%
  mutate(START_DATE_EXPAND_FINAL = EFFECTIVE_DATE,
         END_DATE_EXPAND_FINAL = END_DATE2)

# Join Closure Types Together ####
# Combine closure data frames
Combined_closures <- bind_rows(add_closure_value_seasonal, add_closure_value_one)

# Sort the data frame by date_sequence
sort_closures <- Combined_closures %>%
  arrange(date_sequence)

# Create Multi_expand to indicate whether records have the same date_sequence 
sort_closures_multi <- sort_closures %>%
  group_by(ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence) %>%
  mutate(Multi_expand = ifelse(n() > 1, 1, 0)) %>%
  ungroup()

# Keep information from newest FR 
closures_remove_multi <- sort_closures_multi %>%
  arrange(date_sequence, desc(FR_CITATION)) %>%
  group_by(ZONE_USE, SECTOR_USE, SUBSECTOR_USE, date_sequence) %>%
  slice(1) %>%
  ungroup()

# Remove extra processing fields from dataset
closures_remove_fields <- sort_closures_multi %>%
  select(-START_YEAR_expand, -END_YEAR_expand, -START_DATE_EXPAND, -END_DATE_EXPAND, -Multi_expand)

#Start year expand final should match date sequence
start_year <- closures_remove_fields %>%
  mutate(START_DATE_EXPAND_FINAL = update(START_DATE_EXPAND_FINAL, year = year(date_sequence)))

# Collapse rows
collapse <- start_year %>%
  arrange(CLUSTER, REGULATION_ID, FR_CITATION, FR_SECTION, FR_URL, FMP, ACTION, ACTION_TYPE, 
          AMENDMENT_NUMBER, ACCOUNTABILITY_MEASURE, MANAGEMENT_CATEGORY, MANAGEMENT_TYPE, 
          MANAGEMENT_TYPE_USE, MANAGEMENT_STATUS, MANAGEMENT_STATUS_USE, STATUS_TYPE, DETAILED,
          JURISDICTION, SECTOR, SECTOR_USE, SUBSECTOR, SUBSECTOR_USE, REGION, ZONE, ZONE_USE, 
          JURISDICTIONAL_WATERS, COMMON_NAME_USE, SPP_TYPE, SPP_NAME, SPECIES_ITIS_USE, ADJUSTMENT,
          REVERSION, EFFECTIVE_DATE, INEFFECTIVE_DATE, START_DATE2, END_DATE2, diff_days, START_MONTH,
          START_DAY, START_DAY_USE, START_YEAR, START_TIME, START_TIME_USE, START_DAY_OF_WEEK, 
          START_DAY_OF_WEEK_USE, END_MONTH, END_MONTH_USE, END_DAY, END_DAY_USE, END_YEAR, END_YEAR_USE,
          END_TIME, END_TIME_USE, END_DAY_OF_WEEK, END_DAY_OF_WEEK_USE, VALUE, VALUE_UNITS, VALUE_TYPE, VALUE_RATE,
          MULTI_REG_VALUE, MULTI_REG_FORECAST, MULTI_REG_CLOSURE, MULTI_REG_CLUSTER, REG_REMOVED,
          NEVER_IMPLEMENTED, FLAG, START_DATE_EXPAND_FINAL, END_DATE_EXPAND_FINAL, CLOSE_OPEN) %>%
  group_by(CLUSTER, REGULATION_ID, FR_CITATION, FR_SECTION, FR_URL, FMP, ACTION, ACTION_TYPE, 
           AMENDMENT_NUMBER, ACCOUNTABILITY_MEASURE, MANAGEMENT_CATEGORY, MANAGEMENT_TYPE, 
           MANAGEMENT_TYPE_USE, MANAGEMENT_STATUS, MANAGEMENT_STATUS_USE, STATUS_TYPE, DETAILED,
           JURISDICTION, SECTOR, SECTOR_USE, SUBSECTOR, SUBSECTOR_USE, REGION, ZONE, ZONE_USE, 
           JURISDICTIONAL_WATERS, COMMON_NAME_USE, SPP_TYPE, SPP_NAME, SPECIES_ITIS_USE, ADJUSTMENT,
           REVERSION, EFFECTIVE_DATE, INEFFECTIVE_DATE, START_DATE2, END_DATE2, diff_days, START_MONTH,
           START_DAY, START_DAY_USE, START_YEAR, START_TIME, START_TIME_USE, START_DAY_OF_WEEK, 
           START_DAY_OF_WEEK_USE, END_MONTH, END_MONTH_USE, END_DAY, END_DAY_USE, END_YEAR, END_YEAR_USE,
           END_TIME, END_TIME_USE, END_DAY_OF_WEEK, END_DAY_OF_WEEK_USE, VALUE, VALUE_UNITS, VALUE_TYPE, VALUE_RATE,
           MULTI_REG_VALUE, MULTI_REG_FORECAST, MULTI_REG_CLOSURE, MULTI_REG_CLUSTER, REG_REMOVED,
           NEVER_IMPLEMENTED, FLAG, START_DATE_EXPAND_FINAL, END_DATE_EXPAND_FINAL, CLOSE_OPEN) %>%
  summarise(date_range_start = min(date_sequence),
            date_range_end = max(date_sequence))

# Create EFFECTIVE_YEAR field to indicate the effective year of the closure
closures_effective_year <- collapse %>%
  mutate(EFFECTIVE_YEAR = year(date_range_start)) %>%
  ungroup()

cytsclosure_tab <- closures_effective_year %>%
  mutate(START_DATE1 = format(date_range_start, "%m/%d/%Y"),
         END_DATE1 = format(date_range_end, "%m/%d/%Y"),
         SECTOR1 = str_to_title(paste0(SECTOR, "\n", SUBSECTOR)),
         ZONE1 = str_to_title(paste0(REGION, "\n", ZONE_USE)),
         VALUE1 = case_when(FLAG == 'YES' ~ str_to_title(paste0(VALUE, "*")),
                            FLAG == 'NO' ~ str_to_title(paste0(VALUE))),
         ACTION1 = case_when(is.na(AMENDMENT_NUMBER) & !is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE)),
                             is.na(AMENDMENT_NUMBER) & is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION)),
                             !is.na(AMENDMENT_NUMBER) & is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", AMENDMENT_NUMBER)),
                             !is.na(AMENDMENT_NUMBER) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE, " ", AMENDMENT_NUMBER))),
         START_YEAR1 = format(date_range_start, "%Y"),
         COMMON_NAME_USE = str_to_title(paste0(COMMON_NAME_USE)),
         MANAGEMENT_TYPE_USE = str_to_title(paste0(MANAGEMENT_TYPE_USE)))


#Create table
cytsclosure_tab1 <- cytsclosure_tab %>%
  select(COMMON_NAME_USE, ZONE1, SECTOR1, START_YEAR1, START_DATE1, END_DATE1, VALUE1, FR_CITATION, ACTION1)%>%
  distinct()%>%
  arrange(ZONE1, SECTOR1, START_YEAR1, START_DATE1) %>%
  do(tab = flextable(.[1:9]) %>%
       set_header_labels(SECTOR1 = "Fishery",
                         ZONE1 = "Region Affected",
                         VALUE1 = "Closure",
                         START_YEAR1 = "Start Year",
                         START_DATE1 = "Effective Date",
                         END_DATE1 = "End Date",
                         FR_CITATION = "FR Reference(s)",
                         ACTION1 = "Amendment Number or Rule Type",
                         COMMON_NAME_USE = "Species") %>%
       merge_v(j = 1, part = "body") %>%
       merge_v(j = 2, part = "body") %>%
       merge_v(j = 3, part = "body") %>%
       merge_v(j = 4, part = "body") %>%
       merge_v(j = 5, part = "body") %>%
       merge_v(j = 6, part = "body") %>%
       theme_box() %>%
       hline_top(part = "header", border = fp_border(color = "black", width = 2)) %>%
       hline_bottom(part = "header", border = fp_border(color = "black", width = 2)) %>%
       fontsize(part = "all", size = 12) %>%
       font(part = "all", fontname = "Times New Roman") %>%
       align(part = "all", align = "center"))


cytsclosure_tab1$tab[[1]]
