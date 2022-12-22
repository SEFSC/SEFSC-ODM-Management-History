# FORMAT TABLES TO MATCH SEDAR WORKING PAPERS FOR EASIER VISUAL CHECKING

# Load packages ####
#install.packages("librarian")
librarian::shelf(here, tidyverse, gt, flextable, officer)

# RUN MH CODE
here::i_am('test/MH_test_print.R')
source(here('code', 'main_MH_prep.R'))

# MULTI REG ONLY WORKS WHEN MONTH DAY AND YEAR START PROVIDED?

# ISSUE WITH DATE SHIFT - MANUAL REG LIST HAS 8/29 and 8/30
# CODE HAS 8/28 and 8/29
# REG ID 792

# Input parameters 
spp = 'TRIGGERFISH, GRAY'
fmp = 'SNAPPER-GROUPER FISHERY OF THE SOUTH ATLANTIC REGION'

# Table for Size limits
tab_size <- mh_expanded %>%
  filter(COMMON_NAME_USE == spp, FMP == fmp, MANAGEMENT_CATEGORY == 'SELECTIVITY CONTROLS') %>%
  filter(NEVER_IMPLEMENTED == 0) %>%
  ungroup() %>%
  mutate(START_YEAR = format(START_DATE2, "%Y"),
         START_DATE3 = format(START_DATE2, "%m/%d/%Y"),
         END_DATE3 = format(END_DATE2, "%m/%d/%Y"),
         SECTOR2 = str_to_title(paste0(SECTOR_USE, "\n", SUBSECTOR_USE)),
         ZONE2 = str_to_title(paste0(REGION, "\n", ZONE_USE)),
         VALUE2 = case_when(FLAG == 'YES' ~ paste0(VALUE, " ", tolower(VALUE_UNITS), "*"),
                            FLAG == 'NO' ~ paste0(VALUE, " ", tolower(VALUE_UNITS))),
         VALUE_TYPE2 = case_when(MANAGEMENT_TYPE_USE == 'MINIMUM SIZE LIMIT' ~ paste0("Minimum ", str_to_title(VALUE_TYPE)),
                                 MANAGEMENT_TYPE_USE == 'MAXIMUM SIZE LIMIT' ~ paste0("Maximum ", str_to_title(VALUE_TYPE))),
         ACTION2 = case_when(is.na(AMENDMENT_NUMBER) & !is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE)),
                             is.na(AMENDMENT_NUMBER) & is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION)),
                             TRUE ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE, " ", AMENDMENT_NUMBER)))) %>%
  arrange(SECTOR_USE, ZONE2, START_DATE2) 
tab_size2 <- tab_size %>%
  select(CLUSTER, COMMON_NAME_USE, REGION, SECTOR_USE, MANAGEMENT_TYPE_USE, SECTOR2, ZONE2, START_YEAR, START_DATE3, END_DATE3, VALUE2, VALUE_TYPE2, FR_CITATION, ACTION2) %>%
  group_by(CLUSTER, COMMON_NAME_USE, REGION, SECTOR_USE, MANAGEMENT_TYPE_USE) %>%
  do(tab = flextable(.[6:14]) %>%
       set_header_labels(SECTOR2 = "Fishery",
                         ZONE2 = "Region Affected",
                         START_YEAR = "Start Year",
                         START_DATE3 = "Effective Date",
                         END_DATE3 = "End Date",
                         VALUE2 = "Size Limit",
                         VALUE_TYPE2 = "Length Type",
                         FR_CITATION = "FR Reference(s)",
                         ACTION2 = "Amendment Number or Rule Type") %>%
       merge_v(j = 1, part = "body") %>%
       merge_v(j = 2, part = "body") %>%
       merge_v(j = 6, part = "body") %>%
       merge_v(j = 7, part = "body") %>%
       theme_box() %>%
       hline_top(part = "header", border = fp_border(color = "black", width = 2)) %>%
       hline_bottom(part = "header", border = fp_border(color = "black", width = 2)) %>%
       fontsize(part = "all", size = 12) %>%
       font(part = "all", fontname = "Times New Roman") %>%
       align(part = "all", align = "center") %>%
       width(j=c(2,9), width=1.8) %>%
       width(j=c(3), width=0.45) %>%
       width(j=c(1,4:8), width=0.9) %>%
       set_caption(paste0(tab_size$COMMON_NAME_USE)))
for(i in 1:nrow(tab_size2)) {
  print(tab_size2$tab[[i]])
}

# Table for Trip Limit
tab_trip <- mh_expanded %>%
  filter(COMMON_NAME_USE == spp, FMP == fmp, MANAGEMENT_TYPE_USE == 'TRIP LIMIT') %>%
  filter(NEVER_IMPLEMENTED == 0) %>%
  ungroup() %>%
  mutate(START_YEAR = format(START_DATE2, "%Y"),
         START_DATE3 = format(START_DATE2, "%m/%d/%Y"),
         END_DATE3 = format(END_DATE2, "%m/%d/%Y"),
         SECTOR2 = str_to_title(paste0(SECTOR_USE, "\n", SUBSECTOR_USE)),
         ZONE2 = str_to_title(paste0(REGION, "\n", ZONE_USE)),
         VALUE2 = case_when(VALUE_UNITS == "POUNDS" & FLAG == 'YES' ~ paste0(VALUE, " lbs ", str_to_title(VALUE_TYPE), "*"),
                            VALUE_UNITS == "POUNDS" & FLAG == 'NO' ~ paste0(VALUE, " lbs ", str_to_title(VALUE_TYPE)),
                            VALUE_UNITS == "NUMBERS" & FLAG == 'YES' ~ paste0(VALUE, " fish*"),
                            VALUE_UNITS == "NUMBERS" & FLAG == 'NO' ~ paste0(VALUE, " fish"),
                            TRUE ~ paste0(VALUE, " ", tolower(VALUE_UNITS))),
         VALUE_RATE2 = str_to_title(VALUE_RATE),
         ACTION2 = case_when(is.na(AMENDMENT_NUMBER) & !is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE)),
                             is.na(AMENDMENT_NUMBER) & is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION)),
                             TRUE ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE, " ", AMENDMENT_NUMBER)))) %>%
  arrange(SECTOR_USE, ZONE2, START_DATE2)
tab_trip2 <- tab_trip %>%
  select(CLUSTER, COMMON_NAME_USE, REGION, SECTOR_USE, MANAGEMENT_TYPE_USE, SECTOR2, ZONE2, START_YEAR, START_DATE3, END_DATE3, VALUE2, VALUE_RATE2, FR_CITATION, ACTION2) %>%
  group_by(CLUSTER, COMMON_NAME_USE, REGION, SECTOR_USE, MANAGEMENT_TYPE_USE) %>%
  do(tab = flextable(.[6:14]) %>%
       set_header_labels(SECTOR2 = "Fishery",
                         ZONE2 = "Region Affected",
                         START_YEAR = "Start Year",
                         START_DATE3 = "Effective Date",
                         END_DATE3 = "End Date",
                         VALUE2 = "Trip Limit",
                         VALUE_RATE2 = "Rate",
                         FR_CITATION = "FR Reference(s)",
                         ACTION2 = "Amendment Number or Rule Type") %>%
       merge_v(j = 1, part = "body") %>%
       merge_v(j = 2, part = "body") %>%
       merge_v(j = 6, part = "body") %>%
       merge_v(j = 7, part = "body") %>%
       theme_box() %>%
       hline_top(part = "header", border = fp_border(color = "black", width = 2)) %>%
       hline_bottom(part = "header", border = fp_border(color = "black", width = 2)) %>%
       fontsize(part = "all", size = 12) %>%
       font(part = "all", fontname = "Times New Roman") %>%
       align(part = "all", align = "center") %>%
       width(j=c(2,9), width=1.8) %>%
       width(j=c(3), width=0.45) %>%
       width(j=c(1,4:8), width=0.9) %>%
       set_caption(paste(tab_size$COMMON_NAME_USE)))
for(i in 1:nrow(tab_trip2)) {
  print(tab_trip2$tab[[i]])
}
  
# Table for Bag Limit
tab_bag <- mh_expanded %>%
  filter(COMMON_NAME_USE == spp, FMP == fmp, MANAGEMENT_TYPE_USE %in% c('BAG LIMIT', 'CREW BAG LIMIT')) %>%
  filter(NEVER_IMPLEMENTED == 0) %>%
  ungroup() %>%
  mutate(START_YEAR = format(START_DATE2, "%Y"),
         START_DATE3 = format(START_DATE2, "%m/%d/%Y"),
         END_DATE3 = format(END_DATE2, "%m/%d/%Y"),
         SECTOR2 = str_to_title(paste0(SECTOR_USE, "\n", SUBSECTOR_USE)),
         ZONE2 = str_to_title(paste0(REGION, "\n", ZONE_USE)),
         VALUE2 = case_when(VALUE_UNITS == 'NUMBERS' & VALUE_TYPE == 'COUNT' & FLAG == 'YES' ~ paste0(VALUE, " fish*"),
                            VALUE_UNITS == 'NUMBERS' & VALUE_TYPE == 'COUNT' & FLAG == 'NO' ~ paste0(VALUE, " fish"),
                            TRUE ~ paste0(VALUE, " ", str_to_title(VALUE_UNITS))),
         VALUE_RATE2 = str_to_title(VALUE_RATE),
         ACTION2 = case_when(is.na(AMENDMENT_NUMBER) & !is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE)),
                             is.na(AMENDMENT_NUMBER) & is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION)),
                             TRUE ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE, " ", AMENDMENT_NUMBER)))) %>%
  arrange(SECTOR_USE, ZONE2, START_DATE2) 
tab_bag2 <- tab_bag %>%
  select(CLUSTER, COMMON_NAME_USE, REGION, SECTOR_USE, MANAGEMENT_TYPE_USE, SECTOR2, ZONE2, START_YEAR, START_DATE3, END_DATE3, VALUE2, VALUE_RATE2, FR_CITATION, ACTION2) %>%
  group_by(CLUSTER, COMMON_NAME_USE, REGION, SECTOR_USE, MANAGEMENT_TYPE_USE) %>%
  do(tab = flextable(.[6:14]) %>%
       set_header_labels(SECTOR2 = "Fishery",
                         ZONE2 = "Region Affected",
                         START_YEAR = "Start Year",
                         START_DATE3 = "Effective Date",
                         END_DATE3 = "End Date",
                         VALUE2 = "Bag Limit",
                         VALUE_RATE2 = "Rate",
                         FR_CITATION = "FR Reference(s)",
                         ACTION2 = "Amendment Number or Rule Type") %>%
       merge_v(j = 1, part = "body") %>%
       merge_v(j = 2, part = "body") %>%
       merge_v(j = 6, part = "body") %>%
       merge_v(j = 7, part = "body") %>%
       theme_box() %>%
       hline_top(part = "header", border = fp_border(color = "black", width = 2)) %>%
       hline_bottom(part = "header", border = fp_border(color = "black", width = 2)) %>%
       fontsize(part = "all", size = 12) %>%
       font(part = "all", fontname = "Times New Roman") %>%
       align(part = "all", align = "center") %>%
       width(j=c(2,9), width=1.8) %>%
       width(j=c(3), width=0.45) %>%
       width(j=c(1,4:8), width=0.9) %>%
       set_caption(paste(tab_size$COMMON_NAME_USE)))
for(i in 1:nrow(tab_bag2)) {
  print(tab_bag2$tab[[i]])
}

# Table for One-Time Closures
tab_close_simple <- mh_expanded %>%
  filter(COMMON_NAME_USE == spp, FMP == fmp, MANAGEMENT_TYPE_USE == 'CLOSURE', STATUS_TYPE == 'SIMPLE') %>%
  filter(NEVER_IMPLEMENTED == 0) %>%
  filter(ZONE_USE == 'ALL') %>%
  ungroup() %>%
  mutate(START_YEAR = format(START_DATE2, "%Y"),
         START_DATE3 = format(START_DATE2, "%m/%d/%Y"),
         END_YEAR = format(END_DATE2, "%Y"),
         END_DATE3 = format(END_DATE2, "%m/%d/%Y"),
         SECTOR2 = str_to_title(paste0(SECTOR_USE, "\n", SUBSECTOR_USE)),
         ZONE2 = str_to_title(paste0(REGION, "\n", ZONE_USE)),
         VALUE2 = case_when(VALUE == 'CLOSE' ~ 'Closure',
                            VALUE == 'OPEN' ~ 'Reopening'),
         REG_TYPE = case_when(MANAGEMENT_STATUS_USE == 'ONCE' ~ str_to_title(VALUE2),
                              TRUE ~ paste0(str_to_title(MANAGEMENT_STATUS_USE), " ", str_to_title(VALUE2))),
         START_MONTH2 = format(as.Date(paste0("2021-", START_MONTH, "-01"), "%Y-%m-%d"), "%b"),
         END_MONTH2 = format(as.Date(paste0("2021-", END_MONTH, "-01"), "%Y-%m-%d"), "%b")) %>%
  arrange(SECTOR_USE, ZONE2, START_DATE2) 
tab_close_simple2 <- tab_close_simple %>%
  select(CLUSTER, COMMON_NAME_USE, REGION, SECTOR_USE, SUBSECTOR_USE, MANAGEMENT_TYPE_USE, SECTOR2, ZONE2, REG_TYPE, START_YEAR, START_DATE3, END_DATE3, FR_CITATION) %>%
  group_by(CLUSTER, COMMON_NAME_USE, REGION, SECTOR_USE, SUBSECTOR_USE, MANAGEMENT_TYPE_USE) %>%
  do(tab = flextable(.[7:13]) %>%
       set_header_labels(SECTOR2 = "Fishery",
                         ZONE2 = "Region Affected",
                         REG_TYPE = "Regulation Type",
                         START_YEAR = "Start Year",
                         START_DATE3 = "First Day in Effect",
                         END_DATE3 = "Last Day in Effect",
                         FR_CITATION = "FR Reference(s)") %>%
       merge_v(j = 1, part = "body") %>%
       merge_v(j = 2, part = "body") %>%
       theme_box() %>%
       hline_top(part = "header", border = fp_border(color = "black", width = 2)) %>%
       hline_bottom(part = "header", border = fp_border(color = "black", width = 2)) %>%
       fontsize(part = "all", size = 12) %>%
       font(part = "all", fontname = "Times New Roman") %>%
       align(part = "all", align = "center") %>%
       width(j=c(3), width=0.45) %>%
       width(j=c(1,4:6), width=0.9) %>%
       set_caption(paste(tab_size$COMMON_NAME_USE)))
for(i in 1:nrow(tab_close_simple2)) {
  print(tab_close_simple2$tab[[i]])
}

# Table for Recurring Closures
tab_close_recur <- mh_expanded %>%
  filter(COMMON_NAME_USE == spp, FMP == fmp, MANAGEMENT_TYPE_USE == 'CLOSURE', STATUS_TYPE != 'SIMPLE') %>%
  filter(NEVER_IMPLEMENTED == 0) %>%
  filter(ZONE_USE == 'ALL') %>%
  ungroup() %>%
  mutate(START_YEAR = format(START_DATE2, "%Y"),
         START_DATE3 = format(START_DATE2, "%m/%d/%Y"),
         END_YEAR = format(END_DATE2, "%Y"),
         END_DATE3 = format(END_DATE2, "%m/%d/%Y"),
         SECTOR2 = str_to_title(paste0(SECTOR_USE, "\n", SUBSECTOR_USE)),
         ZONE2 = str_to_title(paste0(REGION, "\n", ZONE_USE)),
         VALUE2 = case_when(VALUE == 'CLOSE' ~ 'Closure',
                            VALUE == 'OPEN' ~ 'Reopening'),
         REG_TYPE = case_when(MANAGEMENT_STATUS_USE == 'ONCE' ~ str_to_title(VALUE2),
                              TRUE ~ paste0(str_to_title(MANAGEMENT_STATUS_USE), " ", str_to_title(VALUE2))),
         START_MONTH2 = format(as.Date(paste0("2021-", START_MONTH, "-01"), "%Y-%m-%d"), "%b"),
         END_MONTH2 = format(as.Date(paste0("2021-", END_MONTH, "-01"), "%Y-%m-%d"), "%b"),
         FIRST = case_when(!is.na(START_DAY_OF_WEEK_USE) ~ paste0(str_to_title(START_DAY_OF_WEEK_USE), " ", START_DAY, "-", START_MONTH2),
                           TRUE ~ paste0(START_DAY, "-", START_MONTH2)),
         LAST = case_when(!is.na(END_DAY_OF_WEEK_USE) ~ paste0(str_to_title(END_DAY_OF_WEEK_USE), " ", END_DAY, "-", END_MONTH2),
                          !is.na(END_DAY) & !is.na(END_MONTH) ~ paste0(END_DAY, "-", END_MONTH2),
                          TRUE ~ paste0(format(END_DATE2, "%d"), "-", format(END_DATE2, "%b")))) %>%
  arrange(SECTOR_USE, ZONE2, START_DATE2) 
tab_close_recur2 <- tab_close_recur %>%
  select(CLUSTER, COMMON_NAME_USE, REGION, SECTOR_USE, SUBSECTOR_USE, MANAGEMENT_TYPE_USE, SECTOR2, ZONE2, REG_TYPE, START_YEAR, END_YEAR, START_DATE3, END_DATE3, FIRST, LAST, FR_CITATION) %>%
  group_by(CLUSTER, COMMON_NAME_USE, REGION, SECTOR_USE, SUBSECTOR_USE, MANAGEMENT_TYPE_USE) %>%
  do(tab = flextable(.[7:16]) %>%
       set_header_labels(SECTOR2 = "Fishery",
                         ZONE2 = "Region Affected",
                         REG_TYPE = "Regulation Type",
                         START_YEAR = "Start Year",
                         END_YEAR = "End Year",
                         START_DATE3 = "Effective Date",
                         END_DATE3 = "End Date",
                         FIRST = "First Day in Effect",
                         LAST = "Last Day in Effect",
                         FR_CITATION = "FR Reference(s)") %>%
       merge_v(j = 1, part = "body") %>%
       merge_v(j = 2, part = "body") %>%
       merge_v(j = 8, part = "body") %>%
       merge_v(j = 9, part = "body") %>%
       theme_box() %>%
       hline_top(part = "header", border = fp_border(color = "black", width = 2)) %>%
       hline_bottom(part = "header", border = fp_border(color = "black", width = 2)) %>%
       fontsize(part = "all", size = 12) %>%
       font(part = "all", fontname = "Times New Roman") %>%
       align(part = "all", align = "center") %>%
       width(j=c(2,9), width=1.8) %>%
       width(j=c(3), width=0.45) %>%
       width(j=c(1,4:8), width=0.9) %>%
       set_caption(paste(tab_size$COMMON_NAME_USE)))
for(i in 1:nrow(tab_close_recur2)) {
  print(tab_close_recur2$tab[[i]])
}

# Table for ACLs
tab_acl <- mh_expanded %>%
  filter(COMMON_NAME_USE == spp, FMP == fmp, MANAGEMENT_CATEGORY == 'CATCH LIMITS') %>%
  filter(NEVER_IMPLEMENTED == 0) %>%
  ungroup() %>%
  mutate(START_YEAR = format(START_DATE2, "%Y"),
         START_DATE3 = format(START_DATE2, "%m/%d/%Y"),
         END_DATE3 = format(END_DATE2, "%m/%d/%Y"),
         SECTOR2 = str_to_title(paste0(SECTOR_USE, "\n", SUBSECTOR_USE)),
         ZONE2 = str_to_title(paste0(REGION, "\n", ZONE_USE)),
         START_MONTH2 = format(as.Date(paste0("2021-", START_MONTH, "-01"), "%Y-%m-%d"), "%b"),
         END_MONTH2 = format(as.Date(paste0("2021-", END_MONTH, "-01"), "%Y-%m-%d"), "%b"),
         FIRST = case_when(!is.na(START_DAY_OF_WEEK_USE) ~ paste0(str_to_title(START_DAY_OF_WEEK_USE), " ", START_DAY, "-", START_MONTH2),
                           TRUE ~ paste0(START_DAY, "-", START_MONTH2)),
         LAST = case_when(!is.na(END_DAY_OF_WEEK_USE) ~ paste0(str_to_title(END_DAY_OF_WEEK_USE), " ", END_DAY, "-", END_MONTH2),
                          !is.na(END_DAY) & !is.na(END_MONTH) ~ paste0(END_DAY, "-", END_MONTH2),
                          TRUE ~ paste0(format(END_DATE2, "%d"), "-", format(END_DATE2, "%b"))),
         VALUE2 = case_when(VALUE_UNITS == "POUNDS" & FLAG == 'YES' ~ paste0(format(as.numeric(VALUE), big.mark = ","), " lbs*"),
                            VALUE_UNITS == "POUNDS" & FLAG == 'NO' ~ paste0(format(as.numeric(VALUE), big.mark = ","), " lbs"),
                            VALUE_UNITS == "NUMBERS" & FLAG == 'YES' ~ paste0(format(as.numeric(VALUE), big.mark = ","), " fish*"),
                            VALUE_UNITS == "NUMBERS" & FLAG == 'NO' ~ paste0(format(as.numeric(VALUE), big.mark = ","), " fish"),
                            VALUE_UNITS == "METRIC TONS" & FLAG == 'YES' ~ paste0(format(as.numeric(VALUE), big.mark = ","), " metric tons*"),
                            VALUE_UNITS == "METRIC TONS" & FLAG == 'NO' ~ paste0(format(as.numeric(VALUE), big.mark = ","), " metric tons"),
                            TRUE ~ paste0(VALUE, " ", tolower(VALUE_UNITS))),
         VALUE_TYPE2 = str_to_title(VALUE_TYPE),
         ACTION2 = case_when(is.na(AMENDMENT_NUMBER) & !is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE)),
                             is.na(AMENDMENT_NUMBER) & is.na(ACTION_TYPE) ~ str_to_title(paste0(ACTION)),
                             TRUE ~ str_to_title(paste0(ACTION, " ", ACTION_TYPE, " ", AMENDMENT_NUMBER)))) %>%
  arrange(SECTOR_USE, ZONE2, START_DATE2) 
tab_acl2 <- tab_acl %>%
  select(CLUSTER, COMMON_NAME_USE, REGION, SECTOR_USE, SUBSECTOR_USE, MANAGEMENT_TYPE_USE, SECTOR2, ZONE2, START_YEAR, START_DATE3, END_DATE3, FIRST, LAST, VALUE2, VALUE_TYPE2, FR_CITATION, ACTION2) %>%
  group_by(CLUSTER, COMMON_NAME_USE, REGION, SECTOR_USE, SUBSECTOR_USE, MANAGEMENT_TYPE_USE) %>%
  do(tab = flextable(.[7:17]) %>%
       set_header_labels(SECTOR2 = "Fishery",
                         ZONE2 = "Region Affected",
                         START_YEAR = "Start Year",
                         START_DATE3 = "Effective Date",
                         END_DATE3 = "End Date",
                         FIRST = "First Day in Effect",
                         LAST = "Last Day in Effect",
                         VALUE2 = "Catch Limit",
                         VALUE_TYPE2 = "Type",
                         FR_CITATION = "FR Reference(s)",
                         ACTION2 = "Amendment Number or Rule Type") %>%
       merge_v(j = 1, part = "body") %>%
       merge_v(j = 2, part = "body") %>%
       merge_v(j = 9, part = "body") %>%
       theme_box() %>%
       hline_top(part = "header", border = fp_border(color = "black", width = 2)) %>%
       hline_bottom(part = "header", border = fp_border(color = "black", width = 2)) %>%
       fontsize(part = "all", size = 12) %>%
       font(part = "all", fontname = "Times New Roman") %>%
       align(part = "all", align = "center") %>%
       width(j=c(2,9), width=1.8) %>%
       width(j=c(3), width=0.45) %>%
       width(j=c(1,4:8), width=0.9) %>%
       set_caption(paste(tab_size$COMMON_NAME_USE)))
for(i in 1:nrow(tab_acl2)) {
  print(tab_acl2$tab[[i]])
}
