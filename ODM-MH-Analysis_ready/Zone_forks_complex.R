# Zone forks complex
#Load packages ####
librarian::shelf(here, tidyverse, lubridate, dplyr, tidyr, neatRanges, splitstackshape)

# Read in MH Data log ####
mh_data_log <- readRDS(here("ODM-MH-Data_log", "data", "results", "MH_DL_2024Jan08.RDS"))

# Read in zone_specific_xref
zone_class_xref <- read.csv(here('ODM-MH-Data_log', 'data/raw', "zone_specific_xref.csv"))

merged_data_zone <- mh_data_log %>%
  left_join(zone_class_xref, by = "ZONE_USE")

zone_general <- merged_data_zone %>%
  mutate(ZONE_CLASS = case_when(is.na(ZONE_CLASS) ~ "GENERAL",
                                TRUE ~ ZONE_CLASS))

# Create zone_class to indicate whether the zone within cluster 30 is GENERAL or SPECIFIC 
#zone_class <- mh_data_log %>%
  #filter(CLUSTER == "30") %>%
  #mutate(ZONE_CLASS = case_when(ZONE_USE == "ATLANTIC MIGRATORY GROUP KING MACKEREL" ~ "GENERAL",
   #                             TRUE ~ "SPECIFIC"))

# Create zone_specific_multi to indicate cases where a record is specific and has the matching FR_CITATION and & START_DATE2 of another record (>2 zones replace 1) and its not a zone removal
zone_specific <- zone_general %>%
  group_by(CLUSTER, FR_CITATION, START_DATE2) %>%
  mutate(zone_specific_multi = case_when(ZONE_CLASS == "SPECIFIC" & n()>1 & START_DATE2 != END_DATE2 ~ 1,
                                         TRUE ~0))%>%
  ungroup()
         
# Create zone_specific_single to indicate cases where a record is specific and doesn't have a matching FR_CITATION & START_DATE2 of another record and it is not a zone removal
zone_specific_single <- zone_specific %>%
  group_by(CLUSTER, FR_CITATION, START_DATE2) %>%
  mutate(zone_specific_single = case_when(ZONE_CLASS == "SPECIFIC" & n() == 1 & START_DATE2 != END_DATE2 ~ 1,
                                          TRUE ~ 0)) %>%
  ungroup()

# Create zone_specific_multi_remove to indicate cases where a record is specific and has the matching FR_CITATION and & START_DATE2 of another record (>2 zones replace 1) and its a zone removal
zone_specific_remove <- zone_specific_single %>%
  group_by(CLUSTER, FR_CITATION, START_DATE2) %>%
  mutate(zone_specific_multi_remove = case_when(ZONE_CLASS == "SPECIFIC" & n()>1 & START_DATE2 == END_DATE2 ~ 1,
                                         TRUE ~0))%>%
  ungroup()

# Create zone_specific_single_remove to indicate cases where a record is specific and doesn't have a matching FR_CITATION & START_DATE2 of another record and it is a zone removal
zone_specific_remove2 <- zone_specific_remove %>%
  group_by(CLUSTER, FR_CITATION, START_DATE2) %>%
  mutate(zone_specific_single_remove = case_when(ZONE_CLASS == "SPECIFIC" & n() == 1 & START_DATE2 == END_DATE2 ~ 1,
                                          TRUE ~ 0)) %>%
  ungroup()

# Create zone_specific_combined to indicate cases where zone_specific_multi or zone_specific_single are flagged
zone_specific_combined <- zone_specific_remove2 %>%
  mutate(zone_specific_combined = case_when(zone_specific_multi == 1 | zone_specific_single == 1 ~ 1,
                                            TRUE ~0))

# Create zone_specific_combined_remove to indicate cases where a specific zone is removed and other specific zones are implemented
zone_specific_combined_remove <- zone_specific_combined %>%
  mutate(zone_specific_combined_remove = case_when(zone_specific_multi_remove == 1 | zone_specific_single_remove == 1 ~ 1,
                                            TRUE ~0))

# Create zone_link_prep
zone_link_prep <- zone_specific_combined_remove %>%
  mutate(zone_link_prep = case_when(zone_specific_multi == 1 & lag(zone_specific_single, 2) == 1 & ZONE_USE != lag(ZONE_USE, 2) ~ 1,
                                    TRUE ~ 0))

# Create end_regulation
zone_link_end <- zone_link_prep %>%
  group_by(CLUSTER) %>%
  mutate(zone_link_end = case_when(FR_CITATION == max(FR_CITATION) & END_DATE2 == max(END_DATE2) ~ 1,
                                   TRUE ~ 0))

# Create ZONE_LINK
zone_link_final <- zone_link_end %>%
  mutate(ZONE_LINK_FINAL = case_when(REG_REMOVED == 1 ~ NA,
                                     zone_specific_multi == 1 & lag(REG_REMOVED, 1) == 1 & ZONE_USE == lag(ZONE_USE, 1) ~ NA,
                                     zone_specific_combined == 1 & zone_link_prep == 1 & lag(zone_link_end, 2) == 1 ~ NA,
                                     zone_specific_combined == 0 & lag(zone_specific_combined, 1) == 0 ~ NA,
                                     zone_specific_combined == 0 & lag(zone_specific_combined, 1) == 1 & lag(zone_specific_multi, 1) == 0 & lag(zone_specific_single, 1) == 1 ~ lag(START_DATE2, 1),
                                     zone_specific_combined == 0 & lag(zone_specific_combined, 1) == 1 & lag(zone_specific_multi, 1) == 1 & lag(zone_specific_multi, 2) == 0 ~ lag(START_DATE2, 1),
                                     zone_specific_combined == 0 & lag(zone_specific_combined, 1) == 1 & lag(zone_specific_multi, 1) == 1 & lag(zone_specific_multi, 2) == 1 & lag(zone_specific_multi, 3) == 0 & lag(START_DATE2, 1) == lag(START_DATE2, 2) ~ lag(START_DATE2, 1),
                                     zone_specific_combined == 1 & lag(zone_specific_combined, 1) == 0 ~ lag(START_DATE2, 1),
                                     zone_specific_combined == 1 & lag(zone_specific_combined, 1) == 1 & lag(zone_specific_combined, 2) == 0 & lag(zone_specific_multi, 1) == 1 ~ lag(START_DATE2, 2),
                                     zone_specific_combined == 1 & lag(zone_specific_combined, 1) == 1 & lag(zone_specific_combined, 2) == 1 & lag(zone_specific_multi, 1) == 1 & lag(zone_specific_single, 2) == 1 ~ lag(START_DATE2, 2),
                                     zone_specific_multi == 1 & lag(zone_specific_single, 2) == 1 &  ZONE_USE != lag(ZONE_USE, 1) & lag(ZONE_USE, 1) == lag(ZONE_USE, 2) & START_DATE2 != lag(START_DATE2, 2) ~ NA))

                               

# Fix END_DATE2
zone_end <- zone_link_final %>%
  mutate(END_DATE2 = case_when(!is.na(ZONE_LINK_FINAL) ~ ZONE_LINK_FINAL - 1,
                               TRUE ~ END_DATE2))
