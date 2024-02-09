# Zone forks complex
#Load packages ####
librarian::shelf(here, tidyverse, lubridate, dplyr, tidyr, neatRanges, splitstackshape, reprex)

# Read in MH Data log ####
mh_data_log <- mh_data_log

# Read in zone_specific_xref
zone_class_xref <- read.csv(here('ODM-MH-Data_log', 'data/raw', "zone_specific_xref.csv"))

merged_data_zone <- mh_data_log %>%
  left_join(zone_class_xref, by = "ZONE_USE")

# Without Closures, Catch Limits, or Trip limits ####
zone_general <- merged_data_zone %>%
   mutate(ZONE_CLASS = case_when(is.na(ZONE_CLASS) ~ "GENERAL",
                                TRUE ~ ZONE_CLASS))

zone_specific_use <- zone_general %>%
  mutate(zone_specific_use = case_when(ZONE_CLASS == "SPECIFIC" ~ 1,
                                       TRUE ~ 0))

zone_general_use <- zone_specific_use %>%
  mutate(zone_general_use = case_when(ZONE_CLASS == "GENERAL" ~ 1,
                                      TRUE ~ 0))
#Remove small areas
gen_areas <- c("SPAWNING SMZS - AREA 53", "SPAWNING SMZS - AREA 51", "SPAWNING SMZS - DEVIL'S HOLE/GEORGETOWN HOLE",
               "SPAWNING SMZS - SOUTH OF CAPE LOOKOUT NORTH CAROLINA", "SPAWNING SMZS - WARSAW HOLE", "MPA - (I) THROUGH (VIII)",
               "HAPC - OCULINA BANK EXPERIMENTAL CLOSED AREA", "HAPC - OCULINA BANK", "AREA CLOSURE RELATED TO DEEPWATER HORIZON OIL SPILL",
               "THE EDGES", "MADISON-SWANSON SITES AND STEAMBOAT LUMPS", "HAPC - TORTUGAS MARINE RESERVES", "RILEY'S HUMP", "REEF FISH STRESSED AREA",
               "HAPC - WEST AND EAST FLOWER GARDEN BANKS", "HAPC - PULLEY RIDGE", "HAPC - MCGRAIL BANK", "HAPC - STETSON BANK", "REEF FISH LONGLINE AND BUOY GEAR RESTRICTED AREA",
              "HAPC - DEEPWATER CORAL  - (N)(1)(I) THROUGH (V)", "SMZ - (E)(1)(I) THROUGH (XVIII) AND (E)(1)(XXII) THROUGH (XXIX)",
               "SOUTHWEST FLORIDA SEASONAL TRAWL CLOSURE",
               "TORTUGAS SHRIMP SANCTUARY", "BOTTOM LONGLINE REEF FISH FISHERY MANAGEMENT AREA EAST OF 85°30' WEST",
               "FCZ AREA II",  "FCZ AREA I", "SMZ - (E)(1)(I) THROUGH (LI)", "SMZ - (E)(1)(I) THROUGH (X), (E)(1)(XX), AND (E)(1)(XXII) THROUGH (XXXIX)",
              "SMZ - (A)(20)", "SMZ - (A)(20) AND (A)(22)", "SMZ - ALABAMA", "SMZ - (A)(22)", "SMZ - (A)(1) THROUGH (A)(21)", "SMZ - (A)1 THROUGH (A)19",
              "SMZ - (E)(1)(I) THROUGH (X) (E)(1)(XX) AND (E)(1)(XXII) THROUGH (XXXIX)", "SMZ - (A)(1) THROUGH (A)(10) AND (A)(22) THROUGH (A)(29)", 
              "SMZ - (A)(1) THROUGH (A)(10)", "SMZ - (A)(22)", "SMZ - (E)(1)(I) THROUGH (E)(1)(XVIII) AND (E)(1)(XXII) THROUGH (E)(1)(XXXIX)",
              "SMZ - (A)(20) AND (A)(21)", "SMZ - OFF SOUTH CAROLINA", "SMZ - PA - 04 RON MCMANUS MEMORIAL REEF", "SMZ - OFF NORTH CAROLINA", "HAPC - DEEPWATER CORAL  - POURTALES TERRACE GOLDEN CRAB SOUTHERN ZONE ACCESS AREA",
              "HAPC - DEEPWATER CORAL  - STETSON-MIAMI TERRACE GOLDEN CRAB MIDDLE ZONE ACCESS AREA A", "HAPC - DEEPWATER CORAL  - STETSON-MIAMI TERRACE GOLDEN CRAB MIDDLE ZONE ACCESS AREA B",
              "HAPC - DEEPWATER CORAL  - STETSON_MIAMI TERRACE GOLD CRAB MIDDLE ZONE ACCESS AREA C", "HAPC - DEEPWATER CORAL  - STETSON-MIAMI TERRACE GOLDEN CRAB NORTHERN ZONE ACCESS AREA",
              "HAPC - DEEPWATER CORAL  - STETSON-MIAMI TERRACE SHRIMP ACCESS AREA 1", "HAPC - DEEPWATER CORAL  - STETSON-MIAMI TERRACE SHRIMP ACCESS AREA 2", 
              "HAPC - DEEPWATER CORAL  - STETSON-MIAMI TERRACE SHRIMP ACCESS AREA 4", "SMZ - (A)(1) THROUGH (A)(18) AND (A)(22) THROUGH (A)(29)", "SMZ - (E)(1)(I) THROUGH (XVIII) AND (E)(1)(XXII) THROUGH (LI)",
              "SMZ - (E)(1)(XII) THROUGH (XVIII) AND (E)(1)(XL) THROUGH (LI)")

# Create interest_cluster flag to indicate the cases we are interested in
zone_interest <- zone_general_use %>%
  group_by(CLUSTER) %>%
  mutate(count = n_distinct(ZONE_USE),
         interest_cluster = case_when(count > 1 ~ 1,
                                      TRUE ~ 0)) %>%
  select(-count)

# Create zone_specific_multi to indicate cases where a record is specific and has the matching FR_CITATION and & START_DATE2 of another record (>2 zones replace 1) and its not a zone removal
zone_specific <- zone_interest %>%
  filter(interest_cluster == 1) %>%
  filter(DETAILED == "YES") %>%
  filter(!(MANAGEMENT_TYPE_USE == "CLOSURE" | MANAGEMENT_CATEGORY == "CATCH LIMITS" | MANAGEMENT_TYPE_USE == "TRIP LIMIT")) %>%
  filter(!(ZONE_USE %in% gen_areas)) %>%
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

# Create zone_link_na to indicate cases that should not be linked
zone_link_na <- zone_specific_remove2 %>%
  ungroup() %>%
  group_by(CLUSTER, ZONE_USE) %>%
  mutate(zone_link_na = case_when(zone_specific_multi == 1 & lead(zone_specific_multi_remove, 1) == 1 ~ 1,
                                  TRUE ~ 0))

# Create end_regulation to indicate the final FR CITATION in a cluster
zone_link_end <- zone_link_na %>%
  group_by(CLUSTER) %>%
  mutate(zone_link_end = case_when(FR_CITATION == max(FR_CITATION) ~ 1,
                                   TRUE ~ 0))

# create species_all flag to properly link expanded records to their counterpart.
species_all <- zone_link_end %>%
  group_by(CLUSTER) %>%
  mutate(species_all = case_when(SPP_NAME == "ALL" & COMMON_NAME_USE != lead(COMMON_NAME_USE, 1) & START_DATE2 == lead(START_DATE2, 1) ~ 1,
                                 SPP_NAME == "ALL" & COMMON_NAME_USE != lead(COMMON_NAME_USE, 1) & START_DATE2 != lead(START_DATE2, 1) ~ 1,
                                 TRUE ~0 ))

#Create flags based on specific cases that will be used to link properly
zone_ordering <- species_all %>%
  group_by(CLUSTER) %>%
  arrange(START_DATE2, ZONE_USE) %>%
  filter(NEVER_IMPLEMENTED == 0) %>%
  mutate(zone_general_single = case_when(zone_general_use == 1 & lead(zone_specific_use, 1) == 1 & lead(zone_specific_single, 1) == 1 & REG_REMOVED == 0 ~ 1),
         zone_general_multi = case_when(zone_general_use == 1 & lead(zone_specific_multi, 1) == 1 & REG_REMOVED == 0 ~ 1),
         zone_specific_second = case_when(zone_specific_multi == 1 & lead(zone_general_use, 1) == 1 ~ 1),
         zone_specific_first = case_when(zone_specific_multi == 1 & lead(zone_specific_multi, 1) == 1 & lead(zone_general_use, 2) == 1 ~ 1),
         zone_specific_nochange = case_when(ZONE_CLASS == "SPECIFIC" & lead(ZONE_CLASS, 1) == "SPECIFIC" & lead(zone_specific_single, 1) == 1 & ZONE_USE != lead(ZONE_USE,1) ~ 1),
         zone_specific_nochange_general = case_when(zone_specific_nochange == 1 & lead(ZONE_CLASS, 2) == "GENERAL" ~ 1),
         zone_link_final_specific = case_when(ZONE_CLASS == "SPECIFIC" & zone_specific_single == 1 & lead(zone_link_end, 1) == 1 & lead(ZONE_CLASS, 1) == "GENERAL" ~ 1),
         zone_general_general = case_when(ZONE_CLASS == "GENERAL" & lead(ZONE_CLASS, 1) == "GENERAL" & ZONE_USE != lead(ZONE_USE, 1) ~ 1),
         zone_general_general_value = case_when(ZONE_CLASS == "GENERAL" & lead(ZONE_CLASS,1) == "GENERAL" & ZONE_USE == lead(ZONE_USE, 1) & VALUE_RATE != lead(VALUE_RATE, 1) ~ 1),
         zone_general_general_nochange = case_when(ZONE_CLASS == "GENERAL" & lead(ZONE_CLASS,1) == "GENERAL" & ZONE_USE == lead(ZONE_USE, 1) ~ 1),
         zone_general_general4 = case_when(ZONE_CLASS == "GENERAL" & lead(REG_REMOVED, 1) == 1 & lead(REG_REMOVED, 2) == 1 & lead(REG_REMOVED, 3) == 1 & lead(ZONE_CLASS, 4) == "GENERAL" ~ 1),
         zone_specific_general_end = case_when(zone_specific_first == 1 & lead(zone_link_end, 2) == 1 & lead(zone_specific_single, 2) == 1 & ZONE_USE != lead(ZONE_USE,2) ~ 1),
         zone_specific_general1 = case_when(zone_specific_multi == 1 & ZONE_USE != lead(ZONE_USE, 1) & lead(ZONE_CLASS, 1) == "GENERAL" ~ 1),
         zone_specific_general2 = case_when(zone_specific_multi == 1 & ZONE_USE != lead(ZONE_USE, 1) & ZONE_USE != lead(ZONE_USE, 2) & lead(ZONE_CLASS, 2) == "GENERAL" ~ 1),
         zone_specific_general3 = case_when(zone_specific_multi == 1 & ZONE_USE != lead(ZONE_USE, 1) & ZONE_USE != lead(ZONE_USE, 2) & ZONE_USE != lead(ZONE_USE, 3) & lead(ZONE_CLASS, 3) == "GENERAL" ~ 1),
         zone_specific_general4 = case_when(zone_specific_multi == 1 & ZONE_USE != lead(ZONE_USE, 1) & ZONE_USE != lead(ZONE_USE, 2) & ZONE_USE != lead(ZONE_USE, 3) & ZONE_USE != lead(ZONE_USE, 4) & lead(ZONE_CLASS, 4) == "GENERAL" ~ 1),
         zone_specific_general5 = case_when(zone_specific_multi == 1 & ZONE_USE != lead(ZONE_USE, 1) & ZONE_USE != lead(ZONE_USE, 2) & ZONE_USE != lead(ZONE_USE, 3) & ZONE_USE != lead(ZONE_USE, 4) & ZONE_USE != lead(ZONE_USE, 5) & lead(ZONE_CLASS, 5) == "GENERAL" ~ 1),
         zone_specific_general6 = case_when(zone_specific_multi == 1 & ZONE_USE != lead(ZONE_USE, 1) & ZONE_USE != lead(ZONE_USE, 2) & ZONE_USE != lead(ZONE_USE, 3) & ZONE_USE != lead(ZONE_USE, 4) & ZONE_USE != lead(ZONE_USE, 5) & ZONE_USE != lead(ZONE_USE, 6) & lead(ZONE_CLASS, 6) == "GENERAL" ~ 1),
         zone_specific_general7 = case_when(zone_specific_multi == 1 & ZONE_USE != lead(ZONE_USE, 1) & ZONE_USE != lead(ZONE_USE, 2) & ZONE_USE != lead(ZONE_USE, 3) & ZONE_USE != lead(ZONE_USE, 4) & ZONE_USE != lead(ZONE_USE, 5) & ZONE_USE != lead(ZONE_USE, 6) & lead(ZONE_CLASS, 7) == "GENERAL" ~ 1),
         zone_specific_specific_general1 = case_when(zone_specific_multi == 1 & lead(zone_specific_single, 1) == 1 & ZONE_USE != lead(ZONE_USE, 1) & lead(ZONE_CLASS, 2) == "GENERAL" ~ 1),
         zone_specific_general_single1 = case_when(zone_specific_single == 1 & ZONE_USE != lead(ZONE_USE, 1) & lead(ZONE_CLASS, 1) == "GENERAL" ~ 1),
         zone_general_specific_single = case_when(zone_general_single == 1 & lead(zone_specific_single, 1) == 1 ~ 1),
         zone_specific_specific1 = case_when(ZONE_CLASS == "SPECIFIC" & lead(ZONE_CLASS, 1) == "SPECIFIC" & ZONE_USE == lead(ZONE_USE, 1) ~ 1),
         zone_specific_specific2 = case_when(ZONE_CLASS == "SPECIFIC" & lead(ZONE_CLASS, 2) == "SPECIFIC" & ZONE_USE == lead(ZONE_USE, 2) ~ 1),
         zone_specific_specific3 = case_when(ZONE_CLASS == "SPECIFIC" & lead(ZONE_CLASS, 3) == "SPECIFIC" & ZONE_USE == lead(ZONE_USE, 3) ~ 1),
         zone_specific_specific4 = case_when(ZONE_CLASS == "SPECIFIC" & lead(ZONE_CLASS, 4) == "SPECIFIC" & ZONE_USE == lead(ZONE_USE, 4) ~ 1),
         zone_specific_specific_multi = case_when(zone_specific_single == 1 & lead(zone_specific_multi, 1) == 1 & ZONE_USE != lead(ZONE_USE) & lead(zone_link_end, 1) == 1 ~ 1),
         zone_specific_specific_change = case_when(ZONE_CLASS == "SPECIFIC" & lead(ZONE_CLASS, 1) == "SPECIFIC" & ZONE_USE != lead(ZONE_USE, 1) & zone_specific_single == 1 & lead(zone_specific_single, 1) == 1 & zone_specific_nochange == 1 & lead(zone_link_end, 1) == 1 ~ 1))

# Create ZONE_LINK
zone_link <- zone_ordering %>%
  group_by(CLUSTER) %>%
  arrange(START_DATE2, ZONE_USE) %>%
  mutate(ZONE_LINK = case_when(zone_link_na == 1 & zone_specific_specific_general1 == 1 ~ lead(START_DATE2, 2),
                               zone_link_na == 1 ~ NA,
                               REG_REMOVED == 1 ~ NA,
                               species_all == 1 ~ NA,
                               zone_general_general_value == 1 ~ NA,
                               zone_specific_nochange_general == 1 ~ lead(START_DATE2, 2),
                               zone_specific_general_end == 1 ~ NA,
                               zone_general_multi == 1 ~ lead(START_DATE2, 1),
                               zone_specific_first == 1 ~ lead(START_DATE2, 2),
                               zone_specific_second == 1 ~ lead(START_DATE2, 1),
                               zone_link_final_specific == 1 ~ lead(START_DATE2, 1),
                               zone_general_general == 1 ~ lead(START_DATE2,1),
                               zone_general_general_nochange == 1 ~ lead(START_DATE2,1), 
                               zone_general_general4 == 1 ~ lead(START_DATE2, 4),
                               zone_specific_general1 == 1 ~ lead(START_DATE2,1),
                               zone_specific_general2 == 1 ~ lead(START_DATE2,2),
                               zone_specific_general3 == 1 ~ lead(START_DATE2, 3),
                               zone_specific_general4 == 1 ~ lead(START_DATE2, 4),
                               zone_specific_general5 == 1 ~ lead(START_DATE2, 5),
                               zone_specific_general6 == 1 ~ lead(START_DATE2, 6),
                               zone_specific_general7 == 1 ~ lead(START_DATE2, 7),
                               zone_specific_general_single1 == 1 ~ lead(START_DATE2, 1),
                               zone_general_specific_single == 1 ~ lead(START_DATE2,1),
                               zone_specific_specific1 == 1 ~ lead(START_DATE2, 1),
                               zone_specific_specific2 == 1 ~ lead(START_DATE2, 2),
                               zone_specific_specific3 == 1 ~ lead(START_DATE2, 3),
                               zone_specific_specific4 == 1 ~ lead(START_DATE2, 3),
                               zone_specific_specific_multi == 1 ~ lead(START_DATE2, 1),
                               zone_specific_specific_change == 1 ~ lead(START_DATE2, 1)))


zone_species_all <- zone_link %>%
  group_by(CLUSTER, COMMON_NAME_USE) %>%
  mutate(zone_link2 = case_when(species_all == 1 ~ lead(START_DATE2, 1))) %>%
  ungroup()

# Fix END_DATE2
zone_end <- zone_species_all %>%
  mutate(END_DATE2 = case_when(!is.na(ZONE_LINK) & ZONE_LINK < END_DATE2 ~ ZONE_LINK - 1,
                               !is.na(zone_link2) & zone_link2 < END_DATE2 ~ zone_link2 - 1,
                               TRUE ~ END_DATE2))         

# Create REVERSION_REMOVE to indicate cases when reversion records do not go into effect because they are overwritten
zone_reversion <- zone_end %>%
  group_by(CLUSTER) %>%
  mutate(reversion_remove = case_when(REVERSION == "TRUE" & START_DATE2 > END_DATE2 ~ 1,
                                      TRUE ~ 0))
zone_reversion_remove <- zone_reversion %>%
  group_by(CLUSTER) %>%
  filter(reversion_remove == 0)

# Remove flags
zone_remove_flags <- zone_reversion_remove %>%
  select(-c(zone_specific_use, zone_general_use, zone_general_general_value, zone_specific_multi, zone_specific_single,
            zone_specific_multi_remove, zone_specific_single_remove, zone_link_na, zone_link_end, zone_general_single, zone_general_multi, zone_specific_first,zone_specific_second, zone_specific_nochange, zone_specific_nochange_general, zone_link_final_specific, 
            zone_general_general, zone_general_general_nochange, zone_general_general4, zone_specific_general_end, zone_specific_general1, zone_specific_general2,
            zone_specific_general3, zone_specific_general4, zone_specific_general5, zone_specific_general6, zone_specific_general7,
            zone_specific_specific_general1, zone_specific_general_single1, zone_general_specific_single, zone_specific_specific1,
            zone_specific_specific2, zone_specific_specific3, zone_specific_specific4, zone_specific_specific_change, zone_specific_specific_multi, species_all, reversion_remove, ZONE_LINK, zone_link2))

# Join Zone forks with zone_interest
zone_interest_filter <- zone_interest %>%
  filter(!(CLUSTER %in% c("342","343","1","2",
     "360",
     "296",
     "297",
      "80",
     "118",
     "119",
      "30",
    "1043",
      "39",
      "33",
    "1042",
    "1607",
    "1611",
    "2003",
    "1173",
    "1174",
    "1617",
    "1618",
      "10",
      "11",
      "50",
      "49",
      "54",
      "79",
      "53",
     "282",
     "283",
    "1132",
     "511",
    "1484",
    "1485",
    "1905",
     "241",
     "952",
    "1202",
    "1048",
      "99",
     "100",
     "101",
     "102",
     "382",
     "383",
     "393",
     "843",
     "844",
     "711",
     "718",
     "723",
     "725",
     "727",
     "729",
   "2026",
     "226",
   "108",
   "275",
   "276",
   "281",
   "1101")))

mh_data_log_final <- bind_rows(zone_interest_filter, zone_remove_flags_filter, trip)%>%
  arrange(CLUSTER)%>%
  select(-zone_specific_use, -zone_general_use)
  
saveRDS(mh_data_log_final, here("ODM-MH-Data_log", "data", "results", paste0('MH_DL_', format(Sys.Date(), "%Y%b%d"), '.RDS')))


