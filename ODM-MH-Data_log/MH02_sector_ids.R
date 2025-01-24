# Script 2
# Create Sector IDs and Address SECTOR forks by expanding SUBSECTOR information

# Suppress summarize info ####
options(dplyr.summarise.inform = FALSE)

# Overview: ####
  # Define matching variables for sector groupings to create SECTOR_ID
  # Read in files with existing SECTOR_ID 
  # Create new SECTOR_ID for new files
  # Create variables to outline how SUBSECTOR expands
  # Perform expansion by separating SUBSECTOR information into new variable of SUBSECTOR_USE

# Define matching variables for sector groupings to create SECTOR_ID ####
# Develop means to track which SECTOR options have multiple SUBSECTORS and their unique combinations
# Use the variables of MANAGEMENT_TYPE_USE, JURISDICTION, JURISDICTIONAL_WATERS, FMP, SECTOR_USE, REGION, 
# and SPP_NAME to develop SECTOR groupings
sector.match <- c("MANAGEMENT_TYPE_USE",
                  "JURISDICTION", "JURISDICTIONAL_WATERS", "FMP",
                  "SECTOR_USE",
                  "REGION",
                  "SPP_NAME")

# Read in files with existing SECTOR_ID ####
# Read in list of existing sector_precluster files
# This list is grouped by the variables described above and contains a unique SECTOR_ID for each unique sector grouping
  sector_id_files <- dir(here('ODM-MH-Data_log', 'data/interim/sector_clusters'), full.names = TRUE)

  # Read in and combine sector_id_files with existing_sector_clusters
  # This step combines any new sector_id_files with old sector groupings outlined in existing_sector_clusters
  if(length(sector_id_files) > 0) {
    existing_sector_clusters <- sector_id_files %>%
      map(read_csv) %>% 
      reduce(rbind)
  } else {
    existing_sector_clusters <- mh_newvar %>%
      select(one_of(sector.match)) %>%
      filter(FMP == "") %>%
      add_column(SECTOR_ID = NA)
  }

  # CHECK: Get starting number of existing sector groupings for reference
  sector_max = max(c(existing_sector_clusters$SECTOR_ID, 0))

# Create new SECTOR_ID for new files ####
# CREATE: Assign numbers (SECTOR_ID) to any new sector groupings based on newly downloaded data
new_sector_clusters <- mh_newvar %>%
  select(one_of(sector.match)) %>%
  distinct() %>%
  anti_join(existing_sector_clusters, by = c("MANAGEMENT_TYPE_USE", "JURISDICTION", "JURISDICTIONAL_WATERS", "FMP", "SECTOR_USE", "REGION", "SPP_NAME")) %>%
  mutate(SECTOR_ID = (1:n() + sector_max)[seq_len(nrow(.))])

  # WRITE: export new sector groupings into mh_sector_clusters_ CSV 
  if(length(new_sector_clusters$SECTOR_ID) > 0) {
    write_csv(new_sector_clusters, 
              here('ODM-MH-Data_log/data/interim/sector_clusters', paste0("mh_sector_clusters_", format(Sys.Date(), "%d%b%Y"),".csv")))
  }
  
  # Alert the user that new clusters were created
  if(nrow(new_sector_clusters) != 0){
    paste0("File mh_sector_clusters_", format(Sys.Date(), "%d%b%Y"),".csv created")
  }

  # Merge old and new sector groupings into unique_sector_clusters data frame ####
  unique_sector_clusters <- rbind(existing_sector_clusters, new_sector_clusters)

  # Join the unique_sector_clusters data frame to the mh_newvar data frame to 
  # incorporate sector information in the data set
  mh_sector_id <- mh_newvar %>%
    left_join(unique_sector_clusters, 
              by = c("JURISDICTION", "REGION", "JURISDICTIONAL_WATERS", "FMP", 
                     "SECTOR_USE", "SPP_NAME", "MANAGEMENT_TYPE_USE"))

# Create variables to outline how SUBSECTOR expands ####
# Process SECTOR_ID that have multi-SUBSECTOR
# Filter records to include cases where SECTOR has more than one SUBSECTOR for DETAILED MANAGEMENT_TYPE
# and records where MANAGEMENT_CATEGORY is not equal to CATCH LIMITS  
multi_subsector <- mh_sector_id %>%
  filter(DETAILED == "YES") %>%
  filter(MANAGEMENT_CATEGORY != "CATCH LIMITS") %>%
  select(FMP, SECTOR_USE, SECTOR_ID, SUBSECTOR) %>%
  distinct() %>%
  group_by(FMP, SECTOR_USE, SECTOR_ID) %>%
  mutate(subsector_count = length(SUBSECTOR),
         #CREATE: Flag to indicate if SUBSECTOR "ALL" is used
         subsector_all_used = sum(SUBSECTOR == "ALL")) %>%
  filter(subsector_count > 1) %>%
  arrange(SECTOR_ID, SUBSECTOR) %>%
  data.frame()

  # CREATE: the variable SUBSECTOR_KEY to expand SUBSECTOR "ALL" into all components
  multi_subsector_key <- mh_sector_id %>%
    select(FMP, SECTOR_USE, SECTOR_ID, SUBSECTOR, EFFECTIVE_DATE) %>%
    group_by(FMP, SECTOR_USE, SECTOR_ID, SUBSECTOR) %>%
    # CREATE: the variable of start_use to indicate the date of the first time each SUBSECTOR is used 
    summarize(start_use = min(EFFECTIVE_DATE)) %>%
    right_join(., multi_subsector, by = c("FMP", "SECTOR_USE", "SECTOR_ID", "SUBSECTOR")) %>%
    group_by(FMP, SECTOR_USE, SECTOR_ID) %>%
    mutate(date_count = length(unique(start_use))) %>%
    arrange(SECTOR_ID) %>%
    # Remove SECTORS that do not use SUBSECTOR "ALL" 
    # Remove cases when all identified SUBSECTORs started on a single date 
    filter(!(subsector_all_used == 0 & date_count == 1)) %>%
    # Concatenate SUBSECTOR names used within SECTOR_ID 
    group_by(SECTOR_ID) %>%
    mutate(SUBSECTOR_KEY = paste(unique(SUBSECTOR), sep = ",", collapse=', ')) %>%
    data.frame()

  # CREATE: the variable of SUBSECTOR_N to outline how many SUBSECTORS are included in the expansion
  # Currently not being used because we are only looking at GOM Reef Fish
  expand_sector_keys = multi_subsector_key %>%
    select(FMP, SECTOR_ID, SECTOR_USE, SUBSECTOR_KEY) %>%
    group_by(FMP, SECTOR_ID, SECTOR_USE, SUBSECTOR_KEY) %>%
    mutate(SUBSECTOR_N = length(SECTOR_USE)) %>%
    distinct() %>%
    arrange(SUBSECTOR_N) %>%
    data.frame()

  # CHECK: Temporary look at GOM Reef Fish while we get this to include other FMPs 
  quick_look <- multi_subsector_key %>%
    filter(FMP == "REEF FISH RESOURCES OF THE GULF OF MEXICO") 

  # CREATE: variables to track SUBSECTOR expansion process for GOM Reef Fish records
  expand_sector_keys_recGOMRF <- multi_subsector_key %>%
    filter(FMP == "REEF FISH RESOURCES OF THE GULF OF MEXICO",
           SECTOR_USE == "RECREATIONAL") %>%
    select(SECTOR_ID, SECTOR_USE, SUBSECTOR_KEY, subsector_all_used) %>%
    distinct() %>%
    #CREATE: the variables of expand_from, expand_temp, and expand_to to track the SUBSECTOR expansion process
    mutate(column_name = "SUBSECTOR",
           expand_from = case_when(subsector_all_used == 1 ~ "ALL"),
           expand_temp = str_remove(SUBSECTOR_KEY, paste0(expand_from, ", ")),
           expand_to = case_when(str_count(expand_temp, ',') >= 1 ~ expand_temp,
                                 TRUE ~ "MANUAL CHECK"))

  # By removing the variable of ZONE from the sector.match variable,
  # we reduced manual forks for GOM Reef Fish (we may need to perform manual checks for other FMPs)
  # Example of addressing MANAUAL CHECKS from expand_sector_keys_recGOMRF
  expand_sector_keys_recGOMRF_use <- expand_sector_keys_recGOMRF %>%
    mutate(expand_to = case_when(SECTOR_ID == 1110 ~ "FOR-HIRE, PRIVATE",
                                 TRUE ~ expand_to))

  # Create the expansions data frame to include the expand_sector_keys_recGOMRF_use data frame 
  # This will be used to join to mh_sector_id
  expansions <- expand_sector_keys_recGOMRF

# Perform expansion by separating SUBSECTOR information into new variable of SUBSECTOR_USE ####
# Join mh_sector_id to the expansions data frame and use the expand_from and expand_to variables to track the expansion process
# Results in the mh_subsect_expanded data frame
mh_subsect_expanded <- left_join(mh_sector_id, expansions, by = c("SECTOR_USE", "SECTOR_ID")) %>%
  mutate(SUBSECTOR_USE = case_when(!is.na(expand_to) & expand_from == SUBSECTOR ~ expand_to,
                                   TRUE ~ SUBSECTOR)) %>%
  # Expand SUBSECTOR_USE at the commas
  separate_rows(SUBSECTOR_USE, sep = ", ")

  