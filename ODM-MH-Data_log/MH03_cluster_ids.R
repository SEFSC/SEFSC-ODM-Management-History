# Script 3
# Group data into processing CLUSTER groupings and create CLUSTER IDs

# Suppress summarize info ####
options(dplyr.summarise.inform = FALSE)

# Overview: ####
 # Define matching variables for CLUSTER groupings
 # Read in files with existing CLUSTER IDs
 # Create new CLUSTER IDs for new files
 # Join old CLUSTER groupings with new CLUSTER groupings
 # Create variable to indicate when there are multiple records per cluster per FR_CITATION active at the same time (MULTI_REG)

# Define matching variables for CLUSTER groupings ####
# CREATE: the variable of CLUSTER to assist in processing identical MANAGEMENT_TYPE for a species (COMMON_NAME/SPECIES_AGGREGATE/SPECIES_GROUP) 
# within the same FMP and REGION
# CLUSTER groupings assist in accurately creating a time series of particular regulation types for a species
# Define which variables must match for records to be considered part of a CLUSTER
cluster.match <- c("MANAGEMENT_TYPE_USE",
                   "JURISDICTION", "JURISDICTIONAL_WATERS", "FMP",
                   "SECTOR_USE", "SUBSECTOR_USE", "REGION",
                   "SPP_NAME")

# Read in files with existing CLUSTER IDs ####
cluster_files <- dir(here('ODM-MH-Data_log', 'data/interim/clusters'), full.names = TRUE)

  #Create existing_clusters data frame that includes previous CLUSTER groupings 
  if(length(cluster_files) > 0) {
    existing_clusters <- cluster_files %>%
      map(read_csv) %>% 
      reduce(rbind)
  } else {
    existing_clusters <- mh_subsect_expanded %>%
      select(one_of(cluster.match)) %>%
      filter(FMP == "") %>%
      add_column(CLUSTER = NA)
  }

  # CHECK: get starting number of current CLUSTERS for reference
  clusters_max = max(c(existing_clusters$CLUSTER, 0))

# Create new CLUSTER IDs for new files ####
# CREATE: Assign new CLUSTER_ID to any new CLUSTER groupings
new_clusters <- mh_subsect_expanded %>%
  select(one_of(cluster.match)) %>%
  distinct() %>%
  anti_join(existing_clusters, by = c("MANAGEMENT_TYPE_USE", "JURISDICTION", "JURISDICTIONAL_WATERS", "FMP", "SECTOR_USE", "SUBSECTOR_USE", "REGION", "SPP_NAME")) %>%
  mutate(CLUSTER = (1:n() + clusters_max)[seq_len(nrow(.))])

  # Export new CLUSTERs
  if(length(new_clusters$CLUSTER) > 0) {
    write_csv(new_clusters, 
              here("ODM-MH-Data_log/data/interim/clusters", paste0("mh_unique_clusters_", format(Sys.Date(), "%d%b%Y"),".csv")))
  }
  
  # Alert the user that new clusters were created
  if(nrow(new_sector_clusters) != 0){
    paste0("File mh_unique_clusters_", format(Sys.Date(), "%d%b%Y"),".csv created")
  }

# Join old CLUSTER groupings with new CLUSTER groupings ####
# Merge old and new CLUSTERS into unique_clusters data frame 
unique_clusters <- rbind(existing_clusters, new_clusters)

  # Join CLUSTERS to entire data set
  # Results in the mh_prep data frame
  mh_prep <- mh_subsect_expanded %>%
    left_join(., unique_clusters, by = cluster.match) %>%
    mutate(REG_CHANGE = 1)

# Create variable to indicate when there are multiple records per cluster per FR_CITATION active at the same time with different VALUE_TYPE or VALUE_RATE (MULTI_REG_VALUE) ####
# CREATE: the variable of MULTI_REG_VALUE to flag cases when there multiple records per FR_CITATION within the same 
# CLUSTER that are effective at the same time but have different VALUE_TYPEs or VALUE_RATEs (example: CLUSTER 2046)
multi_reg_value <- mh_prep %>%
  group_by(CLUSTER, FR_CITATION, ZONE_USE) %>%
  mutate(MULTI_REG_VALUE = as.numeric(n() > 1 & (n_distinct(VALUE_TYPE) > 1 | n_distinct(VALUE_RATE) > 1))) %>%
  ungroup()
  
# Create variable to indicate when there are multiple records per cluster per FR_CITATION active at the same time but
# have different START_YEARs (MULTI_REG_FORECAST)
# CREATE: the variable of MULTI_REG_FORECAST to flag cases when multiple records per FR_CITATION within the same
# CLUSTER that are effective at the same time but have different START_YEARs (i.e., the record implements regulations for multiple years) (example: CLUSTER 199)
multi_reg_forecast <- multi_reg_value %>%
    group_by(CLUSTER, FR_CITATION, ZONE_USE) %>%
    mutate(MULTI_REG_FORECAST = as.numeric(n_distinct(START_YEAR) >1)) %>%
    ungroup()

# Create variable to indicate when there are multiple records per cluster per FR_CITATION active at the same time but
# are TEMPORAL CONTROLS with START_MONTH and START_DAY (MULTI_REG_CLOSURE)
# CREATE: the variable MULTI_REG_CLOSURE to flag cases when multiple records per FR_CITATION within the same 
# CLUSTER that are effective at the same time but are TEMPORAL CONTROLS with different START_MONTH and START_DAY (example: CLUSTER 280)
#multi_reg_closure <- multi_reg_forecast %>%
 # group_by(CLUSTER, FR_CITATION, EFFECTIVE_DATE, MANAGEMENT_STATUS_USE, ZONE_USE) %>%
  #mutate(MULTI_REG_CLOSURE = as.numeric(n() > 1 & n_distinct(START_MONTH) >1 &
   #                                       n_distinct(START_DAY) &
                                         # all(MANAGEMENT_CATEGORY == "TEMPORAL CONTROLS"))) %>%
  #ungroup()

# Create variable to indicate when there are multiple records per cluster per FR_CITATION active at the same time but
# are SEASONAL
# CREATE: the variable MULTI_REG_SEASONAL to flag cases when multiple records per FR_CITATION within the same
# CLUSTER that are effective at the same time but are SEASONAL with different START_MONTH and START_DAY (example: CLUSTER 1112 & CLUSTER 280)
multi_reg_seasonal <- multi_reg_forecast %>%
  group_by(CLUSTER, FR_CITATION, MANAGEMENT_TYPE, MANAGEMENT_STATUS_USE) %>%
  mutate(MULTI_REG_SEASONAL = as.numeric(n() > 1 & n_distinct(START_MONTH) >1 &
                                           n_distinct(START_DAY))) %>%
  ungroup()
  
  # Join flagged MULTI_REG cases with the full data set 
  # Results in mh_cluster_ids data frame 
  mh_cluster_ids <- multi_reg_seasonal %>%
    # Identify regulations that are MULTI_REG (same FR_NOTICE within a CLUSTER)
    #left_join(., multi_reg_value, by = c("FR_CITATION", "CLUSTER", "ZONE_USE")) %>%
    # Replace all NAs with 0
    # mutate_at("MULTI_REG_VALUE", ~replace(., is.na(.), 0)) %>%
    # CHECK: Does this CLUSTER have any instances of a MULTI_REG?
    mutate(MULTI_REG_CLUSTER = as.numeric(MULTI_REG_VALUE == 1 | MULTI_REG_FORECAST == 1 | MULTI_REG_SEASONAL == 1))
