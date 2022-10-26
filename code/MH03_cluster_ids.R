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
cluster_files <- dir(here('data/interim/clusters'), full.names = TRUE)

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
              here("data/interim/clusters", paste0("mh_unique_clusters_", format(Sys.Date(), "%d%b%Y"),".csv")))
  }

# Join old CLUSTER groupings with new CLUSTER groupings ####
# Merge old and new CLUSTERS into unique_clusters data frame 
unique_clusters <- rbind(existing_clusters, new_clusters)

  # Join CLUSTERS to entire data set
  # Results in the mh_prep data frame
  mh_prep <- mh_subsect_expanded %>%
    left_join(., unique_clusters, by = cluster.match) %>%
    mutate(REG_CHANGE = 1)

# Create variable to indicate when there are multiple records per cluster per FR_CITATION active at the same time (MULTI_REG) ####
# CREATE: the variable of MULTI_REG to flag cases when there multiple records per FR_CITATION within the same 
# CLUSTER that are effective at the same time
multi_reg <- mh_prep %>%
  group_by(CLUSTER, FR_CITATION) %>%
  summarize(MULTI_REG = as.numeric(duplicated(FR_CITATION))) %>%
  filter(MULTI_REG == 1) %>%
  data.frame() %>%
  distinct()
dim(multi_reg)

  # Join flagged MULTI_REG cases with the full data set 
  # Results in mh_cluster_ids data frame 
  mh_cluster_ids <- mh_prep %>%
    # Identify regulations that are MULTI_REG (same FR_NOTICE within a CLUSTER)
    left_join(., multi_reg, by = c("FR_CITATION", "CLUSTER")) %>%
    # Replace all NAs with 0
    mutate_at("MULTI_REG", ~replace(., is.na(.), 0)) %>%
    # CHECK: Does this CLUSTER have any instances of a MULTI_REG?
    mutate(MULTI_REG_CLUSTER = as.numeric(CLUSTER %in% multi_reg$CLUSTER)) 
  
  