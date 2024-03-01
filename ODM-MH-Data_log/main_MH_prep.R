# Management History Processing ####

# Load packages ####
#install.packages("librarian")
librarian::shelf(here, tidyverse, lubridate)

# Suppress summarize info ####
options(dplyr.summarize.inform = FALSE)

# Set working directory ####
# Working directory is relative to project root (SEFSC-MH-Processing.Rproj)
here::i_am('ODM-MH-Data_log/main_MH_prep.R')

# Create data directories ####
# Acquire MH data from ODM; have it in the data/raw folder
if (!dir.exists(here("ODM-MH-Data_log", "data"))){ dir.create(here("ODM-MH-Data_log", "data")) }
if (!dir.exists(here("ODM-MH-Data_log", "data", "raw"))){ dir.create(here("ODM-MH-Data_log", "data", "raw")) }
if (!dir.exists(here("ODM-MH-Data_log", "data", "interim"))){ dir.create(here("ODM-MH-Data_log", "data", "interim")) }
if (!dir.exists(here("ODM-MH-Data_log", "data", "interim", "sector_clusters"))){ dir.create(here("ODM-MH-Data_log", "data", "interim", "sector_clusters")) }
if (!dir.exists(here("ODM-MH-Data_log", "data", "interim", "clusters"))){ dir.create(here("ODM-MH-Data_log", "data", "interim", "clusters")) }
if (!dir.exists(here("ODM-MH-Data_log", "data", "results"))){ dir.create(here("ODM-MH-Data_log", "data", "results")) }

# Define end of time series ####
end_timeseries =  as.Date("2021-12-31", "%Y-%m-%d")

# 0A. Load species list ####
# Load work space containing the species list data frame to use in expansion (sp_info_use)
  # Because the data is coming directly from Oracle, the MH00_clean_spp_tables.R script does not need to run each time and will only work if you have an Oralce connection
  # Data are saved as a work space rather than a csv to avoid issues in the expansion merge to join to fields that contain NA or null values
  # To also better handle NA or null fields, the species data was transposed so that species_name_type indicates to original species field name
  # Name is the species aggregate, species group name, or when common name is ALL
  # The MH data set is also transposed to have a single common name field and no null values
sp_info_use = readRDS(here('ODM-MH-Data_log', 'data', 'interim', 'MH_clean_spp_tables.RDS'))

# 0B: Data Formatting ####
  # Data frame result: mh_cleaned
source(here('ODM-MH-Data_log', 'MH00_data_formatting.R'))

# 1: Create new variables ####
  # Data frame result: mh_newvar
source(here('ODM-MH-Data_log', 'MH01_new_variables.R'))

# 2: Create sector and address sector forks ids ####
  # Data frame result: mh_subsect_expanded
source(here('ODM-MH-Data_log', 'MH02_sector_ids.R'))

# 3: Create cluster groupings and cluster ids ####
  # Data frame result: mh_cluster_ids
source(here('ODM-MH-Data_log', 'MH03_cluster_ids.R'))

# 4: Fill in dates ####
# Data frame result: mh_dates
source(here('ODM-MH-Data_log', 'MH04_dates.R'))

# 5: Species expansion and clean up dates ####
  # Data frame result: mh_expanded2 
  # Data frame result: mh_analysis_ready (includes only variables of interest and simple clusters (no multi-reg for now))
source(here('ODM-MH-Data_log', 'MH05_spp_expansion.R'))

# 6: Zone forks
  # Data frame result: mh_zone_join
source(here('ODM-MH-Data_log', 'MH06_zone_forks.R'))

# Idea to add a script here that will link clusters that are dependent on each other to complete the story 
# Closure, fishing year, and fishing season

# Save environment as .RDS file for testing against a static result
saveRDS(mh_data_log_final, here("ODM-MH-Data_log", "data", "results", paste0('MH_DL_', format(Sys.Date(), "%Y%b%d"), '.RDS')))

