# Management History Processing ####

# Load packages ####
#install.packages("librarian")
librarian::shelf(here, tidyverse, lubridate)

# Suppress summarize info ####
options(dplyr.summarize.inform = FALSE)

# Set working directory ####
# Working directory is relative to project root (SEFSC-MH-Processing.Rproj)
here::i_am('code/main_MH_prep.R')

# Create data directories ####
# Acquire MH data from ODM; have it in the data/raw folder
if (!dir.exists(here("data"))){ dir.create(here("data")) }
if (!dir.exists(here("data", "raw"))){ dir.create(here("data", "raw")) }
if (!dir.exists(here("data", "interim"))){ dir.create(here("data", "interim")) }
if (!dir.exists(here("data", "interim", "sector_clusters"))){ dir.create(here("data", "interim", "sector_clusters")) }
if (!dir.exists(here("data", "interim", "clusters"))){ dir.create(here("data", "interim", "clusters")) }
if (!dir.exists(here("data", "processed"))){ dir.create(here("data", "processed")) }

# Define end of time series ####
end_timeseries =  as.Date("2021-12-31", "%Y-%m-%d")

# 0A. Load species list ####
# Load work space containing the species list data frame to use in expansion (sp_info_use)
  # Because the data is coming directly from Oracle, the MH00_clean_spp_tables.R script does not need to run each time and will only work if you have an Oralce connection
  # Data are saved as a work space rather than a csv to avoid issues in the expansion merge to join to fields that contain NA or null values
  # To also better handle NA or null fields, the species data was transposed so that species_name_type indicates to original species field name
  # Name is the species aggregate, species group name, or when common name is ALL
  # The MH data set is also transposed to have a single common name field and no null values
sp_info_use = readRDS(here('data', 'interim', 'MH_clean_spp_tables.RDS'))

# 0B: Data Formatting ####
  # Data frame result: mh_cleaned
source(here('code', 'MH00_data_formatting.R'))

# 1: Create new variables ####
  # Data frame result: mh_newvar
source(here('code', 'MH01_new_variables.R'))

# 2: Create sector and address sector forks ids ####
  # Data frame result: mh_subsect_expanded
source(here('code', 'MH02_sector_ids.R'))

# 3: Create cluster groupings and cluster ids ####
  # Data frame result: mh_cluster_ids
source(here('code', 'MH03_cluster_ids.R'))

# 4: Fill in dates ####
  # Data frame result: mh_dates
source(here('code', 'MH04_dates.R'))

# 5: Species expansion and clean up dates ####
# Data frame result: mh_expanded 
# Data frame result: mh_analysis_ready (includes only variables of interest and simple clusters (no multi-reg for now))
source(here('code', 'MH05_spp_expansion.R'))

# Save environment as .Rdata file for testing against a static result
saveRDS(mh_analysis_ready, here("data", "processed", paste0('MH_AL_', format(Sys.Date(), "%Y%b%d"), '.RDS')))
