# Script 00
# Data reformatting for R

# Read in data
mh <- read.csv(here('ODM-MH-Data_log', 'data/raw', "MH_DOWNLOAD_DEC_16_2024.csv"), 
               stringsAsFactors = FALSE,
               colClasses=c("REGULATION_ID" = "numeric",
                            "START_DAY" = "numeric",
                            "START_MONTH" = "numeric",
                            "START_YEAR" = "numeric",
                            "END_DAY" = "numeric",
                            "END_MONTH" = "numeric",
                            "END_YEAR" = "numeric"),
               # Added on 10/26/2022 because of a specific issue in how Sarina & Adyan's Rstudio reads in the csv file
               # For some reason Adyan had all hyphens from species groups re coded to "\x97"
               fileEncoding = 'Windows-1252')

# Reformat data frame for date formats and NAs
mh_cleaned <- mh %>%
  # Format dates to be mm/dd/yyyy to match added data (this may not be an issue in the future when pull directly from the database)
  mutate(EFFECTIVE_DATE = as.Date(EFFECTIVE_DATE, "%m/%d/%Y"),
         INEFFECTIVE_DATE = as.Date(INEFFECTIVE_DATE, "%m/%d/%Y")) %>%
  # Filter to exclude data after your desired end of the time series
  filter(EFFECTIVE_DATE <= end_timeseries) %>%
  # Rename regulation ID to match what appears in the database
  #rename(REGULATION_ID = REGULATION_ID.) %>%
  #Remove ="..." characters in species ITIS codes
  mutate(SPECIES_ITIS = gsub('="', '', SPECIES_ITIS),
         SPECIES_ITIS = gsub('"', '', SPECIES_ITIS)) %>%
  # Issue with species ITIS appearing as "\t173138"
  mutate(SPECIES_ITIS = gsub('\t', '', SPECIES_ITIS)) %>%
  # Replace all "blank" fields with NA for consistency
  replace(. == '', NA) %>%
  # Remove A or B in FR Citation (example regulation ID = 11514)
  mutate(FR_CITATION = str_remove(FR_CITATION, " [AB]")) %>%
  # Remove report data bug variable
  #select(-REPORT_A_DATA_BUG) %>%
  # Add code for getting zone name for 3 regulations that are not getting fixed in the database
  # Sarina created data bugs to update the data, but the change was not reflected
  mutate(ZONE = case_when(REGULATION_ID %in% c(11537, 11538, 11539) ~ "SMZ - PA - 04 RON MCMANUS MEMORIAL REEF",
                          TRUE ~ ZONE))
