# Script 00
# Data reformatting for R

# Read in data
mh <- read.csv(here('data/raw', "MH_DOWNLOAD_SEP_20_2022.csv"), 
               stringsAsFactors = FALSE,
               colClasses=c("REGULATION_ID" = "numeric",
                            "START_DAY" = "numeric",
                            "START_MONTH" = "numeric",
                            "START_YEAR" = "numeric",
                            "END_DAY" = "numeric",
                            "END_MONTH" = "numeric",
                            "END_YEAR" = "numeric"))

# Reformat data frame for date formats and NAs
mh_cleaned <- mh %>%
  filter(REGULATION_ID != 37359) %>%
  # Format dates to be mm/dd/yyyy to match added data (this may not be an issue in the future when pull directly from the database)
  mutate(EFFECTIVE_DATE = as.Date(EFFECTIVE_DATE, "%m/%d/%Y"),
         INEFFECTIVE_DATE = as.Date(INEFFECTIVE_DATE, "%m/%d/%Y")) %>%
  # Rename regulation ID to match what appears in the database
  #rename(REGULATION_ID = REGULATION_ID.) %>%
  #Remove ="..." characters in species ITIS codes
  mutate(SPECIES_ITIS = gsub('="', '', SPECIES_ITIS),
         SPECIES_ITIS = gsub('"', '', SPECIES_ITIS))%>%
  # Replace all "blank" fields with NA for consistency
  replace(. == '', NA) %>%
  # Remove A or B in FR Citation (example regulation ID = 11514)
  mutate(FR_CITATION = str_remove(FR_CITATION, " [AB]"))
