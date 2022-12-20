# Create the test data set
# Eventually as we test more cases, we can add more clusters to this data set

# Load packages ####
#install.packages("librarian")
librarian::shelf(here, tidyverse)

# RUN MH CODE
here::i_am('test/MH_create_test_data.R')
source(here('code', 'main_MH_prep.R'))
       
# Current clusters in the test data set are very simple stories
  # Recreational size limits for South Atlantic gray triggerfish = cluster 952 - has different zones
  # Commercial size limits for South Atlantic red snapper = cluster 365 - has reg removed flag

test <- mh_analysis_ready %>% filter(CLUSTER %in% c(952, 365)) %>%
  arrange(CLUSTER, ZONE_USE, START_DATE2)

# Save test data set
saveRDS(test, here('test', 'MH_test_data.RDS'))

# Cases to add once we know its working
  # Recreational bag limits for Gulf of Mexico red snapper = cluster 128 - has adjustments
  # Commercial size limits for Gulf of Mexico red snapper = cluster 199 - has multi reg flag
  # Commercial closures for Gulf of Mexico red snapper = cluster 306 - has reopenings and recurring