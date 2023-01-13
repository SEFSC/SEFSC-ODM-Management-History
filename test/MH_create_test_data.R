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
  # Commercial and recreational size limits for Gulf red snapper = cluster 199 & 220
  # Recreational bag limits for Gulf red snapper = cluster 128
  # Recreational crew bag limits for Gulf red snapper = cluster 200
  # Commercial trip limits for South Atlantic gray triggerfish (all gears and trawl gear) = cluster 1904 & 951
  # Commercial and recreational size limits for South Atlantic gray triggerfish = cluster 241 & 952
  # Recreational bag limits for South Atlantic gray triggerfish = cluster 1994
  # Recreational closures for South Atlantic gray triggerfish = cluster 1527
  # Recreational and commercial size limits for Gulf Spanish mackerel - 19 & 20
  # Commercial one-time closures for Gulf Spanish mackerel - 1268, 1267, 1266, 1265, 1210, 1098
  # Recreational one-time closures for Gulf Spanish mackerel - 1097
  # Recreational and commercial prohibited gear for Gulf Spanish mackerel - 51, 52, 640, 641, 642, 643, 70, 71, 552, 553
  # Commercial gill net specifications for Gulf Spanish mackerel - 577
  # Recreational and commercial landed intact for Gulf Spanish mackerel - 519 & 520

test <- mh_analysis_ready %>% filter(CLUSTER %in% c(952, 365, 199, 220, 128, 200,
                                                    1904, 951, 241, 952, 1994, 1527,
                                                    19, 20, 1268, 1267, 1266, 1265, 1210, 1098, 1097,
                                                    51, 52, 640, 641, 642, 643, 70, 71, 552, 553,
                                                    577, 519, 520)) %>%
  arrange(CLUSTER, ZONE_USE, START_DATE2)

# Save test data set
saveRDS(test, here('test', 'MH_test_data.RDS'))

# Cases to add once we know its working
  # Commercial closures for Gulf of Mexico red snapper = cluster 306 - has reopenings and recurring