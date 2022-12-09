# Create the test data set
# Eventually as we test more cases, we can add more clusters to this data set

# Current clusters in the test data set are very simple stories
  # Recreational size limits for South Atlantic gray triggerfish = cluster 952 - has different zones
  # Commercial size limits for South Atlantic red snapper = cluster 365 - has reg removed flag

test <- mh_expanded %>% filter(CLUSTER %in% c(952, 365)) %>%
  select(CLUSTER, REGULATION_ID, FR_CITATION,
         MANAGEMENT_TYPE_USE, SECTOR_USE, SUBSECTOR_USE, ZONE_USE, SPP_TYPE, SPP_NAME, MANAGEMENT_STATUS_USE,
         REG_REMOVED, ADJUSTMENT, MULTI_REG,
         EFFECTIVE_DATE, INEFFECTIVE_DATE,
         START_DATE2, START_TIME_USE, END_DATE, END_TIME_USE,
         VALUE, VALUE_UNITS, VALUE_TYPE, VALUE_RATE) %>%
  arrange(CLUSTER, ZONE_USE, START_DATE2)

# Keep only test data set in R Environment work space
rm(list=ls()[! ls() %in% c("test")])
# Save test data set
save.image(here('test', './MH_test_data.RData'))

# Cases to add once we know its working
  # Recreational bag limits for Gulf of Mexico red snapper = cluster 128 - has adjustments
  # Commercial size limits for Gulf of Mexico red snapper = cluster 199 - has multi reg flag
  # Commercial closures for Gulf of Mexico red snapper = cluster 306 - has reopenings and recurring