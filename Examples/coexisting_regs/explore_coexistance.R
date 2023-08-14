# Load packages ####

#install.packages("librarian")
librarian::shelf(here, tidyverse)

#READ IN DATA
mh_data_log <- readRDS(here("ODM-MH-Data_log", 
                            "data", 
                            "processed", 
                            "MH_AL_2023Aug08.RDS"))

# SELECT VARIABLES TO IDENTIFY CHANGE IN VALUE VARIABLES WITHIN A CLUSTER
value_setup<- mh_data_log  |>
  ungroup() |>
  dplyr::select(FMP, REGION, CLUSTER, MANAGEMENT_TYPE_USE, VALUE_UNITS, 
                VALUE_TYPE, VALUE_RATE) %>%
  distinct()

# IDENTIFY AND SUBSET TO CLUSTERS THAT HAVE INCONSISTENT VALUE VARIABLES
value_change<-value_setup[duplicated(value_setup$CLUSTER),]  

# SUMMARIZE BY MANAGEMENT_TYPE_USE
value_change_type <- value_change |>
  group_by(MANAGEMENT_TYPE_USE) |>
  summarize(n = n()) |>
  arrange(desc(n))

view(value_change_type)

# SUMMARIZE BY REGION
value_change_region <- value_change |>
  group_by(REGION) |>
  summarize(n = n()) |>
  arrange(desc(n))

value_change_region