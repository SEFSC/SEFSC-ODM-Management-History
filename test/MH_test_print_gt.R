# Load packages ####
#install.packages("librarian")
librarian::shelf(here, tidyverse, gt, flextable)

# RUN MH CODE
here::i_am('test/MH_test_print.R')
source(here('code', 'main_MH_prep.R'))

# MULTI REG ONLY WORKS WHEN MONTH DAY AND YEAR START PROVIDED?

# ISSUE WITH DATE SHIFT - MANUAL REG LIST HAS 8/29 and 8/30
# CODE HAS 8/28 and 8/29
# REG ID 792

closures = mh_expanded %>%
  filter(FMP %in% c("REEF FISH RESOURCES OF THE GULF OF MEXICO"),
         SPP_NAME %in% c("SNAPPER, RED"),
         MANAGEMENT_TYPE_USE  == "CLOSURE")
summary(factor(closures$MANAGEMENT_STATUS_USE))
summary(factor(closures$STATUS_TYPE))

print = mh_expanded %>%
  filter(FMP %in% c("REEF FISH RESOURCES OF THE GULF OF MEXICO"),
         SPP_NAME %in% c("SNAPPER, RED"),
         MANAGEMENT_TYPE_USE %in% c("MINIMUM SIZE LIMIT",
                                    "BAG LIMIT",
                                    "CREW BAG LIMIT",
                                    "CLOSURE"),
         NEVER_IMPLEMENTED == 0)  %>%
  mutate(VALUE_RATE = case_when(is.na(VALUE_RATE) ~ "NA",
                                TRUE ~ VALUE_RATE),
         VALUE_TYPE = case_when(is.na(VALUE_TYPE) ~ "NA",
                                TRUE ~ VALUE_TYPE),
         VALUE_UNITS = case_when(is.na(VALUE_UNITS) ~ "NA",
                                TRUE ~ VALUE_UNITS))%>%
  group_by(MANAGEMENT_CATEGORY, MANAGEMENT_TYPE_USE, STATUS_TYPE,
           JURISDICTION, JURISDICTIONAL_WATERS, FMP,
           SECTOR_USE, SUBSECTOR_USE, REGION,
           SPP_NAME) %>%
  select(MANAGEMENT_STATUS_USE, REGULATION_ID, SECTOR_USE, REGION, ZONE_USE, 
         START_DATE2, END_DATE2,
         VALUE, VALUE_UNITS, VALUE_TYPE, VALUE_RATE,
         FR_CITATION, ACTION, ACTION_TYPE, 
         MANAGEMENT_TYPE_USE, STATUS_TYPE, MANAGEMENT_CATEGORY,
         JURISDICTION, JURISDICTIONAL_WATERS, FMP,
         SUBSECTOR_USE,
         SPP_NAME) %>%
  arrange(SPP_NAME, MANAGEMENT_CATEGORY, 
          MANAGEMENT_TYPE_USE, STATUS_TYPE, MANAGEMENT_STATUS_USE,
          JURISDICTION, JURISDICTIONAL_WATERS, FMP,
          SECTOR_USE, SUBSECTOR_USE, REGION,
          START_DATE2)

gt(print)

print_flex <- print %>%
  ungroup() %>%
  mutate(TABLE = paste(MANAGEMENT_CATEGORY, 
                       MANAGEMENT_TYPE_USE, STATUS_TYPE,
                       SECTOR_USE, SUBSECTOR_USE,
                       JURISDICTION, JURISDICTIONAL_WATERS, REGION,
                       FMP, SPP_NAME, 
                       sep =" - "),
         REGULATION_ID = as.character(REGULATION_ID)) %>%
  select(MANAGEMENT_STATUS_USE, REGULATION_ID, ZONE_USE, 
         START_DATE2, END_DATE2,
         VALUE, VALUE_UNITS, VALUE_TYPE, VALUE_RATE,
         FR_CITATION, ACTION, ACTION_TYPE,
         TABLE) %>%
  arrange(TABLE, MANAGEMENT_STATUS_USE, START_DATE2) %>%
  as_grouped_data(groups = c("TABLE"))

as_flextable(print_flex) %>%
  merge_v(c("MANAGEMENT_STATUS_USE", "ZONE_USE",
            "VALUE", "VALUE_UNITS", "VALUE_TYPE", "VALUE_RATE")) %>%
  theme_box()
