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

summary(factor(mh_expanded$MANAGEMENT_STATUS_USE))
summary(factor(mh_expanded$STATUS_TYPE))

print = mh_expanded %>%
  filter(FMP %in% c("REEF FISH RESOURCES OF THE GULF OF MEXICO"),
         SPP_NAME %in% c("SNAPPER, RED"),
         !MANAGEMENT_TYPE_USE %in% c("MINIMUM SIZE LIMIT",
                                    "BAG LIMIT",
                                    "CREW BAG LIMIT",
                                    "CLOSURE"))  %>%
  mutate(VALUE_RATE = case_when(is.na(VALUE_RATE) ~ "NA",
                                TRUE ~ VALUE_RATE),
         VALUE_TYPE = case_when(is.na(VALUE_TYPE) ~ "NA",
                                TRUE ~ VALUE_TYPE),
         VALUE_UNITS = case_when(is.na(VALUE_UNITS) ~ "NA",
                                TRUE ~ VALUE_UNITS))%>%
  group_by(MANAGEMENT_CATEGORY, MANAGEMENT_TYPE_USE, FMP,
           #JURISDICTION, JURISDICTIONAL_WATERS, 
           SECTOR_USE, SUBSECTOR_USE, REGION,
           SPP_NAME, CLUSTER) %>%
  select(MANAGEMENT_STATUS_USE, SECTOR_USE, REGION, ZONE_USE, 
         START_DATE2, END_DATE2,
         VALUE, VALUE_UNITS, VALUE_TYPE, VALUE_RATE,
         FR_CITATION, #ACTION, 
         ACTION_TYPE, 
         FLAG, NEVER_IMPLEMENTED, MULTI_REG,
         REGULATION_ID, CLUSTER,
         MANAGEMENT_TYPE_USE, MANAGEMENT_CATEGORY,FMP,
         #JURISDICTION, JURISDICTIONAL_WATERS, 
         SUBSECTOR_USE,
         SPP_NAME) %>%
  arrange(SPP_NAME, MANAGEMENT_CATEGORY, 
          MANAGEMENT_TYPE_USE, MANAGEMENT_STATUS_USE, FMP,
          #JURISDICTION, JURISDICTIONAL_WATERS,
          SECTOR_USE, SUBSECTOR_USE, REGION,
         START_DATE2) %>%
  rename(STATUS = MANAGEMENT_STATUS_USE,
         ZONE = ZONE_USE,
         START = START_DATE2,
         END = END_DATE2,
         UNITS = VALUE_UNITS,
         TYPE = VALUE_TYPE,
         RATE = VALUE_RATE,
         CITATION = FR_CITATION,
         NI = NEVER_IMPLEMENTED,
         MR = MULTI_REG,
         ID = REGULATION_ID)

gt(print)  %>%
  tab_style(
    style = list(
      cell_fill(color = "grey84")
    ),
    locations = cells_body(
      rows = NI == 1
    )
  ) %>%
  tab_style(
    style = list(
      cell_text(style = "italic")
    ),
    locations = cells_body(
      rows = FLAG == "YES"
    )
  ) %>%
  tab_style(
    style = gt::cell_text(weight = "bold"),
    locations = list(cells_row_groups(), 
                     cells_column_labels())
  )  %>%
  cols_width(
    ZONE ~ px(200)
  )

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



complex = mh_expanded %>%
  filter(STATUS_TYPE  == "COMPLEX")
summary(factor(complex$MANAGEMENT_STATUS_USE))
summary(factor(complex$STATUS_TYPE))
unique(complex$CLUSTER)
