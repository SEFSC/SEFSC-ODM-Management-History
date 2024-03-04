# Play with Management History Data Log RDS file for Gulf Reef Fish

library(tidyverse)
library(lubridate)
library(ggrepel)
library(openxlsx)

# MH dataset 
mh <- readRDS("C:/Users/sarina.atkinson/Documents/GitHub/SEFSC/SEFSC-ODM-Management-History/ODM-MH-Data_log/data/results/MH_DL_2024Feb29.RDS")

# Subset for Gulf Reef Fish
mh_grf <- mh %>% filter(FMP == "REEF FISH RESOURCES OF THE GULF OF MEXICO") %>%
  # Remove recs that were never implemented
  filter(NEVER_IMPLEMENTED == 0)

# Summarize management types by sector - majority records closures
plot1 <- ggplot(mh_grf %>%
         group_by(SECTOR_USE, MANAGEMENT_CATEGORY) %>%
         summarise(nrecs = n_distinct(REGULATION_ID)) %>%
         group_by(SECTOR_USE) %>% 
         mutate(tot = sum(nrecs), perc = nrecs/tot, labels = paste0(round(perc * 100,1), "%")), aes(x = "" , y = perc, fill = MANAGEMENT_CATEGORY)) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(x = 1.7, label = labels), size=4.5, 
            position = position_stack(vjust = 0.5))+
  facet_wrap(~SECTOR_USE) +
  guides(fill = guide_legend(title = "Management Category")) +
  theme_void()

# Regulations by ZONE
# Group all specific zones
plot2 <- mh_grf %>%
  # Create my own zone class
  mutate(ZONE_CLASS2 = case_when(ZONE_USE %in% c('OFF ALABAMA', 'OFF MISSISSIPPI', 'OFF TEXAS', 'OFF LOUISIANA', 'OFF FLORIDA') ~ 'GENERAL',
                                 ZONE_USE %in% c('SMZ - ALABAMA', 'LOUISIANA PARISHES – CAMERON AND VERMILION', 'LOUISIANA PARISHES - LAFOURCHE ST. BERNARD PLAQUEMINES AND JEFFERSON', 'REEF FISH STRESSED AREA') ~ 'SPECIFIC',
                                 TRUE ~ ZONE_CLASS),
         ZONE_plot = case_when(ZONE_CLASS2 == 'SPECIFIC' ~ 'SMALLER ZONE (I.E. SMZ, HAPC, DWH, ETC.)',
                               ZONE_USE == "WEST OF 85°30' WEST LONGITUDE" ~ "BOTTOM LONGLINE REEF FISH FISHERY MANAGEMENT AREA EAST OF 85°30' WEST",
                               TRUE ~ ZONE_USE)) %>%
  group_by(SECTOR_USE, ZONE_plot) %>%
  summarise(nrecs = n_distinct(REGULATION_ID)) %>%
  ggplot(aes(x=1, y=nrecs, fill = ZONE_plot)) +
  geom_bar(stat="identity", width = .1, position = "fill") +
  coord_polar("y", start = 0) +
  facet_wrap(~SECTOR_USE) +
  theme_void() +
  labs(fill = 'Zone Name') 


# FRs published by year and species
# Clean up species into removed and currently managed categories
species_list <- read.xlsx('D:/Data Analysis Assessment Support/Sarina Atkinson/Management History/gulf_reef_species_list.xlsx') %>% 
  mutate(ADDED_DATE = as.Date(ADDED_DT, origin = "1899-12-30"),
         REMOVED_DATE = as.Date(REMOVED_DT, origin = "1899-12-30"))

currently_managed <- species_list %>% filter(is.na(REMOVED_DT)) %>% select(COMMON_NAME) %>% pull()

# Prep for join
sp_list_all <- species_list %>%
  select(COMMON_NAME, ADDED_DATE, REMOVED_DATE) %>%
  mutate(COMMON_NAME_USE = 'ALL')

# Aggregate species list
species_aggs <- read.xlsx('D:/Data Analysis Assessment Support/Sarina Atkinson/Management History/gulf_reeffish_aggregates.xlsx') %>%
  mutate(ADDED_DATE = as.Date(ADDED_DT, origin = "1899-12-30"),
         REMOVED_DATE = as.Date(REMOVED_DT, origin = "1899-12-30"),
         # convert FMP agg name to uppercase
         FMP_SPECIES_AGG_NAME = toupper(FMP_SPECIES_AGG_NAME))

# Prep for join
species_ifq_agg <- species_aggs %>%
  filter(FMP_SPECIES_AGG_NAME %in% c('IFQ: DEEP-WATER GROUPERS (DWG)',
                                     'IFQ: OTHER SHALLOW-WATER GROUPERS (OTHER SWG)',
                                     'IFQ: TILEFISHES')) %>%
  select(FMP_SPECIES_AGG_NAME, COMMON_NAME, ADDED_DATE, REMOVED_DATE)

mh_grf_cleaned <- mh_grf %>%
  # Expand common name all species
  full_join(sp_list_all, by = join_by(COMMON_NAME_USE), multiple = 'all') %>%
  mutate(YEAR = year(EFFECTIVE_DATE),
         SPECIES = case_when(COMMON_NAME_USE == 'ALL' ~ COMMON_NAME,
                             COMMON_NAME_USE == 'JEWFISH' ~ 'GROUPER, GOLIATH',
                             TRUE ~ COMMON_NAME_USE),
         flg_dt = case_when(COMMON_NAME_USE == 'ALL' & EFFECTIVE_DATE<ADDED_DATE ~ 1,
                            TRUE ~ 0)) %>%
  # Remove regs expanded for species before they were in the FMP
  filter(flg_dt == 0) %>%
  select(-c(ADDED_DATE, REMOVED_DATE, flg_dt)) %>%
  # Fix effective dates
  left_join(sp_list_all %>% select(-COMMON_NAME_USE), by=join_by(SPECIES == COMMON_NAME)) %>%
  mutate(flg_dt = case_when(EFFECTIVE_DATE<ADDED_DATE ~ 1,
                            EFFECTIVE_DATE>REMOVED_DATE ~ 1,
                            END_DATE2 > REMOVED_DATE ~ 1,
                            TRUE ~ 0)) %>%
  filter(flg_dt == 0) %>%
  select(-c(COMMON_NAME, ADDED_DATE, REMOVED_DATE, flg_dt)) %>%
  # Expand IFQ species aggs
  full_join(species_ifq_agg, by = join_by(SPECIES == FMP_SPECIES_AGG_NAME), multiple='all') %>%
  filter(!is.na(FR_CITATION)) %>%
  mutate(SPECIES = case_when(!is.na(COMMON_NAME) ~ COMMON_NAME,
                             TRUE ~ SPECIES),
         flg_dt = case_when(!is.na(ADDED_DATE) & EFFECTIVE_DATE<ADDED_DATE ~ 1,
                            !is.na(REMOVED_DATE) & END_DATE2 > REMOVED_DATE ~ 1,
                            TRUE ~ 0)) %>%
  filter(flg_dt == 0) %>%
  mutate(SPECIES = case_when(SPECIES == 'Caulolatilus' ~ 'TILEFISHES',
                             SPECIES == 'JEWFISH' ~ 'GROUPER, GOLIATH',
                             SPECIES == 'Centropristis melana' ~ 'BASS, BLACK SEA',
                             TRUE ~ SPECIES),
         managed = case_when(SPECIES %in% currently_managed ~ 'Currently managed', TRUE ~ 'Removed from FMP'))

chk <- mh_grf_cleaned %>% ungroup() %>% 
  select(SPECIES, managed) %>% distinct()

sp_yr  <- mh_grf_cleaned %>%  group_by(managed, SPECIES, YEAR) %>%
  summarize(nFRs = n_distinct(FR_CITATION)) 

plot <- ggplot(sp_yr# %>% filter(managed == 'Currently managed')
               , aes(x=YEAR, y=SPECIES, fill=nFRs)) +
  geom_tile() +
  scale_fill_gradientn(colours = c("#44ce1b", "#f7e379", "#e51f1f"), values = c(0,0.5,1)) +
  theme_classic()


# Timeline - number of regulations by sector by year
reg_yr <- mh_grf %>%
  mutate(YEAR = year(EFFECTIVE_DATE)) %>%
  group_by(SECTOR_USE, YEAR, MANAGEMENT_CATEGORY, MANAGEMENT_TYPE_USE) %>%
  summarize(nFRs = n_distinct(FR_CITATION),
            nregs = n()) 

# Plot by management cat and sector
plot3 <- ggplot(reg_yr) +
  geom_bar(aes(y=nFRs, x=YEAR), stat = 'identity') +
  facet_grid(SECTOR_USE~MANAGEMENT_CATEGORY, scales = "free_y") +
  theme_bw()

# Look at timeline for Red Snapper
plot3a <- mh_grf %>%
  filter(COMMON_NAME_USE == 'SNAPPER, RED') %>%
  mutate(YEAR = year(EFFECTIVE_DATE)) %>%
  group_by(SECTOR_USE, YEAR, MANAGEMENT_CATEGORY, MANAGEMENT_TYPE_USE) %>%
  summarize(nFRs = n_distinct(FR_CITATION),
            nregs = n()) %>%
  ggplot() +
  geom_bar(aes(y=nFRs, x=YEAR), stat = 'identity') +
  facet_grid(SECTOR_USE~MANAGEMENT_CATEGORY, scales = "free_y") +
  theme_bw()

# Red Grouper
plot3b <- mh_grf %>%
  filter(COMMON_NAME_USE == 'GROUPER, RED') %>%
  mutate(YEAR = year(EFFECTIVE_DATE)) %>%
  group_by(SECTOR_USE, YEAR, MANAGEMENT_CATEGORY, MANAGEMENT_TYPE_USE) %>%
  summarize(nFRs = n_distinct(FR_CITATION),
            nregs = n()) %>%
  ggplot() +
  geom_bar(aes(y=nFRs, x=YEAR), stat = 'identity') +
  facet_grid(SECTOR_USE~MANAGEMENT_CATEGORY, scales = "free_y") +
  theme_bw()

# Greater Amberjack
plot3c <- mh_grf %>%
  filter(COMMON_NAME_USE == 'AMBERJACK, GREATER') %>%
  mutate(YEAR = year(EFFECTIVE_DATE)) %>%
  group_by(SECTOR_USE, YEAR, MANAGEMENT_CATEGORY, MANAGEMENT_TYPE_USE) %>%
  summarize(nFRs = n_distinct(FR_CITATION),
            nregs = n()) %>%
  ggplot() +
  geom_bar(aes(y=nFRs, x=YEAR), stat = 'identity') +
  facet_grid(SECTOR_USE~MANAGEMENT_CATEGORY, scales = "free_y") +
  theme_bw()

# Get count of FRs by species, consol. mcat, and sector
sum_sp_mtype_sect <- mh_grf_cleaned %>%
  filter(managed == "Currently managed") %>% ungroup() %>%
  # Consolidate management category into 4 groups
  mutate(MCAT_GRP = case_when(MANAGEMENT_CATEGORY %in% c('EFFORT LIMITS', 'GEAR REQUIREMENTS', 'HARVEST LIMITATIONS',
                                                        'UNIVERSAL', 'SELECTIVITY CONTROLS') ~ 'EFFORT/GEAR LIMITS',
                              TRUE ~ MANAGEMENT_CATEGORY)) %>%
  filter(MANAGEMENT_CATEGORY != 'OTHER') %>%
  group_by(SPECIES, managed, MANAGEMENT_CATEGORY) %>%
  summarize(nFRs = n_distinct(FR_CITATION),
            nregs = n_distinct(REGULATION_ID)) 

plot_sms <- ggplot(sum_sp_mtype_sect) +
  geom_bar(aes(x=nregs, y = SPECIES, fill=MANAGEMENT_CATEGORY), stat="identity") +
  labs(fill="Management Category", x = "Number of Regulations")+
  theme_classic()

# Look into 'other' category
other_reg_yr <- mh_grf %>%
  mutate(YEAR = year(EFFECTIVE_DATE)) %>%
  filter(MANAGEMENT_CATEGORY == 'OTHER') %>%
  group_by(YEAR, MANAGEMENT_TYPE_USE) %>%
  summarize(nFRs = n_distinct(FR_CITATION),
            nregs = n()) %>%
  ggplot() +
  geom_bar(aes(y=nFRs, x=YEAR), stat = 'identity') +
  facet_wrap(~MANAGEMENT_TYPE_USE, ncol=2, scales = "free_y") +
  theme_bw()

# Type of closures - mostly once and seasonal
plot4 <- mh_grf %>%
  filter(ZONE_USE != 'AREA CLOSURE RELATED TO DEEPWATER HORIZON OIL SPILL', 
         MANAGEMENT_TYPE_USE == 'CLOSURE') %>%
  group_by(SECTOR_USE, MANAGEMENT_STATUS_USE) %>%
  summarise(nrecs = n()) %>%
  group_by(SECTOR_USE) %>%
  mutate(totregs = sum(nrecs),
         perc = nrecs/totregs) %>%
  ggplot(aes(x=1, y=nrecs, fill = MANAGEMENT_STATUS_USE)) +
  geom_bar(stat="identity", width = .1, position = "fill") +
  coord_polar("y", start = 0) +
  facet_wrap(~SECTOR_USE) +
  theme_void() +
  labs(fill = 'Type of Closure')

# Heat map of commercial closures overtime by species when closure for all of Gulf
# Read in management status function
source("C:/Users/sarina.atkinson/Documents/GitHub/SEFSC/SEFSC-ODM-Management-History/ODM-MH-Analysis_ready/Closures/func_expand_status.R")

# Expand closures
closures <- expand_status(mh_grf, "CLOSURE")

# Filter to main closures for subsector all, zone all
# Add IFQ
sum_closures <- closures %>%
  mutate(value = 1,
         managed = case_when(COMMON_NAME_USE %in% currently_managed ~ 'Currently managed', TRUE ~ 'Removed from FMP'),
         COMMON_NAME_USE = case_when(COMMON_NAME_USE == 'Caulolatilus' ~ 'TILEFISHES', TRUE ~ COMMON_NAME_USE),
         SUBSECTOR_USE = case_when(COMMON_NAME_USE == 'SNAPPER, RED' & SUBSECTOR_USE %in% c('PRIVATE', 'FOR-HIRE') ~ 'ALL',
                                TRUE ~ SUBSECTOR_USE),
         SECTOR_lab =  paste(SECTOR_USE, SUBSECTOR_USE, sep="-"),
         type = 'Closure') %>%
  filter(VALUE == 'CLOSE', SUBSECTOR_USE == 'ALL', ZONE_USE == 'ALL', managed == 'Currently managed') %>%
  # Add IFQ
  bind_rows(mh_grf %>% filter(MANAGEMENT_TYPE_USE == "IFQ PROGRAM ESTABLISHED") %>% ungroup() %>%
              select(REGULATION_ID, CLUSTER, FR_CITATION, SECTOR_USE, SUBSECTOR_USE, COMMON_NAME_USE, VALUE, 
                     START_DATE2, END_DATE2) %>%
              mutate(END_DATE2 = case_when(COMMON_NAME_USE == 'SNAPPER, RED' ~ max(mh_grf$END_DATE2),
                                           TRUE ~ END_DATE2),
                     date_sequence = map2(START_DATE2, END_DATE2, seq, by = "days")) %>%
              unnest(date_sequence) %>%
              mutate(value = 2,
                     type = "IFQ",
                     SECTOR_lab =  paste(SECTOR_USE, SUBSECTOR_USE, sep="-")))

# Plot using expand function
plot5 <- ggplot(sum_closures, aes(x=date_sequence, y = COMMON_NAME_USE, fill=value)) +
  geom_tile() +
  facet_wrap(~SECTOR_lab, ncol = 1) +
  scale_x_date(date_breaks = "1 year",date_labels = "%Y", 
               limits = c(min(sum_closures$date_sequence), max = max(sum_closures$date_sequence)), expand=c(0,0)) +
  #geom_vline(xintercept = as.Date("2010-01-01"), linetype = 'dashed', color = 'red') +
  #geom_vline(xintercept = as.Date("2007-01-01"), linetype = 'dashed', color = 'red') +
  labs(y = "Species", x = "Date") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

######################################
# Original code for expanding
close2 <- mh_grf %>%
  filter(MANAGEMENT_CATEGORY == 'TEMPORAL CONTROLS',
         ZONE_USE == 'ALL') %>%
  mutate(COMMON_NAME_USE = case_when(COMMON_NAME_USE == 'Caulolatilus' ~ 'TILEFISHES', TRUE ~ COMMON_NAME_USE)) %>%
  # Just do simple closures for now
  filter(STATUS_TYPE == 'SIMPLE') %>%
  # Remove record 761 for now because of error in dates
  filter(REGULATION_ID != 761) %>%
  arrange(COMMON_NAME_USE, START_DATE2) %>% ungroup() %>%
  select(REGULATION_ID, CLUSTER, SECTOR_USE, SUBSECTOR_USE, COMMON_NAME_USE, VALUE, START_DATE2, END_DATE2) %>%
  # Remove reopening because already used to adjust dates
  filter(VALUE == 'CLOSE') %>% ungroup() %>%
  # Create sequence between 2 dates
  mutate(date_sequence = map2(START_DATE2, END_DATE2, seq, by = "day")) %>%
  unnest(cols = c(date_sequence)) %>%
  mutate(value = 1,
         SECTOR_lab =  paste(SECTOR_USE, SUBSECTOR_USE, sep="-"))

plot5_one <- ggplot(close2, aes(x=date_sequence, y = COMMON_NAME_USE, fill=value)) +
  geom_tile() +
  facet_wrap(~SECTOR_lab, ncol = 1) +
  scale_x_date(date_breaks = "1 year",date_labels = "%Y", 
               limits = c(min(close2$date_sequence), max = max(close2$date_sequence)), expand=c(0,0)) +
  #geom_vline(xintercept = as.Date("2010-01-01"), linetype = 'dashed', color = 'red') +
  #geom_vline(xintercept = as.Date("2007-01-01"), linetype = 'dashed', color = 'red') +
  labs(y = "Species", x = "Date") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Seasonal closures
close3 <- mh_grf %>%
  filter(MANAGEMENT_CATEGORY == 'TEMPORAL CONTROLS',
         ZONE_USE == 'ALL', REG_REMOVED == 0) %>%
  mutate(COMMON_NAME_USE = case_when(COMMON_NAME_USE == 'Caulolatilus' ~ 'TILEFISHES', TRUE ~ COMMON_NAME_USE)) %>%
  filter(MANAGEMENT_STATUS_USE == 'SEASONAL') %>%
  # Remove single record where start date > end date
  filter(REGULATION_ID != 744) %>%
  mutate(date_sequence = map2(EFFECTIVE_DATE, END_DATE2, seq, by = "days")) %>%
  unnest(date_sequence) %>%
  rowwise() %>%
  mutate(START_YEAR_expand = year(date_sequence),
         END_YEAR_expand = year(date_sequence),
         START_DATE_EXPAND = as.Date(paste(START_YEAR_expand, START_MONTH, START_DAY, sep = "-")),
         END_DATE_EXPAND = as.Date(paste(END_YEAR_expand, END_MONTH, END_DAY, sep = "-"))) %>%
  filter(date_sequence >= START_DATE_EXPAND,
         END_DATE_EXPAND >= date_sequence) %>% ungroup() %>%
  select(REGULATION_ID, CLUSTER, SECTOR_USE, SUBSECTOR_USE, COMMON_NAME_USE, VALUE, EFFECTIVE_DATE, INEFFECTIVE_DATE,
         START_DATE2, END_DATE2, START_MONTH, START_DAY_USE, START_YEAR, START_YEAR_expand,
         END_MONTH_USE, END_DAY_USE, END_YEAR_USE, END_YEAR_expand,
         START_DATE_EXPAND, END_DATE_EXPAND, date_sequence)
  
# Combine one time closures and seasonal closures for plot
plot5b <- close3 %>%
  select(REGULATION_ID, CLUSTER, SECTOR_USE, SUBSECTOR_USE, COMMON_NAME_USE, VALUE, 
         START_DATE_EXPAND, END_DATE_EXPAND, date_sequence) %>%
  mutate(value = 1,
         SECTOR_lab =  paste(SECTOR_USE, SUBSECTOR_USE, sep="-")) %>%
  rename(START_DATE2 = "START_DATE_EXPAND",
         END_DATE2 = "END_DATE_EXPAND") %>%
  bind_rows(close2) %>%
  mutate(type = 'Closure') %>%
  # Add IFQ
  bind_rows(mh_grf %>% filter(MANAGEMENT_TYPE_USE == "IFQ PROGRAM ESTABLISHED") %>% ungroup() %>%
              select(REGULATION_ID, CLUSTER, SECTOR_USE, SUBSECTOR_USE, COMMON_NAME_USE, VALUE, 
                     START_DATE2, END_DATE2) %>%
              mutate(END_DATE2 = max(mh_grf$END_DATE2),
                     date_sequence = map2(START_DATE2, END_DATE2, seq, by = "days")) %>%
              unnest(date_sequence) %>%
              mutate(value = 2,
                     type = "IFQ",
                     SECTOR_lab =  paste(SECTOR_USE, SUBSECTOR_USE, sep="-"))) %>%
  ggplot(., aes(x=date_sequence, y = COMMON_NAME_USE, fill=value)) +
  geom_tile() +
  facet_wrap(~SECTOR_USE, ncol = 1) +
  scale_x_date(date_breaks = "1 year",date_labels = "%Y", 
               limits = c(min(close2$date_sequence), max = max(close2$date_sequence)), expand=c(0,0)) +
  labs(y = "Species", x = "Date", fill = "") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 90))

# non season recurring closures - not capturing everything for red snapper in plot above
close4 <- mh_grf %>%
  filter(MANAGEMENT_CATEGORY == 'TEMPORAL CONTROLS',
         ZONE_USE == 'ALL', REG_REMOVED == 0) %>%
  mutate(COMMON_NAME_USE = case_when(COMMON_NAME_USE == 'Caulolatilus' ~ 'TILEFISHES', TRUE ~ COMMON_NAME_USE)) %>%
  filter(!MANAGEMENT_STATUS_USE %in% c('SEASONAL', 'ONCE'))
############################
