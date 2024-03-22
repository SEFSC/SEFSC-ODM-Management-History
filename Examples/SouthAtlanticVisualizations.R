# Play with Management History Data Log RDS file for South Atlantic

library(tidyverse)
library(here)
library(lubridate)
library(ggrepel)
library(openxlsx)

# MH dataset 
mh <- readRDS(here("ODM-MH-Data_log", "data", "results", "MH_DL_2024Feb29.RDS"))

mh <- mh %>%
  mutate(COMMON_NAME_USE = case_when(REGULATION_ID == '1411' ~ "MALACANTHIDAE—TILEFISHES",
                                     TRUE ~ COMMON_NAME_USE))

# Subset for South Atlantic Snapper Grouper
mh_srgr <- mh %>% filter(FMP == "SNAPPER-GROUPER FISHERY OF THE SOUTH ATLANTIC REGION") %>%
  # Remove recs that were never implemented
  filter(NEVER_IMPLEMENTED == 0)

# Summarize management types by sector
plot1 <- ggplot(mh_srgr %>%
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

# Timeline - number of regulations by sector by year
reg_yr <- mh_srgr %>%
  mutate(YEAR = year(EFFECTIVE_DATE)) %>%
  group_by(SECTOR_USE, YEAR, MANAGEMENT_CATEGORY, MANAGEMENT_TYPE_USE) %>%
  summarize(nFRs = n_distinct(FR_CITATION),
            nregs = n()) 

# Plot by management cat and sector
plot2 <- ggplot(reg_yr) +
  geom_bar(aes(y=nFRs, x=YEAR), stat = 'identity') +
  facet_grid(SECTOR_USE~MANAGEMENT_CATEGORY, scales = "free_y") +
  theme_bw()

# FRs published by year and species
# Clean up species into removed and currently managed categories
species_list <- read.csv(here("Examples", "snappergrouper_specieslist.csv"))

species_list <- species_list %>%
  mutate(REMOVED_DT = case_when(REMOVED_DT == '' ~ NA,
                                TRUE ~ REMOVED_DT))

currently_managed <- species_list %>% filter(is.na(REMOVED_DT)) %>% select(COMMON_NAME) %>% pull()

# Prep for join
sp_list_all <- species_list %>%
  select(COMMON_NAME, ADDED_DT, REMOVED_DT) %>%
  mutate(COMMON_NAME_USE = 'ALL',
         ) %>%
  mutate(ADDED_DATE = as.Date(ADDED_DT, origin = "1899-12-30"),
         REMOVED_DATE = as.Date(REMOVED_DT, origin = "1899-12-30"))
  

# Aggregate species list
species_aggs <- read.csv(here("Examples", "snappergrouper_aggregatefinal.csv")) %>%
  mutate(COMMON_NAME_USE = COMMON_NAME,
         SPP_NAME = FMP_SPECIES_AGG_NAME,
         ADDED_DATE = as.Date(ADDED_DT, origin = "1899-12-30"),
         REMOVED_DATE = as.Date(REMOVED_DT, origin = "1899-12-30"),
         # convert FMP agg name to uppercase
         FMP_SPECIES_AGG_NAME = toupper(FMP_SPECIES_AGG_NAME))


# Prep for join
#species_ifq_agg <- species_aggs %>%
 # filter(FMP_SPECIES_AGG_NAME %in% c('IFQ: DEEP-WATER GROUPERS (DWG)',
  #                                   'IFQ: OTHER SHALLOW-WATER GROUPERS (OTHER SWG)',
  #                                   'IFQ: TILEFISHES')) %>%
  #select(FMP_SPECIES_AGG_NAME, COMMON_NAME, ADDED_DATE, REMOVED_DATE)

mh_srgr_cleaned <- mh_srgr %>%
  # Expand common name all species
  full_join(sp_list_all, by = join_by(COMMON_NAME_USE), multiple = 'all') %>%
  mutate(YEAR = year(EFFECTIVE_DATE),
         SPECIES = case_when(COMMON_NAME_USE == 'ALL' ~ COMMON_NAME,
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
  full_join(species_aggs, by = join_by(SPECIES == FMP_SPECIES_AGG_NAME), multiple='all') %>%
  filter(!is.na(FR_CITATION)) %>%
  mutate(SPECIES = case_when(!is.na(COMMON_NAME) ~ COMMON_NAME,
                             TRUE ~ SPECIES),
         flg_dt = case_when(!is.na(ADDED_DATE) & EFFECTIVE_DATE<ADDED_DATE ~ 1,
                           !is.na(REMOVED_DATE) & END_DATE2 > REMOVED_DATE ~ 1,
                            TRUE ~ 0)) %>%
  filter(flg_dt == 0) %>%
  mutate(SPECIES = case_when(SPECIES == 'JEWFISH' ~ 'GROUPER, GOLIATH',
                             TRUE ~ SPECIES),
                             #SPECIES == 'Caulolatilus' ~ 'TILEFISHES',
          #                   
           #                  SPECIES == 'Centropristis melana' ~ 'BASS, BLACK SEA',
            #               TRUE ~ SPECIES),
         managed = case_when(SPECIES %in% currently_managed ~ 'Currently managed', TRUE ~ 'Removed from FMP'))

chk <- mh_srgr_cleaned %>% ungroup() %>% 
  select(SPECIES, managed) %>% distinct()

sp_yr  <- mh_srgr_cleaned %>%  group_by(managed, SPECIES, YEAR) %>%
  summarize(nFRs = n_distinct(FR_CITATION))

plot <- ggplot(sp_yr %>% filter(managed == 'Currently managed')
               , aes(x=YEAR, y=SPECIES, fill=nFRs)) +
  geom_tile() +
  scale_fill_gradientn(colours = c("#44ce1b", "#f7e379", "#e51f1f"), values = c(0,0.5,1)) +
  theme_classic()

# Timeline - number of regulations by sector by year
reg_yr <- mh_srgr %>%
  mutate(YEAR = year(EFFECTIVE_DATE)) %>%
  group_by(SECTOR_USE, YEAR, MANAGEMENT_CATEGORY, MANAGEMENT_TYPE_USE) %>%
  summarize(nFRs = n_distinct(FR_CITATION),
            nregs = n()) 

# Plot by management cat and sector
plot3 <- ggplot(reg_yr) +
  geom_bar(aes(y=nFRs, x=YEAR), stat = 'identity') +
  facet_grid(SECTOR_USE~MANAGEMENT_CATEGORY, scales = "free_y") +
  theme_bw()


# Type of closures - mostly once and seasonal
plot4 <- mh_srgr %>%
  filter(MANAGEMENT_TYPE_USE == 'CLOSURE') %>%
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
source("C:/Users/gaitlyn.malone/Documents/SEFSC-ODM-Management-History/ODM-MH-Analysis_ready/Closures/func_expand_status.R")

# Expand closures
closures <- expand_status(mh_srgr, "CLOSURE")

# Filter to main closures for subsector all, zone all
# Add IFQ
sum_closures <- closures %>%
  mutate(value = 1,
         managed = case_when(COMMON_NAME_USE %in% currently_managed ~ 'Currently managed', TRUE ~ 'Removed from FMP'),
         SECTOR_lab =  paste(SECTOR_USE, SUBSECTOR_USE, sep="-"),
         type = 'Closure') %>%
  filter(VALUE == 'CLOSE', SUBSECTOR_USE == 'ALL', ZONE_USE == 'ALL', managed == 'Currently managed') %>%
  # Add IFQ
  bind_rows(mh_srgr %>% filter(MANAGEMENT_TYPE_USE == "ITQ PROGRAM ESTABLISHED") %>% ungroup() %>%
              select(REGULATION_ID, CLUSTER, FR_CITATION, SECTOR_USE, SUBSECTOR_USE, COMMON_NAME_USE, VALUE, 
                     START_DATE2, END_DATE2) %>%
              mutate(date_sequence = map2(START_DATE2, END_DATE2, seq, by = "days")) %>%
              unnest(date_sequence) %>%
              mutate(value = 2,
                     type = "ITQ",
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

# Get count of FRs by species, consol. mcat, and sector
sum_sp_mtype_sect <- mh_srgr_cleaned %>%
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
other_reg_yr <- mh_srgr %>%
  mutate(YEAR = year(EFFECTIVE_DATE)) %>%
  filter(MANAGEMENT_CATEGORY == 'OTHER') %>%
  group_by(YEAR, MANAGEMENT_TYPE_USE) %>%
  summarize(nFRs = n_distinct(FR_CITATION),
            nregs = n()) %>%
  ggplot() +
  geom_bar(aes(y=nFRs, x=YEAR), stat = 'identity') +
  facet_wrap(~MANAGEMENT_TYPE_USE, ncol=2, scales = "free_y") +
  theme_bw()
