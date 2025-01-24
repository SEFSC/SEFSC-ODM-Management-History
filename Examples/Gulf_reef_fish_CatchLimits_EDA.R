
library(tidyverse)
library(RColorBrewer)

# MH dataset 
mh <- readRDS("C:/Users/sarina.atkinson/Documents/GitHub/SEFSC/SEFSC-ODM-Management-History/ODM-MH-Data_log/data/results/MH_DL_2024Apr30.RDS")

# Catch limits for Gulf Reef
mh2 <- mh %>% filter(MANAGEMENT_CATEGORY == 'CATCH LIMITS',
                    FMP == "REEF FISH RESOURCES OF THE GULF OF MEXICO")

chk <- filter(mh, MANAGEMENT_TYPE == 'DEFINITION', 
              FMP == "REEF FISH RESOURCES OF THE GULF OF MEXICO") %>% ungroup() %>% select(VALUE, FR_CITATION) %>% distinct()

# Summarize management types by sector 
plot1 <- ggplot(mh2 %>%
                  group_by(SECTOR_USE, MANAGEMENT_TYPE_USE) %>%
                  summarise(nrecs = n_distinct(REGULATION_ID)) %>%
                  group_by(SECTOR_USE) %>% 
                  mutate(tot = sum(nrecs), perc = nrecs/tot, labels = paste0(round(perc * 100,1), "%")), aes(x = "" , y = perc, fill = MANAGEMENT_TYPE_USE)) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  #geom_text(aes(x = 1.7, label = labels), size=4.5, 
  #          position = position_stack(vjust = 0.5))+
  facet_wrap(~SECTOR_USE) +
  scale_fill_brewer(palette = "Set3") +
  guides(fill = guide_legend(title = "Management Type")) +
  theme_void()

# ACLs by species
plot2 <- ggplot(mh2 %>%
                  filter(MANAGEMENT_TYPE == 'ACL') %>%
                  group_by(SECTOR_USE, SPP_NAME) %>%
                  summarise(nrecs = n_distinct(REGULATION_ID)) %>%
                  group_by(SECTOR_USE) %>% 
                  mutate(tot = sum(nrecs), perc = nrecs/tot, labels = paste0(round(perc * 100,1), "%")), aes(x = "" , y = perc, fill = SPP_NAME)) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  #geom_text(aes(x = 1.7, label = labels), size=4.5, 
  #          position = position_stack(vjust = 0.5))+
  facet_wrap(~SECTOR_USE) +
  #scale_fill_brewer(palette = "Set3") +
  guides(fill = guide_legend(title = "Species")) +
  theme_void()

unique(mh2$SPP_NAME)
  
# DWG ACLS
acl_dwg <- mh2 %>% ungroup() %>%
  filter(SPP_NAME == 'ACL/ACT:  DEEP-WATER GROUPER (DWG) COMBINED') %>%
  select(REGULATION_ID, MANAGEMENT_TYPE_USE, SECTOR, SECTOR_USE, FR_CITATION, 
         EFFECTIVE_DATE, START_YEAR, START_DATE2, END_DATE2,
         VALUE, VALUE_TYPE, VALUE_UNITS, VALUE_RATE) %>% distinct() %>%
  arrange(START_YEAR, FR_CITATION)

chk <- filter(mh2, MANAGEMENT_TYPE_USE == 'QUOTA', SPP_NAME == 'QUOTA: DEEP-WATER GROUPERS (DWG)')
# SWG ACLS
acl_swg <- mh2 %>% ungroup() %>%
  filter(SPP_NAME == 'ACL/ACT:  OTHER SHALLOW-WATER GROUPER (OTHER SWG)') %>%
  select(REGULATION_ID, MANAGEMENT_TYPE_USE, SECTOR, SECTOR_USE, FR_CITATION, 
         EFFECTIVE_DATE, START_YEAR, START_DATE2, END_DATE2,
         VALUE, VALUE_TYPE, VALUE_UNITS, VALUE_RATE) %>% distinct() %>%
  arrange(START_YEAR, FR_CITATION)

filter(mh2, SPP_NAME == 'ACL/ACT: SHALLOW-WATER GROUPER (SWG) COMBINED') %>% ungroup() %>% 
  select(COMMON_NAME_USE) %>% distinct() %>% pull()

# FRs for ACLS
chk <- mh2 %>%
  filter(MANAGEMENT_TYPE == 'ACL') %>%
  group_by(FR_CITATION, SPP_NAME) %>%
  summarise(nrecs = n_distinct(REGULATION_ID))

filter(mh2, FR_CITATION == '73 FR 38139') %>% ungroup() %>% select(FR_URL) %>% distinct() %>% pull()

# Gray Snapper ACLS
acl_gs <- mh2 %>% ungroup() %>%
  filter(SPP_NAME == 'SNAPPER, GRAY') %>%
  select(REGULATION_ID, MANAGEMENT_TYPE_USE, SECTOR, SECTOR_USE, FR_CITATION, 
         EFFECTIVE_DATE, START_YEAR, START_DATE2, END_DATE2,
         VALUE, VALUE_TYPE, VALUE_UNITS, VALUE_RATE) %>% distinct() %>%
  arrange(START_YEAR, FR_CITATION)

# Tilefish ACLS
acl_tile <- mh2 %>% ungroup() %>%
  filter(SPP_NAME == 'ACL/ACT: TILEFISHES COMBINED') %>%
  select(REGULATION_ID, MANAGEMENT_TYPE_USE, SECTOR, SECTOR_USE, FR_CITATION, 
         EFFECTIVE_DATE, START_YEAR, START_DATE2, END_DATE2,
         VALUE, VALUE_TYPE, VALUE_UNITS, VALUE_RATE) %>% distinct() %>%
  arrange(START_YEAR, FR_CITATION)

# Vermilion Snapper ACLS
acl_vs <- mh2 %>% ungroup() %>%
  filter(SPP_NAME == 'SNAPPER, VERMILION') %>%
  select(REGULATION_ID, MANAGEMENT_TYPE_USE, SECTOR, SECTOR_USE, FR_CITATION, 
         EFFECTIVE_DATE, START_YEAR, START_DATE2, END_DATE2,
         VALUE, VALUE_TYPE, VALUE_UNITS, VALUE_RATE) %>% distinct() %>%
  arrange(START_YEAR, FR_CITATION)

# Mutton Snapper ACLS
acl_ms <- mh2 %>% ungroup() %>%
  filter(SPP_NAME == 'SNAPPER, MUTTON') %>%
  select(REGULATION_ID, MANAGEMENT_TYPE_USE, SECTOR, SECTOR_USE, FR_CITATION, 
         EFFECTIVE_DATE, START_YEAR, START_DATE2, END_DATE2,
         VALUE, VALUE_TYPE, VALUE_UNITS, VALUE_RATE) %>% distinct() %>%
  arrange(START_YEAR, FR_CITATION)

# Yellowtail Snapper ACLS
acl_ys <- mh2 %>% ungroup() %>%
  filter(SPP_NAME == 'SNAPPER, YELLOWTAIL') %>%
  select(REGULATION_ID, MANAGEMENT_TYPE_USE, SECTOR, SECTOR_USE, FR_CITATION, 
         EFFECTIVE_DATE, START_YEAR, START_DATE2, END_DATE2,
         VALUE, VALUE_TYPE, VALUE_UNITS, VALUE_RATE) %>% distinct() %>%
  arrange(START_YEAR, FR_CITATION)

# HOgfish ACLS
acl_hog <- mh2 %>% ungroup() %>%
  filter(SPP_NAME == 'HOGFISH') %>%
  select(REGULATION_ID, MANAGEMENT_TYPE_USE, SECTOR, SECTOR_USE, FR_CITATION, 
         EFFECTIVE_DATE, START_YEAR, START_DATE2, END_DATE2,
         VALUE, VALUE_TYPE, VALUE_UNITS, VALUE_RATE, NEVER_IMPLEMENTED) %>% distinct() %>%
  arrange(START_YEAR, FR_CITATION)

# Catch limits for Gulf Reef
mh3 <- mh %>% filter(MANAGEMENT_CATEGORY == 'CATCH LIMITS',
                     FMP == "COASTAL MIGRATORY PELAGIC RESOURCES")
# Cobia ACLS
acl_cobia <- mh3 %>% ungroup() %>%
  filter(SPP_NAME == 'COBIA', MANAGEMENT_TYPE_USE == 'ACL') %>%
  select(REGULATION_ID, MANAGEMENT_TYPE_USE, SECTOR, SECTOR_USE, FR_CITATION, ZONE_USE,
         EFFECTIVE_DATE, START_YEAR, START_DATE2, END_DATE2,
         VALUE, VALUE_TYPE, VALUE_UNITS, VALUE_RATE) %>% distinct() %>%
  arrange(START_YEAR, FR_CITATION, REGULATION_ID)

# Spanish Mackerel ACLS
acl_sm <- mh3 %>% ungroup() %>%
  filter(SPP_NAME == 'MACKEREL, SPANISH', ZONE_USE == 'GULF MIGRATORY GROUP SPANISH MACKEREL', MANAGEMENT_TYPE_USE == 'ACL') %>%
  select(REGULATION_ID, MANAGEMENT_TYPE_USE, SECTOR, SECTOR_USE, FR_CITATION, ZONE_USE,
         EFFECTIVE_DATE, START_YEAR, START_DATE2, END_DATE2,
         VALUE, VALUE_TYPE, VALUE_UNITS, VALUE_RATE) %>% distinct() %>%
  arrange(START_YEAR, FR_CITATION, REGULATION_ID)


