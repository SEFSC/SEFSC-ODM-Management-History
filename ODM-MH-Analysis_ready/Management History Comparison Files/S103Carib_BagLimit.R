#############################
#  Recreational Bag Limits  #
#############################

librarian::shelf(here, tidyverse, lubridate, openxlsx, neatRanges, splitstackshape, dplyr)

# Set output directory for plots
Dirout<-paste0(getwd(),"/Bag and crew limits/")
if(!file.exists(Dirout)) dir.create(Dirout)

# MH dataset 
mh <- readRDS(here("ODM-MH-Data_log", "data", "results", "MH_DL_2025Nov18.RDS"))

# Set end year for plots
termyr<-2025

# Subset for Gulf Reef Fish recreational bag limits
mh_carib_pr_bag <- mh %>% filter(FMP %in% c("REEF FISH FISHERY OF PUERTO RICO AND THE U.S. VIRGIN ISLANDS", 
                                             #"FMP FOR THE EEZ AROUND ST. THOMAS AND ST. JOHN",
                                             #"FMP FOR THE EEZ AROUND ST. CROIX",
                                             "FMP FOR THE EEZ AROUND PUERTO RICO",
                                             "SPINY LOBSTER FISHERY OF PUERTO RICO AND THE U.S. VIRGIN ISLANDS",
                                             "QUEEN CONCH RESOURCES OF PUERTO RICO AND THE U.S. VIRGIN ISLANDS")) %>%
  # Remove recs that were never implemented
  filter(NEVER_IMPLEMENTED == 0, SECTOR_USE == 'RECREATIONAL',
         MANAGEMENT_TYPE_USE == 'BAG LIMIT',
         ZONE_USE %in% c("ALL", "PUERTO RICO MANAGEMENT AREA"))
mh_carib_pr_bag <- mh_carib_pr_bag %>% 
  mutate(END_DATE2 = case_when(FR_CITATION == "61 FR 32151" & COMMON_NAME_USE == "CONCH, QUEEN" ~ as.Date("2021-10-13"),
                               FR_CITATION == "76 FR 82413" & SPP_NAME == "BAG LIMIT: OTHER REEF FISH SPECIES COMBINED" ~ as.Date("2022-10-13"),
                               FR_CITATION == "76 FR 82403" & SPP_NAME == "BAG LIMIT: GROUPERS, SNAPPERS, AND PARROTFISHES COMBINED" ~ as.Date("2022-10-13"),
                               FR_CITATION == "76 FR 82403" & SPP_NAME == "BAG LIMIT: PARROTFISHES" & VALUE == "2" ~ as.Date("2021-10-13"),
                               FR_CITATION == "76 FR 82403" & SPP_NAME == "BAG LIMIT: PARROTFISHES" & VALUE == "6" ~ as.Date("2021-10-13"),
                               FR_CITATION == "76 FR 82403" & SPP_NAME == "BAG LIMIT: PARROTFISHES" & VALUE == "5" ~ as.Date("2022-10-13"),
                               FR_CITATION == "76 FR 82403" & SPP_NAME == "BAG LIMIT: PARROTFISHES" & VALUE == "15" ~ as.Date("2022-10-13"),
                               TRUE ~ END_DATE2)) %>%
  filter(!(FR_CITATION == "87 FR 56204" & COMMON_NAME_USE == "LOBSTER, CARIBBEAN SPINY"))%>%
  filter(!(FR_CITATION == "87 FR 56204" & COMMON_NAME_USE == "SURGEONFISH"))
  

# Confirm bag limits
unique(mh_carib_pr_bag$MANAGEMENT_TYPE_USE)

# Confirm all recreational
table(mh_carib_pr_bag$SECTOR_USE)

source(here("ODM-MH-Analysis_ready", "Closures", "func_expand_status.R"))

##############
# Bag limits #
##############


bag_df_pr <- expand_status(mh_carib_pr_bag, "BAG LIMIT")

# Collapse to annual value
bag_yr_df_pr <- bag_df_pr %>%
  dplyr::mutate(YEAR = year(date_sequence)) %>%
  dplyr::select(COMMON_NAME_USE, ZONE_USE, SUBSECTOR_USE, YEAR,
         VALUE, VALUE_UNITS, VALUE_TYPE, VALUE_RATE) %>%
  distinct() 

# Explore Zone 
table(bag_yr_df_pr$ZONE_USE)
# All (hogfish zone removed when excluding FR)

# Explore Subsector
table(bag_yr_df_pr$SUBSECTOR_USE)
# All for all data

# Explore year - only include complete years
table(bag_yr_df_pr$YEAR)
bag_yr_df_pr<-bag_yr_df_pr %>% filter(YEAR <=termyr)
table(bag_yr_df_pr$YEAR)

parrotfish_species <- c(
  "PARROTFISH, PRINCESS",
  "PARROTFISH, QUEEN",
  "PARROTFISH, REDBAND",
  "PARROTFISH, REDTAIL",
  "PARROTFISH, STOPLIGHT",
  "PARROTFISH, STRIPED"
)

years_add <- 2022:2025
year_cast <- if(is.numeric(bag_yr_df_pr$YEAR)) as.numeric else as.character()

# Use the Parrotfish 2 rows as a "template" for ZONE_USE / SUBSECTOR_USE.
# This is what typically creates "two rows" per species-year (two distinct combos).
template <- bag_yr_df_pr %>%
  dplyr::filter(COMMON_NAME_USE == "PARROTFISH 2", YEAR %in% years_add) %>%
  dplyr::distinct(ZONE_USE, SUBSECTOR_USE)

# If template isn't exactly 2 rows, force it to be 2 (duplicate first row if needed)
template2 <- dplyr::bind_rows(template, template) %>%
  dplyr::slice(1:2)

rate_specs <- tibble::tibble(
    VALUE = c("2", "6"),
    VALUE_UNITS = c("NUMBERS", "NUMBERS"),
    VALUE_TYPE  = c("COUNT", "COUNT"),
    VALUE_RATE  = c("PER PERSON PER DAY", "PER VESSEL PER DAY")
  )

add_rows <- tidyr::crossing(
  COMMON_NAME_USE = parrotfish_species,
  YEAR = year_cast(years_add),
  template2,
  rate_specs
)

# Optionally REMOVE the original Parrotfish 2 records for those years
# (recommended if you're replacing them with the six species)
bag_yr_df_pr_aug <- bag_yr_df_pr %>%
  dplyr::filter(!(COMMON_NAME_USE == "PARROTFISH 2" & YEAR %in% year_cast(years_add))) %>%
  dplyr::bind_rows(add_rows)


bag_yr_df2_pr <- bag_yr_df_pr_aug %>%
  dplyr::mutate(
    VALUE_UNITS = dplyr::case_when(
      VALUE_UNITS == "NUMBERS" ~ "fish",
      VALUE_UNITS == "CONCH" ~ "conch",
      VALUE_UNITS == "LOBSTER" ~ "lobster",
      TRUE ~ VALUE_UNITS
    ),
    COMMON_NAME_USE = dplyr::case_when(
      COMMON_NAME_USE == "Calamus pennatula" ~ "PORGY, PLUMA",
      TRUE ~ COMMON_NAME_USE
    ),
    VAL_LAB = paste(VALUE, VALUE_UNITS, stringr::str_to_lower(VALUE_RATE)),
    LAB = paste(stringr::str_to_title(sub("(.*)\\,\\s+(.*)", "\\2 \\1", COMMON_NAME_USE)), "-", VAL_LAB),
    n = 1
  ) %>%
  dplyr::select(LAB, YEAR, n) %>%
  dplyr::arrange(YEAR) %>%
  tidyr::pivot_wider(names_from = YEAR, values_from = n) %>%
  dplyr::arrange(LAB)


# Format for plot - Don't use
bag_yr_df2_pr <- bag_yr_df_pr %>%
  dplyr::mutate(
    VALUE_UNITS = case_when(VALUE_UNITS == 'NUMBERS' ~ 'conch',
                                        VALUE_UNITS == "CONCH" ~ 'conch',
                                 TRUE ~ VALUE_UNITS),
                COMMON_NAME_USE = case_when(COMMON_NAME_USE == "Calamus pennatula" ~ "PORGY, PLUMA",
                                            TRUE ~ COMMON_NAME_USE),
         VAL_LAB = paste(VALUE, VALUE_UNITS, str_to_lower(VALUE_RATE)),
         LAB = paste(str_to_title(sub('(.*)\\,\\s+(.*)','\\2 \\1', COMMON_NAME_USE)),"-",  VAL_LAB),
         #         LAB = paste(COMMON_NAME_USE,"-",  VAL_LAB),
         #         LAB = paste(COMMON_NAME_USE, "(Zone ", ZONE_USE, ")", VAL_LAB),
         n = 1) %>%
  dplyr::select(LAB, YEAR, n) %>%
  arrange(YEAR) %>%
  pivot_wider(names_from = YEAR, values_from = n) %>%
  arrange(LAB) 

# Code below no longer needed if remove 82 FR 34574 (single bag limit for hogfish)

# Checking on two FRs in 1 year that both set a bag limit of 5 fish: 64 FR 57403 82 FR 34574
#chk <- filter(bag_df, COMMON_NAME_USE == 'HOGFISH', year(date_sequence) == 2017)
#table(chk$FR_CITATION)

# Need to replace c(1,1) with 1
#bag_yr_df2[bag_yr_df2$LAB=="HOGFISH - 5 fish Per Person Per Day",29]<-bag_yr_df2[bag_yr_df2$LAB=="HOGFISH - 5 fish Per Person Per Day",28]
#bag_yr_df2<-as.data.frame(bag_yr_df2)

# Subset aggregate groups and then species limits; reorder by family/species (manually for now...)
bag_yr_df2_pr<-bag_yr_df2_pr[c(2, 1, 4, 3, 6, 5, 8, 7, 10, 9, 12, 11, 14, 13, 16, 15,
                               18, 17, 20, 19, 22, 21, 24, 23, 26, 25, 27, 29, 30, 28,
                               32, 31, 34, 33, 36, 35, 38, 37, 39, 41, 42, 40, 44, 43,
                               46, 45, 48, 47, 50, 49, 54, 53, 56, 55, 58, 57, 60, 59,
                               62, 61, 64, 63, 66, 65, 68, 67, 70, 69, 72, 71, 74, 73,
                               ),]

# Need to redefine aggregate limits
#bag_yr_df3$LAB[bag_yr_df3$LAB=="Golden Tilefish - 20 Fish Per Person Per Day"]<-"Gulf Reef Fish - 20 Fish Per Person Per Day"
#bag_yr_df3$LAB[bag_yr_df3$LAB=="Banded Rudderfish - 5 Fish Per Person Per Day"]<-"Banded Rudderfish and Lesser Amberjack - 5 Fish Per Person Per Day"
#bag_yr_df3$LAB[bag_yr_df3$LAB=="Queen Snapper - 10 Fish Per Person Per Day"]<-"Snapper - 10 Fish Per Person Per Day"
#bag_yr_df3$LAB[bag_yr_df3$LAB=="Snowy Grouper - 3 Fish Per Person Per Day"]<-"Grouper - 3 Fish Per Person Per Day"
#bag_yr_df3$LAB[bag_yr_df3$LAB=="Snowy Grouper - 4 Fish Per Person Per Day"]<-"Grouper - 4 Fish Per Person Per Day"
#bag_yr_df3$LAB[bag_yr_df3$LAB=="Snowy Grouper - 5 Fish Per Person Per Day"]<-"Grouper - 5 Fish Per Person Per Day"

# Need to extend hogfish since we had to remove FR
#bag_yr_df3[bag_yr_df3$LAB=="Hogfish - 5 Fish Per Person Per Day",(which(colnames(bag_yr_df3)=="2018")):dim(bag_yr_df3)[2]]<-1 #2018+

# Save outputs before we clean up labels
#saveRDS(bag_yr_df3,paste0(Dirout,"recreational_bag_limits.rds"))

# Remove species name in consecutive rows
bag_yr_df2_pr$LAB[which(bag_yr_df2_pr$LAB=="Almaco Jack - 15 fish per person per day")]<-"- 15 fish per person per day"
bag_yr_df2_pr$LAB[which(bag_yr_df2_pr$LAB=="And Snappers Combined Bag Limit: Groupers, Parrotfishes - 5 fish per vessel per day")]<-"Groupers, Parrotfishes, and Snappers combined - 5 fish per vessel per day"
bag_yr_df2_pr$LAB[which(bag_yr_df2_pr$LAB=="And Snappers Combined Bag Limit: Groupers, Parrotfishes - 15 fish per person per day")]<-"- 15 fish per person per day"
bag_yr_df2_pr$LAB[which(bag_yr_df2_pr$LAB=="And Wrasses Combined Bag Limit: Angelfishes, Grunts, Jacks, Surgeonfishes, Triggerfishes - 5 fish per vessel per day")]<-"Angelfishes, Grunts, Jacks, Surgeonfishes, Triggerfishes, and Wrasses combined - 5 fish per vessel per day"
bag_yr_df2_pr$LAB[which(bag_yr_df2_pr$LAB=="And Wrasses Combined Bag Limit: Angelfishes, Grunts, Jacks, Surgeonfishes, Triggerfishes - 15 fish per person per day")]<-"- 15 fish per person per day"
bag_yr_df2_pr$LAB[which(bag_yr_df2_pr$LAB=="Bar Jack - 15 fish per person per day")]<-"- 15 fish per person per day"
bag_yr_df2_pr$LAB[which(bag_yr_df2_pr$LAB=="Bigeye - 15 fish per person per day")]<-"- 15 fish per person per day"
bag_yr_df2_pr$LAB[which(bag_yr_df2_pr$LAB=="Black Durgon - 15 fish per person per day")]<-"- 15 fish per person per day"
bag_yr_df2_pr$LAB[which(bag_yr_df2_pr$LAB=="Black Grouper - 15 fish per person per day")]<-"- 15 fish per person per day"
bag_yr_df2_pr$LAB[which(bag_yr_df2_pr$LAB=="Black Jack - 15 fish per person per day")]<-"- 15 fish per person per day"
bag_yr_df2_pr$LAB[which(bag_yr_df2_pr$LAB=="Black Snapper - 15 fish per person per day")]<-"- 15 fish per person per day"
bag_yr_df2_pr$LAB[which(bag_yr_df2_pr$LAB=="Blackbar Soldierfish - 15 fish per person per day")]<-"- 15 fish per person per day"
bag_yr_df2_pr$LAB[which(bag_yr_df2_pr$LAB=="Blackfin Snapper - 15 fish per person per day")]<-"- 15 fish per person per day"
bag_yr_df2_pr$LAB[which(bag_yr_df2_pr$LAB=="Blackline Tilefish - 15 fish per person per day")]<-"- 15 fish per person per day"
bag_yr_df2_pr$LAB[which(bag_yr_df2_pr$LAB=="Blue Runner - 15 fish per person per day")]<-"- 15 fish per person per day"
bag_yr_df2_pr$LAB[which(bag_yr_df2_pr$LAB=="Black Jack - 15 fish per person per day")]<-"- 15 fish per person per day"
bag_yr_df2_pr$LAB[which(bag_yr_df2_pr$LAB=="Black Jack - 15 fish per person per day")]<-"- 15 fish per person per day"
bag_yr_df2_pr$LAB[which(bag_yr_df2_pr$LAB=="Black Jack - 15 fish per person per day")]<-"- 15 fish per person per day"
bag_yr_df2_pr$LAB[which(bag_yr_df2_pr$LAB=="Black Jack - 15 fish per person per day")]<-"- 15 fish per person per day"
bag_yr_df2_pr$LAB[which(bag_yr_df2_pr$LAB=="Black Jack - 15 fish per person per day")]<-"- 15 fish per person per day"
bag_yr_df2_pr$LAB[which(bag_yr_df2_pr$LAB=="Black Jack - 15 fish per person per day")]<-"- 15 fish per person per day"
bag_yr_df2_pr$LAB[which(bag_yr_df2_pr$LAB=="Black Jack - 15 fish per person per day")]<-"- 15 fish per person per day"

###############
# Crew limits #
###############

crew_df <- expand_status(mh_grf, "CREW BAG LIMIT")

# Collapse to annual value
crew_yr_df <- crew_df %>%
  mutate(YEAR = year(date_sequence)) %>%
  select(COMMON_NAME_USE, ZONE_USE, SUBSECTOR_USE, YEAR,
         VALUE, VALUE_UNITS, VALUE_TYPE, VALUE_RATE) %>%
  distinct() 

# Explore Zone 
table(crew_yr_df$ZONE_USE)
# All for all data

# Explore Subsector
table(crew_yr_df$SUBSECTOR_USE)
# For-Hire

# Explore year - only include complete years
table(crew_yr_df$YEAR)
crew_yr_df<-crew_yr_df %>% filter(YEAR <=termyr)
table(crew_yr_df$YEAR)

# Format for plot
crew_yr_df2 <- crew_yr_df %>%
  mutate(VALUE_UNITS = case_when(VALUE_UNITS == 'NUMBERS' ~ 'Fish',
                                 TRUE ~ VALUE_UNITS),
         VAL_LAB = paste(VALUE, VALUE_UNITS, str_to_title(VALUE_RATE)),
         LAB = paste(str_to_title(sub('(.*)\\,\\s+(.*)','\\2 \\1',COMMON_NAME_USE)),"-",  VAL_LAB),
         #         LAB = paste(COMMON_NAME_USE,"-",  VAL_LAB),
         #         LAB = paste(COMMON_NAME_USE, "(Zone ", ZONE_USE, ")", VAL_LAB),
         n = 1) %>%
  dplyr::select(LAB, YEAR, n) %>%
  arrange(YEAR) %>%
  pivot_wider(names_from = YEAR, values_from = n) %>%
  arrange(LAB) 

# Save outputs before we clean up labels
saveRDS(crew_yr_df2,paste0(Dirout,"recreational_crew_bag_limits.rds"))

#########
# Plots #
#########

library(raster)
#Note: importing this package masks select from dplyr...

#########################
# Bag limits by species #
#########################

bag<-raster(as.matrix(bag_yr_df2[,2:dim(bag_yr_df2)[2]]))
groups<-dplyr::select(bag_yr_df2, LAB) %>% pull()
names<-c(min(bag_yr_df$YEAR):max(bag_yr_df$YEAR))

# Note: Labels were too long, which ends up messing up the "square" shape of the plot; 
#   The longer the LABEL gets, the more white space there is above/below the actual data

jpeg(paste0(Dirout,"Recreational_bag_limits_species.jpeg"),res=300,height=4.8,width=6.7, units="in")
par(mar=c(0.1, 9.5, 2, 0), xpd=TRUE,oma=c(0,0,1.5,0))
brks<-c(0,1,2); cols<-c("lightblue","red")
plot(bag,breaks=brks,col=cols,axes=F,legend=F,legend.shrink=0.1,xlab="",main="")
abline(h=c(seq(0,1,(1/dim(bag_yr_df2)[1]))),col="lightgray")
abline(v=c(seq(0,1,(1/length(names)))),col="lightgray")
axis(2,at=seq(((1/dim(bag_yr_df2)[1])/2),1,(1/dim(bag_yr_df2)[1])),labels=rev(groups),las=2,tck=0,cex.axis=0.2,font=1)
axis(3,at=seq(((1/length(names))/2),1,(1/length(names))),labels=names,las=2,tck=0,cex.axis=0.5,las=2,hadj=0.7)
dev.off()

bag2<-raster(as.matrix(bag_yr_df3[,2:dim(bag_yr_df3)[2]]))
groups<-dplyr::select(bag_yr_df3, LAB) %>% pull()
names<-c(min(bag_yr_df$YEAR):max(bag_yr_df$YEAR))

# Note: Labels were too long, which ends up messing up the "square" shape of the plot; 
#   The longer the LABEL gets, the more white space there is above/below the actual data

########################################
# Bag limits by aggregates and species #
########################################

jpeg(paste0(Dirout,"Recreational_bag_limits.jpeg"),res=300,height=3.6,width=6.7, units="in")
par(mar=c(0.1, 14.5, 2, 0), xpd=TRUE,oma=c(0,0,1.5,0))
brks<-c(0,1,2); cols<-c("lightblue","red")
plot(bag2,breaks=brks,col=cols,axes=F,legend=F,legend.shrink=0.1,xlab="",main="")
abline(h=c(seq(0,1,(1/dim(bag_yr_df3)[1]))),col="lightgray")
abline(v=c(seq(0,1,(1/length(names)))),col="lightgray")
axis(2,at=seq(((1/dim(bag_yr_df3)[1])/2),1,(1/dim(bag_yr_df3)[1])),labels=rev(groups),las=2,tck=0,cex.axis=0.5,font=1)
axis(3,at=seq(((1/length(names))/2),1,(1/length(names))),labels=names,las=2,tck=0,cex.axis=0.5,las=2,hadj=0.7)
dev.off()

##########################
# Crew limits by species #
##########################

# Reorder by family/species (manually for now...)
crew_yr_df2<-crew_yr_df2[c(3,5,2,11,1,17,16,15,13,12,14,6,10,4,7:9),]

crew<-raster(as.matrix(crew_yr_df2[,2:dim(crew_yr_df2)[2]]))
groups<-dplyr::select(crew_yr_df2, LAB) %>% pull()
names<-c(min(crew_yr_df$YEAR):max(crew_yr_df$YEAR))

# Note: Labels were too long, which ends up messing up the "square" shape of the plot; 
#   The longer the LABEL gets, the more white space there is above/below the actual data

jpeg(paste0(Dirout,"Recreational_crew_limits.jpeg"),res=300,height=4.8,width=7, units="in")
par(mar=c(0.1, 11.5, 2, 0), xpd=TRUE,oma=c(0,0,1.5,0))
brks<-c(0,1,2); cols<-c("lightblue","red")
plot(crew,breaks=brks,col=cols,axes=F,legend=F,legend.shrink=0.1,xlab="",main="")
abline(h=c(seq(0,1,(1/dim(crew_yr_df2)[1]))),col="lightgray")
abline(v=c(seq(0,1,(1/length(names)))),col="lightgray")
axis(2,at=seq(((1/dim(crew_yr_df2)[1])/2),1,(1/dim(crew_yr_df2)[1])),labels=rev(groups),las=2,tck=0,cex.axis=0.5,font=1)
axis(3,at=seq(((1/length(names))/2),1,(1/length(names))),labels=names,las=2,tck=0,cex.axis=0.6,las=2,hadj=0.7)
dev.off()

#########
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)

termyr <- 2025

bag_yr_df_pr <- mh_carib_pr_bag %>%
  # keep only bag limit records you want
  dplyr::filter(
    NEVER_IMPLEMENTED == 0,
    SECTOR_USE == "RECREATIONAL",
    MANAGEMENT_TYPE_USE == "BAG LIMIT"
  ) %>%
  # ensure dates exist; if your fields are START_DATE2/END_DATE2 use those
  dplyr::mutate(
    start = as.Date(START_DATE2),
    end   = as.Date(dplyr::coalesce(END_DATE2, Sys.Date()))
  ) %>%
  dplyr::filter(!is.na(start), !is.na(end), end >= start) %>%
  # (optional) if you only want complete years
  dplyr::mutate(
    start_year = year(start),
    end_year   = year(end)
  ) %>%
  # expand to years, NOT days
  tidyr::uncount(end_year - start_year + 1, .id = "k") %>%
  dplyr::mutate(YEAR = start_year + (k - 1L)) %>%
  dplyr::filter(YEAR <= termyr) %>%
  # keep SPP_NAME (and anything else you need)
  dplyr::select(SPP_NAME, COMMON_NAME_USE, SUBSECTOR_USE,
                YEAR, VALUE, VALUE_UNITS, VALUE_TYPE, VALUE_RATE) %>%
  dplyr::distinct(SPP_NAME, SUBSECTOR_USE, YEAR, VALUE, VALUE_UNITS, VALUE_TYPE, VALUE_RATE)

bag_yr_df2_pr <- bag_yr_df_pr %>%
  dplyr::mutate(
    VALUE_UNITS = case_when(
      VALUE_UNITS == "NUMBERS" ~ "fish",
      VALUE_UNITS == "CONCH" ~ "conch",
      VALUE_UNITS == "LOBSTER" ~ "lobster", 
      TRUE ~ VALUE_UNITS
    ),
    VAL_LAB = paste(VALUE, VALUE_UNITS, str_to_lower(VALUE_RATE)),
    # include SPP_NAME in label (optional but recommended if you want that instead of COMMON_NAME_USE)
    LAB = paste(str_to_title(sub('(.*)\\,\\s+(.*)','\\2 \\1',SPP_NAME)),"-",  VAL_LAB),
    n = 1
  ) %>%
  dplyr::select(LAB, YEAR, n) %>%
  dplyr::distinct() %>%
  tidyr::pivot_wider(names_from = YEAR, values_from = n) %>%
  dplyr::arrange(LAB)

bag_yr_df2_pr<-bag_yr_df2_pr[c(2, 1, 4, 3, 6, 5, 8, 7, 9:12, 14, 13, 16, 15, 17:18, 20, 19, 21:22, 24, 23),]

# Remove species name in consecutive rows
bag_yr_df2_pr$LAB[which(bag_yr_df2_pr$LAB=="And Parrotfishes Combined Bag Limit: Groupers, Snappers - 5 fish per person per day")]<-"Groupers, Snappers, and Parrotfishes combined - 5 fish per vessel per day"
bag_yr_df2_pr$LAB[which(bag_yr_df2_pr$LAB=="And Parrotfishes Combined Bag Limit: Groupers, Snappers - 15 fish per vessel per day")]<-"Groupers, Snappers, and Parrotfishes combined - 15 fish per person per day"
bag_yr_df2_pr$LAB[which(bag_yr_df2_pr$LAB=="And Snappers Combined Bag Limit: Groupers, Parrotfishes - 5 fish per person per day")]<-"Groupers, Parrotfishes, and Snappers combined - 5 fish per person per day"
bag_yr_df2_pr$LAB[which(bag_yr_df2_pr$LAB=="And Snappers Combined Bag Limit: Groupers, Parrotfishes - 15 fish per vessel per day")]<-"Groupers, Parrotfishes, and Snappers combined - 15 fish per vessel per day"
bag_yr_df2_pr$LAB[which(bag_yr_df2_pr$LAB=="And Wrasses Combined Bag Limit: Angelfishes, Grunts, Jacks, Surgeonfishes, Triggerfishes - 5 fish per person per day")]<-"Angelfishes, Grunts, Jacks, Surgeonfishes, Triggerfishes, and Wrasses combined - 5 fish per person per day"
bag_yr_df2_pr$LAB[which(bag_yr_df2_pr$LAB=="And Wrasses Combined Bag Limit: Angelfishes, Grunts, Jacks, Surgeonfishes, Triggerfishes - 15 fish per vessel per day")]<-"Angelfishes, Grunts, Jacks, Surgeonfishes, Triggerfishes, and Wrasses combined - 15 fish per vessel per day"
bag_yr_df2_pr$LAB[which(bag_yr_df2_pr$LAB=="Bag Limit: Other Reef Fish Species Combined - 5 fish per person per day")]<-"Other Reef Fish Species Combined - 5 fish per person per day"
bag_yr_df2_pr$LAB[which(bag_yr_df2_pr$LAB=="Bag Limit: Other Reef Fish Species Combined - 15 fish per vessel per day")]<-"Other Reef Fish Species Combined - 15 fish per person per day"
bag_yr_df2_pr$LAB[which(bag_yr_df2_pr$LAB=="Bag Limit: Parrotfishes - 2 fish per person per day")]<-"Parrotfishes - 2 fish per person per day"
bag_yr_df2_pr$LAB[which(bag_yr_df2_pr$LAB=="Bag Limit: Parrotfishes - 6 fish per vessel per day")]<-"Parrotfishes - 6 fish per vessel per day"
bag_yr_df2_pr$LAB[which(bag_yr_df2_pr$LAB=="Bag Limit: Surgeonfish - 1 fish per person per day")]<-"Surgeonfish - 1 fish per person per day"
bag_yr_df2_pr$LAB[which(bag_yr_df2_pr$LAB=="Bag Limit: Surgeonfish - 4 fish per vessel per day")]<-"Surgeonfish - 4 fish per vessel per day"
#bag_yr_df2_pr$LAB[which(bag_yr_df2_pr$LAB=="Caribbean Spiny Lobster - 10 lobster per vessel per day")]<-"- 10 lobster per vessel per day"
#bag_yr_df2_pr$LAB[which(bag_yr_df2_pr$LAB=="Dolphin - 15 fish per vessel per day")]<-"- 15 fish per person per day"
#bag_yr_df2_pr$LAB[which(bag_yr_df2_pr$LAB=="Parrotfish 2 - 6 fish per person per day")]<-"- 6 fish per person per day"
#bag_yr_df2_pr$LAB[which(bag_yr_df2_pr$LAB=="Queen Conch - 12 conch per vessel per day")]<-"- 12 conch per vessel per day"
#bag_yr_df2_pr$LAB[which(bag_yr_df2_pr$LAB=="Surgeonfish - 4 fish per vessel per day")]<-"- 4 fish per vessel per day"
#bag_yr_df2_pr$LAB[which(bag_yr_df2_pr$LAB=="Wahoo - 10 fish per vessel per day")]<-"- 10 fish per vessel per day"

library(tidyverse)

# 1) Get just the year columns (handles if LAB is col 1)
yrs <- names(bag_yr_df2_pr)
yrs <- yrs[grepl("^\\d{4}$", yrs)]   # keep only "1985", "1986", ...
yrs <- yrs[as.integer(yrs) >= 1985 & as.integer(yrs) <= termyr]

# 2) Optional: drop empty rows (all NA across years)
keep_rows <- rowSums(!is.na(bag_yr_df2_pr[, yrs, drop = FALSE])) > 0

plot_df_pr <- bag_yr_df2_pr %>%
 # dplyr::filter(keep_rows) %>%
  # keep LAB + years
  dplyr::select(LAB, all_of(yrs)) %>%
  # long format: one row per LAB-year
  tidyr::pivot_longer(cols = dplyr::all_of(yrs), names_to = "YEAR", values_to = "n") %>%
  dplyr::mutate(
    YEAR = as.integer(YEAR),
    # presence flag (you can keep n as-is if you later want 2 = highlight)
    present = !is.na(n),
    # order so your first row is at the top (like your raster plot with rev())
    LAB = factor(LAB, levels = rev(unique(LAB)))
  )

# 3) Plot
p_pr_bag <- ggplot(plot_df_pr, aes(x = YEAR, y = LAB, fill = present)) +
  geom_tile(color = "lightgray", linewidth = 0.25) +
  scale_x_continuous(breaks = seq(1985, termyr, by = 2), expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_fill_manual(values = c(`TRUE` = "lightblue", `FALSE` = "white"), guide = "none") +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    plot.margin = margin(5, 5, 5, 5)
  )
print(p_pr_bag)
ggsave(filename = paste0(Dirout, "Commercial_size_limits_SC.jpeg"),
       plot = p_pr_bag, width = 6.5, height = 4.6, dpi = 300)



#############################
library(dplyr)
library(tidyr)
library(ggplot2)

termyr <- 2025

# Make sure it's not grouped, and LAB has no NA
bag_plot_base <- bag_yr_df2_pr %>%
  dplyr::ungroup() %>%
  dplyr::mutate(LAB = dplyr::coalesce(as.character(LAB), "(blank)"))

# 1) Year columns
yrs <- names(bag_plot_base)
yrs <- yrs[grepl("^\\d{4}$", yrs)]
yrs <- yrs[as.integer(yrs) >= 1985 & as.integer(yrs) <= termyr]

# 2) Create unique row IDs *once* in the same order you want
bag_row_map <- bag_plot_base %>%
  dplyr::mutate(LAB_ID = make.unique(LAB, sep = "___")) %>%
  dplyr::arrange(LAB) %>%                   # alphabetical A→Z
  dplyr::select(LAB, LAB_ID)

# 3) Long format for tiles (use the SAME LAB_ID logic as above)
plot_df_pr <- bag_plot_base %>%
  dplyr::mutate(LAB_ID = make.unique(LAB, sep = "___")) %>%
  dplyr::select(LAB, LAB_ID, dplyr::all_of(yrs)) %>%
  tidyr::pivot_longer(cols = dplyr::all_of(yrs), names_to = "YEAR", values_to = "n") %>%
  dplyr::mutate(
    YEAR = as.integer(YEAR),
    present = !is.na(n),
    # top-to-bottom A→Z:
    LAB_ID = factor(LAB_ID, levels = rev(bag_row_map$LAB_ID))
  )

# Optional: drop rows that are empty across all years
plot_df_pr <- plot_df_pr %>%
  dplyr::group_by(LAB_ID) %>%
  dplyr::filter(any(present)) %>%
  dplyr::ungroup()

# 4) Plot
p_pr_bag <- ggplot(plot_df_pr, aes(x = YEAR, y = LAB_ID, fill = present)) +
  geom_tile(color = "lightgray", linewidth = 0.25) +
  scale_x_continuous(breaks = seq(1985, termyr, by = 2), expand = c(0, 0)) +
  scale_y_discrete(
    expand = c(0, 0),
    labels = function(x) sub("___\\d+$", "", x)  # show original LAB text
  ) +
  scale_fill_manual(values = c(`TRUE` = "lightblue", `FALSE` = "white"), guide = "none") +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    plot.margin = margin(5, 5, 5, 5)
  )

print(p_pr_bag)

# 5) Save
ggsave(
  filename = paste0(Dirout, "Recreational_bag_limits_PR.jpeg"),
  plot = p_pr_bag,
  width = 7.0, height = 5.0, dpi = 300
)




################################

library(dplyr)
library(tidyr)
library(ggplot2)

# Make a unique internal id per row, preserving your designed order
bag_row_map <- bag_yr_df2_pr %>%
  dplyr::mutate(LAB_ID = make.unique(as.character(LAB), sep = "___")) %>%
  dplyr::select(LAB, LAB_ID)

bag_plot_long <- bag_yr_df2_pr %>%
  dplyr::mutate(LAB_ID = make.unique(as.character(LAB), sep = "___")) %>%
  tidyr::pivot_longer(
    cols = -c(LAB, LAB_ID),
    names_to = "YEAR",
    values_to = "n"
  ) %>%
  dplyr::mutate(
    YEAR = as.integer(YEAR),
    present = !is.na(n),
    # enforce your exact row order (top-to-bottom as in bag_yr_df2_pr)
    LAB_ID = factor(LAB_ID, levels = rev(bag_row_map$LAB_ID))
  ) %>%
  dplyr::filter(YEAR <= termyr)

p_bag <- ggplot(bag_plot_long, aes(x = YEAR, y = LAB_ID, fill = present)) +
  geom_tile(color = "lightgray", linewidth = 0.25) +
  scale_fill_manual(values = c(`TRUE` = "red", `FALSE` = "white"), guide = "none") +
  scale_x_continuous(
    breaks = seq(min(bag_plot_long$YEAR, na.rm = TRUE), termyr, by = 2),
    expand = c(0, 0)
  ) +
  scale_y_discrete(
    expand = c(0, 0),
    # show original LAB text (remove the make.unique suffix)
    labels = function(x) sub("___\\d+$", "", x)
  ) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_text(size = 8),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    plot.margin = margin(5, 5, 5, 5)
  )

ggsave(
  filename = paste0(Dirout, "Recreational_bag_limits_PR.jpeg"),
  plot = p_bag,
  width = 7.0, height = 5.0, dpi = 300
)










###########
#Try this
# =========================
# Bag-limit heatmap plot
# Y-axis: alphabetical by species/group, then numeric limit ascending (5 before 15, 3 before 10, etc.)
# Retains duplicate display labels as separate rows
# Assumes you already created + cleaned: bag_yr_df2_pr
# =========================

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

termyr <- 2025

# ---- Base table (ensure ungrouped, clean LAB) ----
bag_plot_base <- bag_yr_df2_pr %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    LAB = dplyr::coalesce(as.character(LAB), "(blank)"),
    # species/group text = everything before " - "
    species = sub(" - .*", "", LAB),
    # first number in the label (2, 5, 10, 12, 15, etc.)
    limit_value = as.numeric(stringr::str_extract(LAB, "\\d+"))
  )

# ---- Identify year columns ----
yrs <- names(bag_plot_base)
yrs <- yrs[grepl("^\\d{4}$", yrs)]
yrs <- yrs[as.integer(yrs) >= 1985 & as.integer(yrs) <= termyr]

# ---- Create a unique internal row id, then order alphabetically + numerically ----
bag_row_map <- bag_plot_base %>%
  dplyr::mutate(LAB_ID = make.unique(LAB, sep = "___")) %>%
  dplyr::arrange(species, limit_value) %>%       # A→Z by species, then 2<3<5<10<12<15...
  dplyr::select(LAB, LAB_ID)

# ---- Long data for tiles ----
plot_df_pr <- bag_plot_base %>%
  dplyr::mutate(LAB_ID = make.unique(LAB, sep = "___")) %>%
  dplyr::select(LAB, LAB_ID, species, limit_value, dplyr::all_of(yrs)) %>%
  tidyr::pivot_longer(cols = dplyr::all_of(yrs), names_to = "YEAR", values_to = "n") %>%
  dplyr::mutate(
    YEAR = as.integer(YEAR),
    present = !is.na(n),
    # ggplot draws lowest factor at bottom, so reverse to get A→Z from top to bottom
    LAB_ID = factor(LAB_ID, levels = rev(bag_row_map$LAB_ID))
  )

# Optional: drop rows that are empty across all years
plot_df_pr <- plot_df_pr %>%
  dplyr::group_by(LAB_ID) %>%
  dplyr::filter(any(present)) %>%
  dplyr::ungroup()

# ---- Plot ----
p_pr_bag <- ggplot(plot_df_pr, aes(x = YEAR, y = LAB_ID, fill = present)) +
  geom_tile(color = "lightgray", linewidth = 0.25) +
  scale_x_continuous(breaks = seq(1985, termyr, by = 2), expand = c(0, 0)) +
  scale_y_discrete(
    expand = c(0, 0),
    labels = function(x) sub("___\\d+$", "", x)  # remove internal suffix for display
  ) +
  scale_fill_manual(values = c(`TRUE` = "lightblue", `FALSE` = "white"), guide = "none") +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    plot.margin = margin(5, 5, 5, 5)
  )

print(p_pr_bag)

# ---- Save ----
ggsave(
  filename = paste0(Dirout, "Recreational_bag_limits_PR.jpeg"),
  plot = p_pr_bag,
  width = 12.0, height = 5.0, dpi = 300
)

#### St thomas st John
# Subset for Gulf Reef Fish recreational bag limits
mh_carib_st_bag <- mh %>% filter(FMP %in% c("REEF FISH FISHERY OF PUERTO RICO AND THE U.S. VIRGIN ISLANDS", 
                                            "FMP FOR THE EEZ AROUND ST. THOMAS AND ST. JOHN",
                                            #"FMP FOR THE EEZ AROUND ST. CROIX",
                                            #"FMP FOR THE EEZ AROUND PUERTO RICO",
                                            "SPINY LOBSTER FISHERY OF PUERTO RICO AND THE U.S. VIRGIN ISLANDS",
                                            "QUEEN CONCH RESOURCES OF PUERTO RICO AND THE U.S. VIRGIN ISLANDS")) %>%
  # Remove recs that were never implemented
  filter(NEVER_IMPLEMENTED == 0, SECTOR_USE == 'RECREATIONAL',
         MANAGEMENT_TYPE_USE == 'BAG LIMIT',
         ZONE_USE %in% c("ALL", "ST. THOMAS/ST. JOHN MANAGEMENT AREA"))
mh_carib_st_bag <- mh_carib_st_bag %>% 
  mutate(END_DATE2 = case_when(FR_CITATION == "61 FR 32151" & COMMON_NAME_USE == "CONCH, QUEEN" ~ as.Date("2021-10-13"),
                               FR_CITATION == "76 FR 82413" & SPP_NAME == "BAG LIMIT: OTHER REEF FISH SPECIES COMBINED" ~ as.Date("2022-10-13"),
                               FR_CITATION == "76 FR 82403" & SPP_NAME == "BAG LIMIT: GROUPERS, SNAPPERS, AND PARROTFISHES COMBINED" ~ as.Date("2022-10-13"),
                               FR_CITATION == "76 FR 82403" & SPP_NAME == "BAG LIMIT: PARROTFISHES" & VALUE == "2" ~ as.Date("2021-10-13"),
                               FR_CITATION == "76 FR 82403" & SPP_NAME == "BAG LIMIT: PARROTFISHES" & VALUE == "6" ~ as.Date("2021-10-13"),
                               FR_CITATION == "76 FR 82403" & SPP_NAME == "BAG LIMIT: PARROTFISHES" & VALUE == "5" ~ as.Date("2022-10-13"),
                               FR_CITATION == "76 FR 82403" & SPP_NAME == "BAG LIMIT: PARROTFISHES" & VALUE == "15" ~ as.Date("2022-10-13"),
                               TRUE ~ END_DATE2)) %>%
  filter(!(FR_CITATION == "87 FR 56204" & COMMON_NAME_USE == "LOBSTER, CARIBBEAN SPINY"))%>%
  filter(!(FR_CITATION == "87 FR 56204" & COMMON_NAME_USE == "SURGEONFISH"))

# Explore Zone 
table(mh_carib_st_bag$ZONE_USE)
# All (hogfish zone removed when excluding FR)

# Explore Subsector
table(mh_carib_st_bag$SUBSECTOR_USE)
# All for all data

library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)

termyr <- 2025

bag_yr_df_st <- mh_carib_st_bag %>%
  # keep only bag limit records you want
  dplyr::filter(
    NEVER_IMPLEMENTED == 0,
    SECTOR_USE == "RECREATIONAL",
    MANAGEMENT_TYPE_USE == "BAG LIMIT"
  ) %>%
  # ensure dates exist; if your fields are START_DATE2/END_DATE2 use those
  dplyr::mutate(
    start = as.Date(START_DATE2),
    end   = as.Date(dplyr::coalesce(END_DATE2, Sys.Date()))
  ) %>%
  dplyr::filter(!is.na(start), !is.na(end), end >= start) %>%
  # (optional) if you only want complete years
  dplyr::mutate(
    start_year = year(start),
    end_year   = year(end)
  ) %>%
  # expand to years, NOT days
  tidyr::uncount(end_year - start_year + 1, .id = "k") %>%
  dplyr::mutate(YEAR = start_year + (k - 1L)) %>%
  dplyr::filter(YEAR <= termyr) %>%
  # keep SPP_NAME (and anything else you need)
  dplyr::select(SPP_NAME, COMMON_NAME_USE, SUBSECTOR_USE,
                YEAR, VALUE, VALUE_UNITS, VALUE_TYPE, VALUE_RATE) %>%
  dplyr::distinct(SPP_NAME, SUBSECTOR_USE, YEAR, VALUE, VALUE_UNITS, VALUE_TYPE, VALUE_RATE)

bag_yr_df_st <- bag_yr_df_st %>%
  dplyr::mutate(
    VALUE_UNITS = case_when(
      VALUE_UNITS == "NUMBERS" ~ "fish",
      VALUE_UNITS == "CONCH" ~ "conch",
      VALUE_UNITS == "LOBSTER" ~ "lobster", 
      TRUE ~ VALUE_UNITS
    ),
    VAL_LAB = paste(VALUE, VALUE_UNITS, str_to_lower(VALUE_RATE)),
    # include SPP_NAME in label (optional but recommended if you want that instead of COMMON_NAME_USE)
    LAB = paste(str_to_title(sub('(.*)\\,\\s+(.*)','\\2 \\1',SPP_NAME)),"-",  VAL_LAB),
    n = 1
  ) %>%
  dplyr::select(LAB, YEAR, n) %>%
  dplyr::distinct() %>%
  tidyr::pivot_wider(names_from = YEAR, values_from = n) %>%
  dplyr::arrange(LAB)

# Remove species name in consecutive rows
bag_yr_df_st$LAB[which(bag_yr_df_st$LAB=="And Parrotfishes Combined Bag Limit: Groupers, Snappers - 5 fish per person per day")]<-"Groupers, Snappers, and Parrotfishes combined - 5 fish per vessel per day"
bag_yr_df_st$LAB[which(bag_yr_df_st$LAB=="And Parrotfishes Combined Bag Limit: Groupers, Snappers - 15 fish per vessel per day")]<-"Groupers, Snappers, and Parrotfishes combined - 15 fish per person per day"
bag_yr_df_st$LAB[which(bag_yr_df_st$LAB=="And Snappers Combined Bag Limit: Groupers, Parrotfishes - 5 fish per person per day")]<-"Groupers, Parrotfishes, and Snappers combined - 5 fish per person per day"
bag_yr_df_st$LAB[which(bag_yr_df_st$LAB=="And Snappers Combined Bag Limit: Groupers, Parrotfishes - 15 fish per vessel per day")]<-"Groupers, Parrotfishes, and Snappers combined - 15 fish per vessel per day"
bag_yr_df_st$LAB[which(bag_yr_df_st$LAB=="And Wrasses Combined Bag Limit: Angelfishes, Grunts, Jacks, Surgeonfishes, Triggerfishes - 5 fish per person per day")]<-"Angelfishes, Grunts, Jacks, Surgeonfishes, Triggerfishes, and Wrasses combined - 5 fish per person per day"
bag_yr_df_st$LAB[which(bag_yr_df_st$LAB=="And Wrasses Combined Bag Limit: Angelfishes, Grunts, Jacks, Surgeonfishes, Triggerfishes - 15 fish per vessel per day")]<-"Angelfishes, Grunts, Jacks, Surgeonfishes, Triggerfishes, and Wrasses combined - 15 fish per vessel per day"
bag_yr_df_st$LAB[which(bag_yr_df_st$LAB=="Bag Limit: Other Reef Fish Species Combined - 5 fish per person per day")]<-"Other Reef Fish Species Combined - 5 fish per person per day"
bag_yr_df_st$LAB[which(bag_yr_df_st$LAB=="Bag Limit: Other Reef Fish Species Combined - 15 fish per vessel per day")]<-"Other Reef Fish Species Combined - 15 fish per person per day"
bag_yr_df_st$LAB[which(bag_yr_df_st$LAB=="Bag Limit: Parrotfishes - 2 fish per person per day")]<-"Parrotfishes - 2 fish per person per day"
bag_yr_df_st$LAB[which(bag_yr_df_st$LAB=="Bag Limit: Parrotfishes - 6 fish per vessel per day")]<-"Parrotfishes - 6 fish per vessel per day"
bag_yr_df_st$LAB[which(bag_yr_df_st$LAB=="Bag Limit: Surgeonfish - 1 fish per person per day")]<-"Surgeonfish - 1 fish per person per day"
bag_yr_df_st$LAB[which(bag_yr_df_st$LAB=="Bag Limit: Surgeonfish - 4 fish per vessel per day")]<-"Surgeonfish - 4 fish per vessel per day"


# =========================
# Bag-limit heatmap plot
# Y-axis: alphabetical by species/group, then numeric limit ascending (5 before 15, 3 before 10, etc.)
# Retains duplicate display labels as separate rows
# Assumes you already created + cleaned: bag_yr_df2_pr
# =========================

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

termyr <- 2025

# ---- Base table (ensure ungrouped, clean LAB) ----
bag_plot_base <- bag_yr_df_st %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    LAB = dplyr::coalesce(as.character(LAB), "(blank)"),
    # species/group text = everything before " - "
    species = sub(" - .*", "", LAB),
    # first number in the label (2, 5, 10, 12, 15, etc.)
    limit_value = as.numeric(stringr::str_extract(LAB, "\\d+"))
  )

# ---- Identify year columns ----
yrs <- names(bag_plot_base)
yrs <- yrs[grepl("^\\d{4}$", yrs)]
yrs <- yrs[as.integer(yrs) >= 1985 & as.integer(yrs) <= termyr]

# ---- Create a unique internal row id, then order alphabetically + numerically ----
bag_row_map <- bag_plot_base %>%
  dplyr::mutate(LAB_ID = make.unique(LAB, sep = "___")) %>%
  dplyr::arrange(species, limit_value) %>%       # A→Z by species, then 2<3<5<10<12<15...
  dplyr::select(LAB, LAB_ID)

# ---- Long data for tiles ----
plot_df_st <- bag_plot_base %>%
  dplyr::mutate(LAB_ID = make.unique(LAB, sep = "___")) %>%
  dplyr::select(LAB, LAB_ID, species, limit_value, dplyr::all_of(yrs)) %>%
  tidyr::pivot_longer(cols = dplyr::all_of(yrs), names_to = "YEAR", values_to = "n") %>%
  dplyr::mutate(
    YEAR = as.integer(YEAR),
    present = !is.na(n),
    # ggplot draws lowest factor at bottom, so reverse to get A→Z from top to bottom
    LAB_ID = factor(LAB_ID, levels = rev(bag_row_map$LAB_ID))
  )

# Optional: drop rows that are empty across all years
plot_df_st <- plot_df_st %>%
  dplyr::group_by(LAB_ID) %>%
  dplyr::filter(any(present)) %>%
  dplyr::ungroup()

# ---- Plot ----
p_st_bag <- ggplot(plot_df_st, aes(x = YEAR, y = LAB_ID, fill = present)) +
  geom_tile(color = "lightgray", linewidth = 0.25) +
  scale_x_continuous(breaks = seq(1985, termyr, by = 2), expand = c(0, 0)) +
  scale_y_discrete(
    expand = c(0, 0),
    labels = function(x) sub("___\\d+$", "", x)  # remove internal suffix for display
  ) +
  scale_fill_manual(values = c(`TRUE` = "lightblue", `FALSE` = "white"), guide = "none") +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    plot.margin = margin(5, 5, 5, 5)
  )

print(p_st_bag)

# ---- Save ----
ggsave(
  filename = paste0(Dirout, "Recreational_bag_limits_PR.jpeg"),
  plot = p_pr_bag,
  width = 7.0, height = 5.0, dpi = 300
)

#### St croix
# Subset for Gulf Reef Fish recreational bag limits
mh_carib_sc_bag <- mh %>% filter(FMP %in% c("REEF FISH FISHERY OF PUERTO RICO AND THE U.S. VIRGIN ISLANDS", 
                                            #"FMP FOR THE EEZ AROUND ST. THOMAS AND ST. JOHN",
                                            "FMP FOR THE EEZ AROUND ST. CROIX",
                                            #"FMP FOR THE EEZ AROUND PUERTO RICO",
                                            "SPINY LOBSTER FISHERY OF PUERTO RICO AND THE U.S. VIRGIN ISLANDS",
                                            "QUEEN CONCH RESOURCES OF PUERTO RICO AND THE U.S. VIRGIN ISLANDS")) %>%
  # Remove recs that were never implemented
  filter(NEVER_IMPLEMENTED == 0, SECTOR_USE == 'RECREATIONAL',
         MANAGEMENT_TYPE_USE == 'BAG LIMIT',
         ZONE_USE %in% c("ALL", "ST. CROIX MANAGEMENT AREA"))
mh_carib_sc_bag <- mh_carib_sc_bag %>% 
  mutate(END_DATE2 = case_when(FR_CITATION == "61 FR 32151" & COMMON_NAME_USE == "CONCH, QUEEN" ~ as.Date("2021-10-13"),
                               FR_CITATION == "76 FR 82413" & SPP_NAME == "BAG LIMIT: OTHER REEF FISH SPECIES COMBINED" ~ as.Date("2022-10-13"),
                               FR_CITATION == "76 FR 82403" & SPP_NAME == "BAG LIMIT: GROUPERS, SNAPPERS, AND PARROTFISHES COMBINED" ~ as.Date("2022-10-13"),
                               FR_CITATION == "76 FR 82403" & SPP_NAME == "BAG LIMIT: PARROTFISHES" & VALUE == "2" ~ as.Date("2021-10-13"),
                               FR_CITATION == "76 FR 82403" & SPP_NAME == "BAG LIMIT: PARROTFISHES" & VALUE == "6" ~ as.Date("2021-10-13"),
                               FR_CITATION == "76 FR 82403" & SPP_NAME == "BAG LIMIT: PARROTFISHES" & VALUE == "5" ~ as.Date("2022-10-13"),
                               FR_CITATION == "76 FR 82403" & SPP_NAME == "BAG LIMIT: PARROTFISHES" & VALUE == "15" ~ as.Date("2022-10-13"),
                               TRUE ~ END_DATE2)) %>%
  filter(!(FR_CITATION == "87 FR 56204" & COMMON_NAME_USE == "LOBSTER, CARIBBEAN SPINY"))%>%
  filter(!(FR_CITATION == "87 FR 56204" & COMMON_NAME_USE == "SURGEONFISH"))

# Explore Zone 
table(mh_carib_sc_bag$ZONE_USE)
# All (hogfish zone removed when excluding FR)

# Explore Subsector
table(mh_carib_sc_bag$SUBSECTOR_USE)
# All for all data

library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)

termyr <- 2025

bag_yr_df_sc <- mh_carib_sc_bag %>%
  # keep only bag limit records you want
  dplyr::filter(
    NEVER_IMPLEMENTED == 0,
    SECTOR_USE == "RECREATIONAL",
    MANAGEMENT_TYPE_USE == "BAG LIMIT"
  ) %>%
  # ensure dates exist; if your fields are START_DATE2/END_DATE2 use those
  dplyr::mutate(
    start = as.Date(START_DATE2),
    end   = as.Date(dplyr::coalesce(END_DATE2, Sys.Date()))
  ) %>%
  dplyr::filter(!is.na(start), !is.na(end), end >= start) %>%
  # (optional) if you only want complete years
  dplyr::mutate(
    start_year = year(start),
    end_year   = year(end)
  ) %>%
  # expand to years, NOT days
  tidyr::uncount(end_year - start_year + 1, .id = "k") %>%
  dplyr::mutate(YEAR = start_year + (k - 1L)) %>%
  dplyr::filter(YEAR <= termyr) %>%
  # keep SPP_NAME (and anything else you need)
  dplyr::select(SPP_NAME, COMMON_NAME_USE, SUBSECTOR_USE,
                YEAR, VALUE, VALUE_UNITS, VALUE_TYPE, VALUE_RATE) %>%
  dplyr::distinct(SPP_NAME, SUBSECTOR_USE, YEAR, VALUE, VALUE_UNITS, VALUE_TYPE, VALUE_RATE)

bag_yr_df_sc <- bag_yr_df_sc %>%
  dplyr::mutate(
    VALUE_UNITS = case_when(
      VALUE_UNITS == "NUMBERS" ~ "fish",
      VALUE_UNITS == "CONCH" ~ "conch",
      VALUE_UNITS == "LOBSTER" ~ "lobster", 
      TRUE ~ VALUE_UNITS
    ),
    VAL_LAB = paste(VALUE, VALUE_UNITS, str_to_lower(VALUE_RATE)),
    # include SPP_NAME in label (optional but recommended if you want that instead of COMMON_NAME_USE)
    LAB = paste(str_to_title(sub('(.*)\\,\\s+(.*)','\\2 \\1',SPP_NAME)),"-",  VAL_LAB),
    n = 1
  ) %>%
  dplyr::select(LAB, YEAR, n) %>%
  dplyr::distinct() %>%
  tidyr::pivot_wider(names_from = YEAR, values_from = n) %>%
  dplyr::arrange(LAB)

# Remove species name in consecutive rows
bag_yr_df_sc$LAB[which(bag_yr_df_sc$LAB=="And Parrotfishes Combined Bag Limit: Groupers, Snappers - 5 fish per person per day")]<-"Groupers, Snappers, and Parrotfishes combined - 5 fish per vessel per day"
bag_yr_df_sc$LAB[which(bag_yr_df_sc$LAB=="And Parrotfishes Combined Bag Limit: Groupers, Snappers - 15 fish per vessel per day")]<-"Groupers, Snappers, and Parrotfishes combined - 15 fish per person per day"
bag_yr_df_sc$LAB[which(bag_yr_df_sc$LAB=="And Snappers Combined Bag Limit: Groupers, Parrotfishes - 5 fish per person per day")]<-"Groupers, Parrotfishes, and Snappers combined - 5 fish per person per day"
bag_yr_df_sc$LAB[which(bag_yr_df_sc$LAB=="And Snappers Combined Bag Limit: Groupers, Parrotfishes - 15 fish per vessel per day")]<-"Groupers, Parrotfishes, and Snappers combined - 15 fish per vessel per day"
bag_yr_df_sc$LAB[which(bag_yr_df_sc$LAB=="And Wrasses Combined Bag Limit: Angelfishes, Grunts, Jacks, Surgeonfishes, Triggerfishes - 5 fish per person per day")]<-"Angelfishes, Grunts, Jacks, Surgeonfishes, Triggerfishes, and Wrasses combined - 5 fish per person per day"
bag_yr_df_sc$LAB[which(bag_yr_df_sc$LAB=="And Wrasses Combined Bag Limit: Angelfishes, Grunts, Jacks, Surgeonfishes, Triggerfishes - 15 fish per vessel per day")]<-"Angelfishes, Grunts, Jacks, Surgeonfishes, Triggerfishes, and Wrasses combined - 15 fish per vessel per day"
bag_yr_df_sc$LAB[which(bag_yr_df_sc$LAB=="Bag Limit: Other Reef Fish Species Combined - 5 fish per person per day")]<-"Other Reef Fish Species Combined - 5 fish per person per day"
bag_yr_df_sc$LAB[which(bag_yr_df_sc$LAB=="Bag Limit: Other Reef Fish Species Combined - 15 fish per vessel per day")]<-"Other Reef Fish Species Combined - 15 fish per person per day"
bag_yr_df_sc$LAB[which(bag_yr_df_sc$LAB=="Bag Limit: Parrotfishes - 2 fish per person per day")]<-"Parrotfishes - 2 fish per person per day"
bag_yr_df_sc$LAB[which(bag_yr_df_sc$LAB=="Bag Limit: Parrotfishes - 6 fish per vessel per day")]<-"Parrotfishes - 6 fish per vessel per day"
bag_yr_df_sc$LAB[which(bag_yr_df_sc$LAB=="Bag Limit: Surgeonfish - 1 fish per person per day")]<-"Surgeonfish - 1 fish per person per day"
bag_yr_df_sc$LAB[which(bag_yr_df_sc$LAB=="Bag Limit: Surgeonfish - 4 fish per vessel per day")]<-"Surgeonfish - 4 fish per vessel per day"


# =========================
# Bag-limit heatmap plot
# Y-axis: alphabetical by species/group, then numeric limit ascending (5 before 15, 3 before 10, etc.)
# Retains duplicate display labels as separate rows
# Assumes you already created + cleaned: bag_yr_df2_pr
# =========================

library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

termyr <- 2025

# ---- Base table (ensure ungrouped, clean LAB) ----
bag_plot_base <- bag_yr_df_sc %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    LAB = dplyr::coalesce(as.character(LAB), "(blank)"),
    # species/group text = everything before " - "
    species = sub(" - .*", "", LAB),
    # first number in the label (2, 5, 10, 12, 15, etc.)
    limit_value = as.numeric(stringr::str_extract(LAB, "\\d+"))
  )

# ---- Identify year columns ----
yrs <- names(bag_plot_base)
yrs <- yrs[grepl("^\\d{4}$", yrs)]
yrs <- yrs[as.integer(yrs) >= 1985 & as.integer(yrs) <= termyr]

# ---- Create a unique internal row id, then order alphabetically + numerically ----
bag_row_map <- bag_plot_base %>%
  dplyr::mutate(LAB_ID = make.unique(LAB, sep = "___")) %>%
  dplyr::arrange(species, limit_value) %>%       # A→Z by species, then 2<3<5<10<12<15...
  dplyr::select(LAB, LAB_ID)

# ---- Long data for tiles ----
plot_df_sc <- bag_plot_base %>%
  dplyr::mutate(LAB_ID = make.unique(LAB, sep = "___")) %>%
  dplyr::select(LAB, LAB_ID, species, limit_value, dplyr::all_of(yrs)) %>%
  tidyr::pivot_longer(cols = dplyr::all_of(yrs), names_to = "YEAR", values_to = "n") %>%
  dplyr::mutate(
    YEAR = as.integer(YEAR),
    present = !is.na(n),
    # ggplot draws lowest factor at bottom, so reverse to get A→Z from top to bottom
    LAB_ID = factor(LAB_ID, levels = rev(bag_row_map$LAB_ID))
  )

# Optional: drop rows that are empty across all years
plot_df_sc <- plot_df_sc %>%
  dplyr::group_by(LAB_ID) %>%
  dplyr::filter(any(present)) %>%
  dplyr::ungroup()

# ---- Plot ----
p_sc_bag <- ggplot(plot_df_sc, aes(x = YEAR, y = LAB_ID, fill = present)) +
  geom_tile(color = "lightgray", linewidth = 0.25) +
  scale_x_continuous(breaks = seq(1985, termyr, by = 2), expand = c(0, 0)) +
  scale_y_discrete(
    expand = c(0, 0),
    labels = function(x) sub("___\\d+$", "", x)  # remove internal suffix for display
  ) +
  scale_fill_manual(values = c(`TRUE` = "lightblue", `FALSE` = "white"), guide = "none") +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_text(size = 9),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    plot.margin = margin(5, 5, 5, 5)
  )

print(p_sc_bag)

# ---- Save ----
ggsave(
  filename = paste0(Dirout, "Recreational_bag_limits_PR.jpeg"),
  plot = p_pr_bag,
  width = 7.0, height = 5.0, dpi = 300
)

