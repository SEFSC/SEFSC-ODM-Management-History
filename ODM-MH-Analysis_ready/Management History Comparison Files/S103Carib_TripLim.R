##########################################
#  Commercial Trip Limits & IFQ Program  #
##########################################

librarian::shelf(here, tidyverse, lubridate, openxlsx, neatRanges, splitstackshape, dplyr)

# Set output directory for plots
Dirout<-paste0(getwd(),"/Trip limits and IFQ/")
if(!file.exists(Dirout)) dir.create(Dirout)

# MH dataset 
mh <- readRDS(here("ODM-MH-Data_log", "data", "results", "MH_DL_2025Nov18.RDS"))

# Set end year for plots
termyr<-2025

# Subset for Gulf Reef Fish commercial trip limits and IFQ
# Subset for Gulf Reef Fish size limits - Puerto Rico
mh_carib_pr_trip <- mh %>% filter(FMP %in% c("REEF FISH FISHERY OF PUERTO RICO AND THE U.S. VIRGIN ISLANDS", 
                                        #"FMP FOR THE EEZ AROUND ST. THOMAS AND ST. JOHN",
                                        #"FMP FOR THE EEZ AROUND ST. CROIX",
                                        "FMP FOR THE EEZ AROUND PUERTO RICO",
                                        "SPINY LOBSTER FISHERY OF PUERTO RICO AND THE U.S. VIRGIN ISLANDS",
                                        "QUEEN CONCH RESOURCES OF PUERTO RICO AND THE U.S. VIRGIN ISLANDS")) %>%
  # Remove recs that were never implemented
  filter(NEVER_IMPLEMENTED == 0, SECTOR_USE == 'COMMERCIAL',
         MANAGEMENT_TYPE_USE == 'TRIP LIMIT',
         ZONE_USE %in% c("ALL", "PUERTO RICO MANAGEMENT AREA"))
mh_carib_pr_trip <- mh_carib_pr_trip %>% 
  mutate(END_DATE2 = case_when(FR_CITATION == "78 FR 56171" & COMMON_NAME_USE == "CONCH, QUEEN" ~ as.Date("2022-10-13"),
                               TRUE ~ END_DATE2))

# Confirm trip limits
unique(mh_carib_pr_trip$MANAGEMENT_TYPE_USE)

# Confirm only commercial
table(mh_carib_pr_trip$SECTOR_USE)

source(here("ODM-MH-Analysis_ready", "Closures", "func_expand_status.R"))

#############
# Trip limits
#############

trip_df_pr <- expand_status(mh_carib_pr_trip, "TRIP LIMIT")

# Collapse to annual value
trip_df_pr <- trip_df_pr %>%
  mutate(YEAR = year(date_sequence)) %>%
  dplyr::select(COMMON_NAME_USE, ZONE_USE, SUBSECTOR_USE, YEAR,
                VALUE, VALUE_UNITS, VALUE_TYPE, VALUE_RATE) %>%
  distinct() 

# Explore Zone 
table(trip_df_pr$ZONE_USE)
# All for all data

# Explore Subsector
table(trip_df_pr$SUBSECTOR_USE)
# All for all data

# Explore year - only include complete years 
table(trip_df_pr$YEAR)
trip_df_pr<-trip_df_pr %>% filter(YEAR <=termyr)
table(trip_df_pr$YEAR)

# Format for plot
trip_df_pr2 <- trip_df_pr %>%
  mutate(VALUE_UNITS = case_when(VALUE_UNITS == 'POUNDS' ~ 'lb',
                                 TRUE ~ VALUE_UNITS),
         VALUE_UNITS = case_when(VALUE_UNITS == 'CONCH' ~ 'conch',
                                 TRUE ~ VALUE_UNITS),
         VALUE_TYPE = case_when(VALUE_TYPE == 'GUTTED WEIGHT' ~ 'GW',
                                TRUE ~ VALUE_TYPE),
         VALUE_TYPE = case_when(VALUE_TYPE == 'ROUND (WHOLE) WEIGHT' ~ 'WW',
                                TRUE ~ VALUE_TYPE),
         VALUE_TYPE = case_when(VALUE_TYPE == 'COUNT' ~ '',
                                TRUE ~ VALUE_TYPE),
         VALUE_RATE = case_when(VALUE_RATE == 'PER VESSEL PER DAY' ~ 'per vessel per day',
                                VALUE_RATE == "PER PERSON PER DAY" ~ "per person per day",
                                TRUE ~ VALUE_RATE),
         VAL_LAB = paste(VALUE, VALUE_UNITS, VALUE_TYPE, VALUE_RATE),
         LAB = paste(str_to_title(sub('(.*)\\,\\s+(.*)','\\2 \\1',COMMON_NAME_USE)),"-",  VAL_LAB),
         #         LAB = paste(COMMON_NAME_USE,"-",  VAL_LAB),
         #         LAB = paste(COMMON_NAME_USE, "(Zone ", ZONE_USE, ")", VAL_LAB),
         n = 1) %>%
  dplyr::select(LAB, YEAR, n) %>%
  arrange(YEAR) %>%
  pivot_wider(names_from = YEAR, values_from = n) %>%
  arrange(LAB)

view(trip_df_pr2)
trip_df_pr2$LAB
# Individual species are showing up, rather than aggregates
#   Black Grouper (10000, 6000, 7500, 5500)
#   Dwarf Sand Perch (10000, 7500)
#   Gag Grouper (10000, 6000, 7500, 5500)
#   Gray Triggerfish (12, 16, 25)
#   Greater Amberjack (1000, 1500, 2000, 250, 7)
#   Misty Grouper (10000, 6000, 7500)
#   Red Grouper (10000, 6000, 7500, 5500)
#   Red Hind (10000, 6000, 7500, 5500)
#   Red Snapper (1000, 200, 2000)
#   Rock Hind (10000, 6000, 7500, 5500)
#   Sand Perch (10000, 7500)
#   Scamp (10000, 6000, 7500, 5500)
#   Snowy Grouper (10000, 6000, 7500)
#   Speckled Hind (10000, 6000, 7500)
#   Warsaw Grouper (10000, 6000, 7500)
#   Yellowedge Grouper (10000, 6000, 7500)
#   Yellowfin Grouper (10000, 6000, 7500, 5500)
#   Yellowmouth Grouper (10000, 6000, 7500, 5500)

# Need to end trip limits when IFQ comes in 
#trip_yr_df2[trip_yr_df2$LAB=="Red Snapper - 200 lb WW per trip",which(colnames(trip_yr_df2)=="2007"):dim(trip_yr_df2)[2]]<-NA #2007+
#trip_yr_df2[trip_yr_df2$LAB=="Red Snapper - 2000 lb WW per trip",which(colnames(trip_yr_df2)=="2007"):dim(trip_yr_df2)[2]]<-NA #2007+
#trip_yr_df2[trip_yr_df2$LAB=="Black Grouper - 6000 lb GW per trip",which(colnames(trip_yr_df2)=="2010"):dim(trip_yr_df2)[2]]<-NA #2010+
#trip_yr_df2[trip_yr_df2$LAB=="Gag Grouper - 6000 lb GW per trip",which(colnames(trip_yr_df2)=="2010"):dim(trip_yr_df2)[2]]<-NA #2010+
#trip_yr_df2[trip_yr_df2$LAB=="Misty Grouper - 6000 lb GW per trip",which(colnames(trip_yr_df2)=="2010"):dim(trip_yr_df2)[2]]<-NA #2010+
#trip_yr_df2[trip_yr_df2$LAB=="Red Grouper - 6000 lb GW per trip",which(colnames(trip_yr_df2)=="2010"):dim(trip_yr_df2)[2]]<-NA #2010+
#trip_yr_df2[trip_yr_df2$LAB=="Red Hind - 6000 lb GW per trip",which(colnames(trip_yr_df2)=="2010"):dim(trip_yr_df2)[2]]<-NA #2010+
#trip_yr_df2[trip_yr_df2$LAB=="Rock Hind - 6000 lb GW per trip",which(colnames(trip_yr_df2)=="2010"):dim(trip_yr_df2)[2]]<-NA #2010+
#trip_yr_df2[trip_yr_df2$LAB=="Scamp - 6000 lb GW per trip",which(colnames(trip_yr_df2)=="2010"):dim(trip_yr_df2)[2]]<-NA #2010+
##trip_yr_df2[trip_yr_df2$LAB=="Snowy Grouper - 6000 lb GW per trip",which(colnames(trip_yr_df2)=="2010"):dim(trip_yr_df2)[2]]<-NA #2010+
#trip_yr_df2[trip_yr_df2$LAB=="Speckled Hind - 6000 lb GW per trip",which(colnames(trip_yr_df2)=="2010"):dim(trip_yr_df2)[2]]<-NA #2010+
#trip_yr_df2[trip_yr_df2$LAB=="Warsaw Grouper - 6000 lb GW per trip",which(colnames(trip_yr_df2)=="2010"):dim(trip_yr_df2)[2]]<-NA #2010+
#trip_yr_df2[trip_yr_df2$LAB=="Yellowedge Grouper - 6000 lb GW per trip",which(colnames(trip_yr_df2)=="2010"):dim(trip_yr_df2)[2]]<-NA #2010+
#trip_yr_df2[trip_yr_df2$LAB=="Yellowfin Grouper - 6000 lb GW per trip",which(colnames(trip_yr_df2)=="2010"):dim(trip_yr_df2)[2]]<-NA #2010+
#trip_yr_df2[trip_yr_df2$LAB=="Yellowmouth Grouper - 6000 lb GW per trip",which(colnames(trip_yr_df2)=="2010"):dim(trip_yr_df2)[2]]<-NA #2010+


# Keep Gray Triggerfish, Greater Amberjack, Red Snapper, D&SWG (Black Grouper), DWG (e.g.,Misty)
#trip_yr_df3<-trip_yr_df2[c(11:13,17,14:16,18,31,30,32,2:4,1,20:21,19),]

# Need to end trip limits when IFQ comes in for D&SWG (2010), DWG (2010), and RSN (2007)
#trip_yr_df3[trip_yr_df3$LAB=="Red Snapper - 200 lb WW per trip",which(colnames(trip_yr_df3)=="2007"):dim(trip_yr_df3)[2]]<-NA #2007+
#trip_yr_df3[trip_yr_df3$LAB=="Red Snapper - 2000 lb WW per trip",which(colnames(trip_yr_df3)=="2007"):dim(trip_yr_df3)[2]]<-NA #2007+
#trip_yr_df3[trip_yr_df3$LAB=="Black Grouper - 6000 lb GW per trip",which(colnames(trip_yr_df3)=="2010"):dim(trip_yr_df3)[2]]<-NA #2010+
#trip_yr_df3[trip_yr_df3$LAB=="Misty Grouper - 6000 lb GW per trip",which(colnames(trip_yr_df3)=="2010"):dim(trip_yr_df3)[2]]<-NA #2010+

# Need to redefine Misty Grouper as Deepwater Grouper
#trip_yr_df3$LAB[trip_yr_df3$LAB=="Misty Grouper - 6000 lb GW per trip"]<-"Deep-Water Grouper - 6000 lb GW per trip"
#trip_yr_df3$LAB[trip_yr_df3$LAB=="Misty Grouper - 7500 lb GW per trip"]<-"Deep-Water Grouper - 7500 lb GW per trip"
#trip_yr_df3$LAB[trip_yr_df3$LAB=="Misty Grouper - 10000 lb GW per trip"]<-"Deep-Water Grouper - 10000 lb GW per trip"

# Need to redefine Black Grouper as Deep & Shallowwater Grouper
#trip_yr_df3$LAB[trip_yr_df3$LAB=="Black Grouper - 5500 lb GW per trip"]<-"Shallow-Water Grouper - 5500 lb GW per trip"
#trip_yr_df3$LAB[trip_yr_df3$LAB=="Black Grouper - 6000 lb GW per trip"]<-"Shallow-Water Grouper - 6000 lb GW per trip"
#trip_yr_df3$LAB[trip_yr_df3$LAB=="Black Grouper - 7500 lb GW per trip"]<-"Shallow-Water Grouper - 7500 lb GW per trip"
#trip_yr_df3$LAB[trip_yr_df3$LAB=="Black Grouper - 10000 lb GW per trip"]<-"Shallow-Water Grouper - 10000 lb GW per trip"

# Save outputs before we clean up labels
saveRDS(trip_df_pr2,paste0(Dirout,"commercial_trip_limits.rds"))

# Remove extra space in label
trip_df_pr2$LAB[which(trip_df_pr2$LAB=="Queen Conch - 200 conch  per vessel per day")]<-"- 200 conch  per vessel per day"

###########################################################
library(tidyverse)

# 1) Get just the year columns (handles if LAB is col 1)
yrs <- names(trip_df_pr2)
yrs <- yrs[grepl("^\\d{4}$", yrs)]   # keep only "1985", "1986", ...
yrs <- yrs[as.integer(yrs) >= 1985 & as.integer(yrs) <= termyr]

# 2) Optional: drop empty rows (all NA across years)
keep_rows <- rowSums(!is.na(trip_df_pr2[, yrs, drop = FALSE])) > 0

plot_df_pr <- trip_df_pr2 %>%
  dplyr::filter(keep_rows) %>%
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
p_pr_trip <- ggplot(plot_df_pr, aes(x = YEAR, y = LAB, fill = present)) +
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
print(p_pr_trip)
ggsave(filename = paste0(Dirout, "Commercial_trip_limits_pr.jpeg"),
       plot = p_pr_trip, width = 6.5, height = 4.6, dpi = 300)


#########
# Plots #
#########

library(raster)
#Note: importing this package masks select from dplyr...


groups<-dplyr::select(trip_df_pr2, LAB) %>% pull()
names<-c(min(trip_df_pr2$YEAR):max(trip_df_pr2$YEAR))

yrs <- as.character(1997:2025)
yrs <- names(trip_df_pr2)
yrs <- yrs[grepl("^\\d{4}$", yrs)]
yrs <- yrs[as.integer(yrs) >= 1997 & as.integer(yrs) <= 2025]

Trip<-raster(as.matrix(trip_df_pr2[,2:dim(trip_df_pr2)[2]]))
m <- as.matrix(trip_df_pr2[, -1])
keep_rows <- which(rowSums(!is.na(m))>0)
m_trim <- m[keep_rows, , drop = FALSE]
groups<-dplyr::select(trip_df_pr2, LAB) %>% pull()
trip_pr_trim <- raster::raster(m_trim)
#names<-c(min(trip_df_pr2$YEAR):max(trip_df_pr2$YEAR))
names <- as.character(1997:2025)
groups_trim <- groups[keep_rows]


nrows <- nrow(m_trim)
ncols <- ncol(m_trim)

y_at <- seq(from = (1/nrows)/ 2,
            to = 1 - (1/nrows)/ 2,
            by = 1/ nrows)

x_at <- seq(from = (1/ncols)/ 2,
            to = 1 - (1/ncols)/2,
            by = 1/ncols)


jpeg(paste0(Dirout,"Commercial_trip.jpeg"),res=300,height=4.8,width=6.9, units="in")
par(mar=c(0.1, 11.5, 2, 0), xpd=TRUE,oma=c(0,0,1.5,0))
brks<-c(0,1,2); cols<-c("lightblue","red")
plot(Trip,breaks=brks,col=cols,axes=F,legend=F,legend.shrink=0.1,xlab="",main="")
abline(h=c(seq(0,1,(1/dim(trip_df_pr2)[1]))),col="lightgray")
abline(v=c(seq(0,1,(1/length(names)))),col="lightgray")
axis(2,at=seq(((1/dim(trip_df_pr2)[1])/2),1,(1/dim(trip_df_pr2)[1])),labels=rev(groups),las=2,tck=0,cex.axis=0.5,font=1)
axis(3,at=seq(((1/length(names))/2),1,(1/length(names))),labels=names,las=2,tck=0,cex.axis=0.6,las=2,hadj=0.7)
dev.off()

Trip2<-raster(as.matrix(trip_yr_df2[,2:dim(trip_yr_df2)[2]]))
groups<-dplyr::select(trip_yr_df2, LAB) %>% pull()
names<-c(min(trip_yr_df$YEAR):max(trip_yr_df$YEAR))

jpeg(paste0(Dirout,"Commercial_trip_species.jpeg"),res=300,height=4.8,width=6.3, units="in")
par(mar=c(0.1, 6.5, 2, 0), xpd=TRUE,oma=c(0,0,1.5,0))
brks<-c(0,1,2); cols<-c("lightblue","red")
plot(Trip2,breaks=brks,col=cols,axes=F,legend=F,legend.shrink=0.1,xlab="",main="")
abline(h=c(seq(0,1,(1/dim(trip_yr_df2)[1]))),col="lightgray")
abline(v=c(seq(0,1,(1/length(names)))),col="lightgray")
axis(2,at=seq(((1/dim(trip_yr_df2)[1])/2),1,(1/dim(trip_yr_df2)[1])),labels=rev(groups),las=2,tck=0,cex.axis=0.3,font=1)
axis(3,at=seq(((1/length(names))/2),1,(1/length(names))),labels=names,las=2,tck=0,cex.axis=0.6,las=2,hadj=0.7)
dev.off()

# Plot
IFQ<-raster(as.matrix(IFQ_yr_df2[,2:dim(IFQ_yr_df2)[2]]))
groups<-dplyr::select(IFQ_yr_df2, LAB) %>% pull()
names<-c(min(IFQ_yr_df$YEAR):max(IFQ_yr_df$YEAR))

jpeg(paste0(Dirout,"Commercial_IFQ_species.jpeg"),res=300,height=4.8,width=6.1, units="in")
par(mar=c(0.1, 5.5, 2, 0), xpd=TRUE,oma=c(0,0,1.5,0))
brks<-c(0,1,2); cols<-c("lightblue","red")
plot(IFQ,breaks=brks,col=cols,axes=F,legend=F,legend.shrink=0.1,xlab="",main="")
abline(h=c(seq(0,1,(1/dim(IFQ_yr_df2)[1]))),col="lightgray")
abline(v=c(seq(0,1,(1/length(names)))),col="lightgray")
axis(2,at=seq(((1/dim(IFQ_yr_df2)[1])/2),1,(1/dim(IFQ_yr_df2)[1])),labels=rev(groups),las=2,tck=0,cex.axis=0.5,font=1)
axis(3,at=seq(((1/length(names))/2),1,(1/length(names))),labels=names,las=2,tck=0,cex.axis=0.6,las=2,hadj=0.7)
dev.off()

# Combine trip limits and IFQ into dataframe
trip_yr_df4<-as.data.frame(rbind(trip_yr_df3,
                                 as.numeric(c("SNAPPER, RED IFQ",rep(NA,15),rep(1,18))),
                                 as.numeric(c("GROUPER-TILEFISH IFQ",rep(NA,18),rep(1,15)))))
trip_yr_df4$LAB[(dim(trip_yr_df4)[1]-1):dim(trip_yr_df4)[1]]<-c("- IFQ","Grouper-Tilefish IFQ")
trip_yr_df4<-trip_yr_df4[c(1:11,19,12:18,20),] # Reorder by family/species (manually for now...)
Trip3<-raster(as.matrix(trip_yr_df4[,2:dim(trip_yr_df4)[2]]))
groups<-dplyr::select(trip_yr_df4, LAB) %>% pull()
names<-c(min(trip_yr_df$YEAR):max(trip_yr_df$YEAR))

jpeg(paste0(Dirout,"Commercial_trip_IFQ.jpeg"),res=300,height=4.2,width=6.3, units="in")
par(mar=c(0.1, 9.5, 2, 0), xpd=TRUE,oma=c(0,0,1.5,0))
brks<-c(0,1,2); cols<-c("lightblue","red")
plot(Trip3,breaks=brks,col=cols,axes=F,legend=F,legend.shrink=0.1,xlab="",main="")
abline(h=c(seq(0,1,(1/dim(trip_yr_df4)[1]))),col="lightgray")
abline(v=c(seq(0,1,(1/length(names)))),col="lightgray")
axis(2,at=seq(((1/dim(trip_yr_df4)[1])/2),1,(1/dim(trip_yr_df4)[1])),labels=rev(groups),las=2,tck=0,cex.axis=0.5,font=1)
axis(3,at=seq(((1/length(names))/2),1,(1/length(names))),labels=names,las=2,tck=0,cex.axis=0.6,las=2,hadj=0.7)
dev.off()

#################################################################
# Subset for Gulf Reef Fish commercial trip limits and IFQ
# Subset for Gulf Reef Fish size limits - Puerto Rico
mh_carib_sc_trip <- mh %>% filter(FMP %in% c("REEF FISH FISHERY OF PUERTO RICO AND THE U.S. VIRGIN ISLANDS", 
                                             #"FMP FOR THE EEZ AROUND ST. THOMAS AND ST. JOHN",
                                             "FMP FOR THE EEZ AROUND ST. CROIX",
                                             #"FMP FOR THE EEZ AROUND PUERTO RICO",
                                             "SPINY LOBSTER FISHERY OF PUERTO RICO AND THE U.S. VIRGIN ISLANDS",
                                             "QUEEN CONCH RESOURCES OF PUERTO RICO AND THE U.S. VIRGIN ISLANDS")) %>%
  # Remove recs that were never implemented
  filter(NEVER_IMPLEMENTED == 0, SECTOR_USE == 'COMMERCIAL',
         MANAGEMENT_TYPE_USE == 'TRIP LIMIT',
         ZONE_USE %in% c("ALL", "ST. CROIX MANAGEMENT AREA"))
mh_carib_sc_trip <- mh_carib_sc_trip %>% 
  mutate(END_DATE2 = case_when(FR_CITATION == "78 FR 56171" & COMMON_NAME_USE == "CONCH, QUEEN" ~ as.Date("2021-10-13"),
                               TRUE ~ END_DATE2))

# Confirm trip limits
unique(mh_carib_sc_trip$MANAGEMENT_TYPE_USE)

# Confirm only commercial
table(mh_carib_sc_trip$SECTOR_USE)

source(here("ODM-MH-Analysis_ready", "Closures", "func_expand_status.R"))

#############
# Trip limits
#############

trip_df_sc <- expand_status(mh_carib_sc_trip, "TRIP LIMIT")

# Collapse to annual value
trip_df_sc <- trip_df_sc %>%
  mutate(YEAR = year(date_sequence)) %>%
  dplyr::select(COMMON_NAME_USE, ZONE_USE, SUBSECTOR_USE, YEAR,
                VALUE, VALUE_UNITS, VALUE_TYPE, VALUE_RATE) %>%
  distinct() 

# Explore Zone 
table(trip_df_sc$ZONE_USE)
# All for all data

# Explore Subsector
table(trip_df_sc$SUBSECTOR_USE)
# All for all data

# Explore year - only include complete years 
table(trip_df_sc$YEAR)
trip_df_sc<-trip_df_sc %>% filter(YEAR <=termyr)
table(trip_df_sc$YEAR)

# Format for plot
trip_df_sc2 <- trip_df_sc %>%
  mutate(VALUE_UNITS = case_when(VALUE_UNITS == 'POUNDS' ~ 'lb',
                                 TRUE ~ VALUE_UNITS),
         VALUE_UNITS = case_when(VALUE_UNITS == 'CONCH' ~ 'conch',
                                 TRUE ~ VALUE_UNITS),
         VALUE_TYPE = case_when(VALUE_TYPE == 'GUTTED WEIGHT' ~ 'GW',
                                TRUE ~ VALUE_TYPE),
         VALUE_TYPE = case_when(VALUE_TYPE == 'ROUND (WHOLE) WEIGHT' ~ 'WW',
                                TRUE ~ VALUE_TYPE),
         VALUE_TYPE = case_when(VALUE_TYPE == 'COUNT' ~ '',
                                TRUE ~ VALUE_TYPE),
         VALUE_RATE = case_when(VALUE_RATE == 'PER VESSEL PER DAY' ~ 'per vessel per day',
                                VALUE_RATE == "PER PERSON PER DAY" ~ "per person per day",
                                TRUE ~ VALUE_RATE),
         VAL_LAB = paste(VALUE, VALUE_UNITS, VALUE_TYPE, VALUE_RATE),
         LAB = paste(str_to_title(sub('(.*)\\,\\s+(.*)','\\2 \\1',COMMON_NAME_USE)),"-",  VAL_LAB),
         #         LAB = paste(COMMON_NAME_USE,"-",  VAL_LAB),
         #         LAB = paste(COMMON_NAME_USE, "(Zone ", ZONE_USE, ")", VAL_LAB),
         n = 1) %>%
  dplyr::select(LAB, YEAR, n) %>%
  arrange(YEAR) %>%
  pivot_wider(names_from = YEAR, values_from = n) %>%
  arrange(LAB)

view(trip_df_sc2)
trip_df_sc2$LAB


# Save outputs before we clean up labels
saveRDS(trip_df_sc2,paste0(Dirout,"commercial_trip_limits.rds"))

# Remove extra space in label
trip_df_sc2$LAB[which(trip_df_sc2$LAB=="Queen Conch - 200 conch  per vessel per day")]<-"- 200 conch  per vessel per day"

###########################################################
library(tidyverse)

# 1) Get just the year columns (handles if LAB is col 1)
yrs <- names(trip_df_sc2)
yrs <- yrs[grepl("^\\d{4}$", yrs)]   # keep only "1985", "1986", ...
yrs <- yrs[as.integer(yrs) >= 1985 & as.integer(yrs) <= termyr]

# 2) Optional: drop empty rows (all NA across years)
keep_rows <- rowSums(!is.na(trip_df_sc2[, yrs, drop = FALSE])) > 0

plot_df_sc <- trip_df_sc2 %>%
  dplyr::filter(keep_rows) %>%
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
p_sc_trip <- ggplot(plot_df_sc, aes(x = YEAR, y = LAB, fill = present)) +
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
print(p_sc_trip)
ggsave(filename = paste0(Dirout, "Commercial_trip_limits_sc.jpeg"),
       plot = p_sc_trip, width = 6.5, height = 4.6, dpi = 300)
