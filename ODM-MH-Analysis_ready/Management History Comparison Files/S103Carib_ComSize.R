############################
#  Commercial Size Limits  #
############################

librarian::shelf(here, tidyverse, lubridate, openxlsx, neatRanges, splitstackshape, dplyr)

# Set output directory for plots
Dirout<-paste0(getwd(),"/Size limits/")
if(!file.exists(Dirout)) dir.create(Dirout)

# MH dataset 
mh <- readRDS(here("ODM-MH-Data_log", "data", "results", "MH_DL_2025Nov18.RDS"))

# Set end year for plots
termyr<-2025

#######################
# Minimum Size limits #
#######################

# Subset for Gulf Reef Fish size limits - Puerto Rico
mh_carib_pr <- mh %>% filter(FMP %in% c("REEF FISH FISHERY OF PUERTO RICO AND THE U.S. VIRGIN ISLANDS", 
                                     #"FMP FOR THE EEZ AROUND ST. THOMAS AND ST. JOHN",
                                     #"FMP FOR THE EEZ AROUND ST. CROIX",
                                     "FMP FOR THE EEZ AROUND PUERTO RICO",
                                     "SPINY LOBSTER FISHERY OF PUERTO RICO AND THE U.S. VIRGIN ISLANDS",
                                     "QUEEN CONCH RESOURCES OF PUERTO RICO AND THE U.S. VIRGIN ISLANDS")) %>%
  # Remove recs that were never implemented
  filter(NEVER_IMPLEMENTED == 0, SECTOR_USE == 'COMMERCIAL',
         MANAGEMENT_CATEGORY == 'SELECTIVITY CONTROLS',
         ZONE_USE %in% c("ALL", "PUERTO RICO MANAGEMENT AREA"))
mh_carib_pr <- mh_carib_pr %>% 
mutate(END_DATE2 = case_when(FR_CITATION == "49 FR 50049" & COMMON_NAME_USE == "LOBSTER, CARIBBEAN SPINY" ~ as.Date("2021-10-12"),
                             FR_CITATION == "55 FR 46214" & COMMON_NAME_USE == "SNAPPER, YELLOWTAIL" & VALUE == 12 ~ as.Date("2021-10-12"),
                             FR_CITATION == "50 FR 34850" & COMMON_NAME_USE == "GROUPER, NASSAU" & VALUE == 17 ~ as.Date("1990-11-29"),
                             FR_CITATION == "81 FR 29166" & COMMON_NAME_USE == "CONCH, QUEEN" ~ as.Date("2022-10-13"),
                               TRUE ~ END_DATE2)) %>%
  filter(!(FR_CITATION == "50 FR 34850" & START_YEAR >= "1991"))

# Confirm size limits
unique(mh_carib_pr$MANAGEMENT_TYPE_USE)

# Confirm only commercial
table(mh_carib_pr$SECTOR_USE)

source(here("ODM-MH-Analysis_ready", "Closures", "func_expand_status.R"))

# Min size limits
min_df_pr <- expand_status(mh_carib_pr, "MINIMUM SIZE LIMIT")

# Collapse to annual value
min_yr_df_pr <- min_df_pr %>%
  mutate(YEAR = year(date_sequence)) %>%
  select(COMMON_NAME_USE, ZONE_USE, SUBSECTOR_USE, YEAR,
         VALUE, VALUE_UNITS, VALUE_TYPE, VALUE_RATE) %>%
  distinct() 

# Explore Zone 
table(min_yr_df_pr$ZONE_USE)

# Explore Subsector
table(min_yr_df_pr$SUBSECTOR_USE)
# All for all data

# Explore year - only include complete years 
table(min_yr_df_pr$YEAR)
min_yr_df_pr<-min_yr_df_pr %>% filter(YEAR <=termyr)
table(min_yr_df_pr$YEAR)

# Format for plot
min_yr_df2_pr <- min_yr_df_pr %>%
  mutate(VALUE_UNITS = case_when(VALUE_UNITS == 'INCHES' ~ 'in',
                                 TRUE ~ VALUE_UNITS),
         VALUE_TYPE = case_when(VALUE_TYPE == 'FORK LENGTH' ~ 'FL',
                                TRUE ~ VALUE_TYPE),
         VALUE_TYPE = case_when(VALUE_TYPE == 'TOTAL LENGTH' ~ 'TL',
                                TRUE ~ VALUE_TYPE),
         VALUE_TYPE = case_when(VALUE_TYPE == "CARAPACE LENGTH" ~ "Carapace Length",
                                TRUE ~ VALUE_TYPE),
         VAL_LAB = paste(VALUE, VALUE_UNITS, VALUE_TYPE),
         LAB = paste(str_to_title(sub('(.*)\\,\\s+(.*)','\\2 \\1',COMMON_NAME_USE)),"-",  VAL_LAB),
         #         LAB = paste(COMMON_NAME_USE,"-",  VAL_LAB),
         #         LAB = paste(COMMON_NAME_USE, "(Zone ", ZONE_USE, ")", VAL_LAB),
         n = 1) %>%
  dplyr::select(LAB, YEAR, n) %>%
  arrange(YEAR) %>%
  pivot_wider(names_from = YEAR, values_from = n) %>%
  arrange(LAB) 


# ######################
# # Prohibited Species #
# ######################
# 
# # Subset for Gulf Reef Fish size limits
# mh_carib2 <- mh %>% filter(FMP %in% c("REEF FISH FISHERY OF PUERTO RICO AND THE U.S. VIRGIN ISLANDS", 
 #                                    "FMP FOR THE EEZ AROUND ST. THOMAS AND ST. JOHN",
  #                                   "FMP FOR THE EEZ AROUND ST. CROIX",
   #                                  "FMP FOR THE EEZ AROUND PUERTO RICO",
    #                                 "SPINY LOBSTER FISHERY OF PUERTO RICO AND THE U.S. VIRGIN ISLANDS",
     #                                "QUEEN CONCH RESOURCES OF PUERTO RICO AND THE U.S. VIRGIN ISLANDS")) %>%
  # Remove recs that were never implemented
   #filter(NEVER_IMPLEMENTED == 0, SECTOR_USE == 'COMMERCIAL',
    #      MANAGEMENT_TYPE_USE == 'PROHIBITED SPECIES')
# 
# # Confirm prohibited
 #unique(mh_carib2$MANAGEMENT_TYPE_USE)
# 
# # Confirm only commercial
 #table(mh_carib2$SECTOR_USE)
# 
# # Min size limits
 #Prohib_df <- expand_status(mh_carib2, "PROHIBITED SPECIES")
# 
# # Collapse to annual value
 #Prohib_yr_df <- Prohib_df %>%
  # mutate(YEAR = year(date_sequence)) %>%
  # select(COMMON_NAME_USE, ZONE_USE, SUBSECTOR_USE, YEAR,
   #       VALUE, VALUE_UNITS, VALUE_TYPE, VALUE_RATE) %>%
   #distinct() 
# 
# # Explore Zone 
 #table(Prohib_yr_df$ZONE_USE)
# # All for all data
# 
# # Explore Subsector
 #table(Prohib_yr_df$SUBSECTOR_USE)
# # All for all data
# 
# # Explore year - only include complete years 
 #table(Prohib_yr_df$YEAR)
 #Prohib_yr_df<-Prohib_yr_df %>% filter(YEAR <=termyr)
 #table(Prohib_yr_df$YEAR)
# 
# # Format for plot
 #Prohib_yr_df2 <- Prohib_yr_df %>%
  # mutate(LAB = str_to_title(sub('(.*)\\,\\s+(.*)','\\2 \\1',COMMON_NAME_USE)),
   #       n = 1) %>%
  # dplyr::select(LAB, YEAR, n) %>%
   #arrange(YEAR) %>%
  # pivot_wider(names_from = YEAR, values_from = n) %>%
   #arrange(LAB) 

# Want to highlight years where species prohibited or removed from FMP
# Goliath prohibited 1990+
#min_yr_df2[min_yr_df2$LAB=="Goliath Grouper - 50 in TL", which(colnames(min_yr_df2)=="1990")]<-2
#min_yr_df2[min_yr_df2$LAB=="Goliath Grouper - 50 in TL", which(colnames(min_yr_df2)=="1991"):dim(min_yr_df2)[2]]<-NA
# Nassau prohibited 1997+
#min_yr_df2[min_yr_df2$LAB=="Nassau Grouper - 20 in TL", which(colnames(min_yr_df2)=="1997")]<-2
#min_yr_df2[min_yr_df2$LAB=="Nassau Grouper - 20 in TL", which(colnames(min_yr_df2)=="1998"):dim(min_yr_df2)[2]]<-NA
# Schoolmaster removed from FMP in 2012 
#min_yr_df2[min_yr_df2$LAB=="Schoolmaster - 12 in TL", which(colnames(min_yr_df2)=="2012")]<-2
# Dog snapper removed from FMP in 2012 
#min_yr_df2[min_yr_df2$LAB=="Dog Snapper - 12 in TL",which(colnames(min_yr_df2)=="2012")]<-2
# Mahogany snapper, Black Sea Bass removed from FMP in 2012 
#min_yr_df2[min_yr_df2$LAB=="Mahogany Snapper - 12 in TL",which(colnames(min_yr_df2)=="2012")]<-2
# Black Sea Bass removed from FMP in 1998 
#min_yr_df2[min_yr_df2$LAB=="Black Sea Bass - 8 in TL",which(colnames(min_yr_df2)=="1998")]<-2

# Need to add in Max Size Limit for banded rudderfish and lesser amberjack
#subset(mh_grf,MANAGEMENT_TYPE_USE=="MAXIMUM SIZE LIMIT")
#min_yr_df2$LAB[which(min_yr_df2$LAB=="Banded Rudderfish - 14 in FL")]<-"Banded Rudderfish - 14 in FL (22 Max)"
#min_yr_df2$LAB[which(min_yr_df2$LAB=="Lesser Amberjack - 14 in FL")]<-"Lesser Amberjack - 14 in FL (22 Max)"

# Rename Scamp 
#min_yr_df2$LAB[which(min_yr_df2$LAB=="Scamp - 16 in TL")]<-"Scamp Grouper - 16 in TL"

# Reorder by family (manually for now...)
dim(min_yr_df2_pr) #15 x 42
min_yr_df2_pr<-min_yr_df2_pr[c(1:10, 14:15, 11:13),]
dim(min_yr_df2_pr) #37 x 42

# Save outputs before we clean up labels
saveRDS(min_yr_df2_pr,paste0(Dirout,"commercial_pr_size_limits.rds"))

# Remove species name in consecutive rows
min_yr_df2_pr$LAB[which(min_yr_df2_pr$LAB=="Nassau Grouper - 13 in TL")]<-"- 13 in TL"
min_yr_df2_pr$LAB[which(min_yr_df2_pr$LAB=="Nassau Grouper - 14 in TL")]<-"- 14 in TL"
min_yr_df2_pr$LAB[which(min_yr_df2_pr$LAB=="Nassau Grouper - 15 in TL")]<-"- 15 in TL"
min_yr_df2_pr$LAB[which(min_yr_df2_pr$LAB=="Nassau Grouper - 16 in TL")]<-"- 16 in TL"
min_yr_df2_pr$LAB[which(min_yr_df2_pr$LAB=="Nassau Grouper - 17 in TL")]<-"- 17 in FL"
min_yr_df2_pr$LAB[which(min_yr_df2_pr$LAB=="Yellowtail Snapper - 9 in TL")]<-"- 9 in TL"
min_yr_df2_pr$LAB[which(min_yr_df2_pr$LAB=="Yellowtail Snapper - 10 in TL")]<-"- 10 in TL"
min_yr_df2_pr$LAB[which(min_yr_df2_pr$LAB=="Yellowtail Snapper - 11 in TL")]<-"- 11 in TL"
min_yr_df2_pr$LAB[which(min_yr_df2_pr$LAB=="Yellowtail Snapper - 12 in TL")]<-"- 12 in TL"


# Add asterisk to indicate size limits that are consist between com/rec
#min_yr_df2$LAB[which(min_yr_df2$LAB=="Banded Rudderfish - 14 in FL (22 Max)")]<-"*Banded Rudderfish - 14 in FL (22 Max)"
#min_yr_df2$LAB[which(min_yr_df2$LAB=="Lesser Amberjack - 14 in FL (22 Max)")]<-"*Lesser Amberjack - 14 in FL (22 Max)"
#min_yr_df2$LAB[which(min_yr_df2$LAB=="Black Sea Bass - 8 in TL")]<-"*Black Sea Bass - 8 in TL"
#min_yr_df2$LAB[which(min_yr_df2$LAB=="Goliath Grouper - 50 in TL")]<-"*Goliath Grouper - 50 in TL"
#min_yr_df2$LAB[which(min_yr_df2$LAB=="Nassau Grouper - 20 in TL")]<-"*Nassau Grouper - 20 in TL"
#min_yr_df2$LAB[which(min_yr_df2$LAB=="Scamp Grouper - 16 in TL")]<-"*Scamp Grouper - 16 in TL"
#min_yr_df2$LAB[which(min_yr_df2$LAB=="Yellowfin Grouper - 20 in TL")]<-"*Yellowfin Grouper - 20 in TL"
#min_yr_df2$LAB[which(min_yr_df2$LAB=="Hogfish - 12 in FL")]<-"*Hogfish - 12 in FL"
#min_yr_df2$LAB[which(min_yr_df2$LAB=="Schoolmaster - 12 in TL")]<-"*Schoolmaster - 12 in TL"
#min_yr_df2$LAB[which(min_yr_df2$LAB=="Cubera Snapper - 12 in TL")]<-"*Cubera Snapper - 12 in TL"
#min_yr_df2$LAB[which(min_yr_df2$LAB=="Dog Snapper - 12 in TL")]<-"*Dog Snapper - 12 in TL"
#min_yr_df2$LAB[which(min_yr_df2$LAB=="Gray Snapper - 12 in TL")]<-"*Gray Snapper - 12 in TL"
#min_yr_df2$LAB[which(min_yr_df2$LAB=="Lane Snapper - 8 in TL")]<-"*Lane Snapper - 8 in TL"
#min_yr_df2$LAB[which(min_yr_df2$LAB=="Mahogany Snapper - 12 in TL")]<-"*Mahogany Snapper - 12 in TL"
#min_yr_df2$LAB[which(min_yr_df2$LAB=="Mutton Snapper - 12 in TL")]<-"*Mutton Snapper - 12 in TL"
#min_yr_df2$LAB[which(min_yr_df2$LAB=="Vermilion Snapper - 8 in TL")]<-"*Vermilion Snapper - 8 in TL"
#min_yr_df2$LAB[which(min_yr_df2$LAB=="Yellowtail Snapper - 12 in TL")]<-"*Yellowtail Snapper - 12 in TL"

library(tidyverse)

# 1) Get just the year columns (handles if LAB is col 1)
yrs <- names(min_yr_df2_pr)
yrs <- yrs[grepl("^\\d{4}$", yrs)]   # keep only "1985", "1986", ...
yrs <- yrs[as.integer(yrs) >= 1985 & as.integer(yrs) <= termyr]

# 2) Optional: drop empty rows (all NA across years)
keep_rows <- rowSums(!is.na(min_yr_df2_pr[, yrs, drop = FALSE])) > 0

plot_df_pr <- min_yr_df2_pr %>%
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
p_pr <- ggplot(plot_df_pr, aes(x = YEAR, y = LAB, fill = present)) +
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
print(p_pr)
ggsave(filename = paste0(Dirout, "Commercial_size_limits_PR.jpeg"),
       plot = p_pr, width = 6.5, height = 4.6, dpi = 300)


########################################################
# Plot size limits over time
#library(raster)
#Note: importing this package masks select from dplyr...

#yrs <- as.character(1985:2025)
#yrs <- names(min_yr_df2_pr)
#yrs <- yrs[grepl("^\\d{4}$", yrs)]
#yrs <- yrs[as.integer(yrs) >= 1985 & as.integer(yrs) <= 2025]

#MinSize_pr<-raster(as.matrix(min_yr_df2_pr[,2:dim(min_yr_df2_pr)[2]]))
#m <- as.matrix(min_yr_df2_pr[, -1])
#keep_rows <- which(rowSums(!is.na(m))>0)
#m_trim <- m[keep_rows, , drop = FALSE]
#groups_trim <- groups[keep_rows]


#groups<-dplyr::select(min_yr_df2_pr, LAB) %>% pull()
#m_trim <- m[keep_rows, , drop = FALSE]
#minsize_pr_trim <- raster::raster(m_trim)
#names<-c(min(min_yr_df2_pr$):max(min_yr_df2_pr$YEAR))
#names <- as.character(1985:2025)

#nrows <- nrow(m_trim)
#ncols <- ncol(m_trim)

#y_at <- seq(from = (1/nrows)/ 2,
 #           to = 1 - (1/nrows)/ 2,
  #          by = 1/ nrows)

#x_at <- seq(from = (1/ncols)/ 2,
 #           to = 1 - (1/ncols)/2,
  #          by = 1/ncols)

#jpeg(paste0(Dirout,"Commercial_size_limits.jpeg"),res=300,height=4.6,width=6.5, units="in")
#par(mar=c(0.1, 12, 2, 0), xpd=TRUE,oma=c(0,0,1.5,0))
#brks<-c(0,1,2); cols<-c("lightblue","red")
#plot(MinSize_pr,breaks=brks,col=cols,axes=F,legend=F,legend.shrink=0.1,xlab="",main="")
#abline(h=c(seq(0,1,(1/dim(min_yr_df2_pr)[1]))),col="lightgray")
#abline(v=c(seq(0,1,(1/length(names)))),col="lightgray")
#axis(2,at=seq(((1/dim(min_yr_df2_pr)[1])/2),1,(1/dim(min_yr_df2_pr)[1])),labels=rev(groups),las=2,tck=0,cex.axis=0.5,font=1)
#axis(3,at=seq(((1/length(names))/2),1,(1/length(names))),labels=names,las=2,tck=0,cex.axis=0.5,las=2,hadj=0.7)
#axis(2, at = y_at, labels = rev(groups_trim), las = 2, tck = 0, cex.axis = 0.5)
#axis(3, at = x_at, labels = names, las = 2, tck = 0, cex.axis = 0.5, hadj = 0.7)
#dev.off()

##### ST. Thomas/St. John
# Subset for Gulf Reef Fish size limits - St. Thomas and ST. John.
mh_carib_st <- mh %>% filter(FMP %in% c("REEF FISH FISHERY OF PUERTO RICO AND THE U.S. VIRGIN ISLANDS", 
                                        "FMP FOR THE EEZ AROUND ST. THOMAS AND ST. JOHN",
                                        #"FMP FOR THE EEZ AROUND ST. CROIX",
                                        #"FMP FOR THE EEZ AROUND PUERTO RICO",
                                        "SPINY LOBSTER FISHERY OF PUERTO RICO AND THE U.S. VIRGIN ISLANDS",
                                        "QUEEN CONCH RESOURCES OF PUERTO RICO AND THE U.S. VIRGIN ISLANDS")) %>%
  # Remove recs that were never implemented
  filter(NEVER_IMPLEMENTED == 0, SECTOR_USE == 'COMMERCIAL',
         MANAGEMENT_CATEGORY == 'SELECTIVITY CONTROLS',
         ZONE_USE %in% c("ALL", "ST. THOMAS/ST. JOHN MANAGEMENT AREA"))
mh_carib_st <- mh_carib_st %>% 
  mutate(END_DATE2 = case_when(FR_CITATION == "49 FR 50049" & COMMON_NAME_USE == "LOBSTER, CARIBBEAN SPINY" ~ as.Date("2021-10-12"),
                               FR_CITATION == "55 FR 46214" & COMMON_NAME_USE == "SNAPPER, YELLOWTAIL" & VALUE == 12 ~ as.Date("2021-10-12"),
                               FR_CITATION == "50 FR 34850" & COMMON_NAME_USE == "GROUPER, NASSAU" & VALUE == 17 ~ as.Date("1990-11-29"),
                               FR_CITATION == "81 FR 29166" & COMMON_NAME_USE == "CONCH, QUEEN" ~ as.Date("2022-10-13"),
                               TRUE ~ END_DATE2))

# Confirm size limits
unique(mh_carib_st$MANAGEMENT_TYPE_USE)

# Confirm only commercial
table(mh_carib_st$SECTOR_USE)

source(here("ODM-MH-Analysis_ready", "Closures", "func_expand_status.R"))

# Min size limits
min_df_st <- expand_status(mh_carib_st, "MINIMUM SIZE LIMIT")

# Collapse to annual value
min_yr_df_st <- min_df_st %>%
  mutate(YEAR = year(date_sequence)) %>%
  select(COMMON_NAME_USE, ZONE_USE, SUBSECTOR_USE, YEAR,
         VALUE, VALUE_UNITS, VALUE_TYPE, VALUE_RATE) %>%
  distinct() 

# Explore Zone 
table(min_yr_df_st$ZONE_USE)

# Explore Subsector
table(min_yr_df_st$SUBSECTOR_USE)
# All for all data

# Explore year - only include complete years 
table(min_yr_df_st$YEAR)
min_yr_df_st<-min_yr_df_st %>% filter(YEAR <=termyr)
table(min_yr_df_st$YEAR)

# Format for plot
min_yr_df2_st <- min_yr_df_st %>%
  mutate(VALUE_UNITS = case_when(VALUE_UNITS == 'INCHES' ~ 'in',
                                 TRUE ~ VALUE_UNITS),
         VALUE_TYPE = case_when(VALUE_TYPE == 'FORK LENGTH' ~ 'FL',
                                TRUE ~ VALUE_TYPE),
         VALUE_TYPE = case_when(VALUE_TYPE == 'TOTAL LENGTH' ~ 'TL',
                                TRUE ~ VALUE_TYPE),
         VALUE_TYPE = case_when(VALUE_TYPE == "CARAPACE LENGTH" ~ "Carapace Length",
                                TRUE ~ VALUE_TYPE),
         VAL_LAB = paste(VALUE, VALUE_UNITS, VALUE_TYPE),
         LAB = paste(str_to_title(sub('(.*)\\,\\s+(.*)','\\2 \\1',COMMON_NAME_USE)),"-",  VAL_LAB),
         #         LAB = paste(COMMON_NAME_USE,"-",  VAL_LAB),
         #         LAB = paste(COMMON_NAME_USE, "(Zone ", ZONE_USE, ")", VAL_LAB),
         n = 1) %>%
  dplyr::select(LAB, YEAR, n) %>%
  arrange(YEAR) %>%
  pivot_wider(names_from = YEAR, values_from = n) %>%
  arrange(LAB) 


# ######################
# # Prohibited Species #
# ######################
# 
# # Subset for Gulf Reef Fish size limits
# mh_carib2 <- mh %>% filter(FMP %in% c("REEF FISH FISHERY OF PUERTO RICO AND THE U.S. VIRGIN ISLANDS", 
#                                    "FMP FOR THE EEZ AROUND ST. THOMAS AND ST. JOHN",
#                                   "FMP FOR THE EEZ AROUND ST. CROIX",
#                                  "FMP FOR THE EEZ AROUND PUERTO RICO",
#                                 "SPINY LOBSTER FISHERY OF PUERTO RICO AND THE U.S. VIRGIN ISLANDS",
#                                "QUEEN CONCH RESOURCES OF PUERTO RICO AND THE U.S. VIRGIN ISLANDS")) %>%
# Remove recs that were never implemented
#filter(NEVER_IMPLEMENTED == 0, SECTOR_USE == 'COMMERCIAL',
#      MANAGEMENT_TYPE_USE == 'PROHIBITED SPECIES')
# 
# # Confirm prohibited
#unique(mh_carib2$MANAGEMENT_TYPE_USE)
# 
# # Confirm only commercial
#table(mh_carib2$SECTOR_USE)
# 
# # Min size limits
#Prohib_df <- expand_status(mh_carib2, "PROHIBITED SPECIES")
# 
# # Collapse to annual value
#Prohib_yr_df <- Prohib_df %>%
# mutate(YEAR = year(date_sequence)) %>%
# select(COMMON_NAME_USE, ZONE_USE, SUBSECTOR_USE, YEAR,
#       VALUE, VALUE_UNITS, VALUE_TYPE, VALUE_RATE) %>%
#distinct() 
# 
# # Explore Zone 
#table(Prohib_yr_df$ZONE_USE)
# # All for all data
# 
# # Explore Subsector
#table(Prohib_yr_df$SUBSECTOR_USE)
# # All for all data
# 
# # Explore year - only include complete years 
#table(Prohib_yr_df$YEAR)
#Prohib_yr_df<-Prohib_yr_df %>% filter(YEAR <=termyr)
#table(Prohib_yr_df$YEAR)
# 
# # Format for plot
#Prohib_yr_df2 <- Prohib_yr_df %>%
# mutate(LAB = str_to_title(sub('(.*)\\,\\s+(.*)','\\2 \\1',COMMON_NAME_USE)),
#       n = 1) %>%
# dplyr::select(LAB, YEAR, n) %>%
#arrange(YEAR) %>%
# pivot_wider(names_from = YEAR, values_from = n) %>%
#arrange(LAB) 

# Want to highlight years where species prohibited or removed from FMP
# Goliath prohibited 1990+
#min_yr_df2[min_yr_df2$LAB=="Goliath Grouper - 50 in TL", which(colnames(min_yr_df2)=="1990")]<-2
#min_yr_df2[min_yr_df2$LAB=="Goliath Grouper - 50 in TL", which(colnames(min_yr_df2)=="1991"):dim(min_yr_df2)[2]]<-NA
# Nassau prohibited 1997+
#min_yr_df2[min_yr_df2$LAB=="Nassau Grouper - 20 in TL", which(colnames(min_yr_df2)=="1997")]<-2
#min_yr_df2[min_yr_df2$LAB=="Nassau Grouper - 20 in TL", which(colnames(min_yr_df2)=="1998"):dim(min_yr_df2)[2]]<-NA
# Schoolmaster removed from FMP in 2012 
#min_yr_df2[min_yr_df2$LAB=="Schoolmaster - 12 in TL", which(colnames(min_yr_df2)=="2012")]<-2
# Dog snapper removed from FMP in 2012 
#min_yr_df2[min_yr_df2$LAB=="Dog Snapper - 12 in TL",which(colnames(min_yr_df2)=="2012")]<-2
# Mahogany snapper, Black Sea Bass removed from FMP in 2012 
#min_yr_df2[min_yr_df2$LAB=="Mahogany Snapper - 12 in TL",which(colnames(min_yr_df2)=="2012")]<-2
# Black Sea Bass removed from FMP in 1998 
#min_yr_df2[min_yr_df2$LAB=="Black Sea Bass - 8 in TL",which(colnames(min_yr_df2)=="1998")]<-2

# Need to add in Max Size Limit for banded rudderfish and lesser amberjack
#subset(mh_grf,MANAGEMENT_TYPE_USE=="MAXIMUM SIZE LIMIT")
#min_yr_df2$LAB[which(min_yr_df2$LAB=="Banded Rudderfish - 14 in FL")]<-"Banded Rudderfish - 14 in FL (22 Max)"
#min_yr_df2$LAB[which(min_yr_df2$LAB=="Lesser Amberjack - 14 in FL")]<-"Lesser Amberjack - 14 in FL (22 Max)"

# Rename Scamp 
#min_yr_df2$LAB[which(min_yr_df2$LAB=="Scamp - 16 in TL")]<-"Scamp Grouper - 16 in TL"

# Reorder by family (manually for now...)
dim(min_yr_df2_st) #22 x 42
min_yr_df2_st<-min_yr_df2_st[c(1:17, 21:22, 18:20),]
dim(min_yr_df2_st) #37 x 42

# Save outputs before we clean up labels
saveRDS(min_yr_df2_st,paste0(Dirout,"commercial_pr_size_limits.rds"))

# Remove species name in consecutive rows
min_yr_df2_st$LAB[which(min_yr_df2_st$LAB=="Nassau Grouper - 13 in TL")]<-"- 13 in TL"
min_yr_df2_st$LAB[which(min_yr_df2_st$LAB=="Nassau Grouper - 14 in TL")]<-"- 14 in TL"
min_yr_df2_st$LAB[which(min_yr_df2_st$LAB=="Nassau Grouper - 15 in TL")]<-"- 15 in TL"
min_yr_df2_st$LAB[which(min_yr_df2_st$LAB=="Nassau Grouper - 16 in TL")]<-"- 16 in TL"
min_yr_df2_st$LAB[which(min_yr_df2_st$LAB=="Nassau Grouper - 17 in TL")]<-"- 17 in FL"
min_yr_df2_st$LAB[which(min_yr_df2_st$LAB=="Nassau Grouper - 18 in TL")]<-"- 18 in TL"
min_yr_df2_st$LAB[which(min_yr_df2_st$LAB=="Nassau Grouper - 19 in TL")]<-"- 19 in TL"
min_yr_df2_st$LAB[which(min_yr_df2_st$LAB=="Nassau Grouper - 20 in TL")]<-"- 20 in TL"
min_yr_df2_st$LAB[which(min_yr_df2_st$LAB=="Nassau Grouper - 21 in TL")]<-"- 21 in TL"
min_yr_df2_st$LAB[which(min_yr_df2_st$LAB=="Nassau Grouper - 22 in TL")]<-"- 22 in TL"
min_yr_df2_st$LAB[which(min_yr_df2_st$LAB=="Nassau Grouper - 23 in TL")]<-"- 23 in TL"
min_yr_df2_st$LAB[which(min_yr_df2_st$LAB=="Nassau Grouper - 24 in TL")]<-"- 24 in TL"
min_yr_df2_st$LAB[which(min_yr_df2_st$LAB=="Yellowtail Snapper - 9 in TL")]<-"- 9 in TL"
min_yr_df2_st$LAB[which(min_yr_df2_st$LAB=="Yellowtail Snapper - 10 in TL")]<-"- 10 in TL"
min_yr_df2_st$LAB[which(min_yr_df2_st$LAB=="Yellowtail Snapper - 11 in TL")]<-"- 11 in TL"
min_yr_df2_st$LAB[which(min_yr_df2_st$LAB=="Yellowtail Snapper - 12 in TL")]<-"- 12 in TL"


# Add asterisk to indicate size limits that are consist between com/rec
min_yr_df2$LAB[which(min_yr_df2$LAB=="Banded Rudderfish - 14 in FL (22 Max)")]<-"*Banded Rudderfish - 14 in FL (22 Max)"
min_yr_df2$LAB[which(min_yr_df2$LAB=="Lesser Amberjack - 14 in FL (22 Max)")]<-"*Lesser Amberjack - 14 in FL (22 Max)"
min_yr_df2$LAB[which(min_yr_df2$LAB=="Black Sea Bass - 8 in TL")]<-"*Black Sea Bass - 8 in TL"
min_yr_df2$LAB[which(min_yr_df2$LAB=="Goliath Grouper - 50 in TL")]<-"*Goliath Grouper - 50 in TL"
min_yr_df2$LAB[which(min_yr_df2$LAB=="Nassau Grouper - 20 in TL")]<-"*Nassau Grouper - 20 in TL"
min_yr_df2$LAB[which(min_yr_df2$LAB=="Scamp Grouper - 16 in TL")]<-"*Scamp Grouper - 16 in TL"
min_yr_df2$LAB[which(min_yr_df2$LAB=="Yellowfin Grouper - 20 in TL")]<-"*Yellowfin Grouper - 20 in TL"
min_yr_df2$LAB[which(min_yr_df2$LAB=="Hogfish - 12 in FL")]<-"*Hogfish - 12 in FL"
min_yr_df2$LAB[which(min_yr_df2$LAB=="Schoolmaster - 12 in TL")]<-"*Schoolmaster - 12 in TL"
min_yr_df2$LAB[which(min_yr_df2$LAB=="Cubera Snapper - 12 in TL")]<-"*Cubera Snapper - 12 in TL"
min_yr_df2$LAB[which(min_yr_df2$LAB=="Dog Snapper - 12 in TL")]<-"*Dog Snapper - 12 in TL"
min_yr_df2$LAB[which(min_yr_df2$LAB=="Gray Snapper - 12 in TL")]<-"*Gray Snapper - 12 in TL"
min_yr_df2$LAB[which(min_yr_df2$LAB=="Lane Snapper - 8 in TL")]<-"*Lane Snapper - 8 in TL"
min_yr_df2$LAB[which(min_yr_df2$LAB=="Mahogany Snapper - 12 in TL")]<-"*Mahogany Snapper - 12 in TL"
min_yr_df2$LAB[which(min_yr_df2$LAB=="Mutton Snapper - 12 in TL")]<-"*Mutton Snapper - 12 in TL"
min_yr_df2$LAB[which(min_yr_df2$LAB=="Vermilion Snapper - 8 in TL")]<-"*Vermilion Snapper - 8 in TL"
min_yr_df2$LAB[which(min_yr_df2$LAB=="Yellowtail Snapper - 12 in TL")]<-"*Yellowtail Snapper - 12 in TL"

# Plot size limits over time
library(raster)
#Note: importing this package masks select from dplyr...

yrs <- as.character(1985:2025)
yrs <- names(min_yr_df2_st)
yrs <- yrs[grepl("^\\d{4}$", yrs)]
yrs <- yrs[as.integer(yrs) >= 1985 & as.integer(yrs) <= 2025]

MinSize_st<-raster(as.matrix(min_yr_df2_st[,2:dim(min_yr_df2_st)[2]]))
groups<-dplyr::select(min_yr_df2_st, LAB) %>% pull()
#names<-c(min(min_yr_df2_pr$):max(min_yr_df2_pr$YEAR))
names <- as.character(1985:2025)

nrows <- nrow(min_yr_df2_st)
ncols <- ncol(min_yr_df2_st) - 1

y_at <- seq(from = (1/nrows)/ 2,
            to = 1 - (1/nrows)/ 2,
            by = 1/ nrows)

x_at <- seq(from = (1/ncols)/ 2,
            to = 1 - (1/ncols)/2,
            by = 1/ncols)

jpeg(paste0(Dirout,"Commercial_size_limitsst.jpeg"),res=300,height=4.6,width=6.5, units="in")
par(mar=c(0.1, 12, 2, 0), xpd=TRUE,oma=c(0,0,1.5,0))
brks<-c(0,1,2); cols<-c("lightblue","red")
plot(MinSize_st,breaks=brks,col=cols,axes=F,legend=F,legend.shrink=0.1,xlab="",main="")
abline(h=c(seq(0,1,(1/dim(min_yr_df2_st)[1]))),col="lightgray")
abline(v=c(seq(0,1,(1/length(names)))),col="lightgray")
#axis(2,at=seq(((1/dim(min_yr_df2_pr)[1])/2),1,(1/dim(min_yr_df2_pr)[1])),labels=rev(groups),las=2,tck=0,cex.axis=0.5,font=1)
#axis(3,at=seq(((1/length(names))/2),1,(1/length(names))),labels=names,las=2,tck=0,cex.axis=0.5,las=2,hadj=0.7)
axis(2, at = y_at, labels = rev(groups), las = 2, tck = 0, cex.axis = 0.5)
axis(3, at = x_at, labels = names, las = 2, tck = 0, cex.axis = 0.5, hadj = 0.7)
dev.off()


##########################
# St. Croix
# Subset for Gulf Reef Fish size limits - Puerto Rico
mh_carib_sc <- mh %>% filter(FMP %in% c("REEF FISH FISHERY OF PUERTO RICO AND THE U.S. VIRGIN ISLANDS", 
                                        #"FMP FOR THE EEZ AROUND ST. THOMAS AND ST. JOHN",
                                        "FMP FOR THE EEZ AROUND ST. CROIX",
                                        #"FMP FOR THE EEZ AROUND PUERTO RICO",
                                        "SPINY LOBSTER FISHERY OF PUERTO RICO AND THE U.S. VIRGIN ISLANDS",
                                        "QUEEN CONCH RESOURCES OF PUERTO RICO AND THE U.S. VIRGIN ISLANDS")) %>%
  # Remove recs that were never implemented
  filter(NEVER_IMPLEMENTED == 0, SECTOR_USE == 'COMMERCIAL',
         MANAGEMENT_CATEGORY == 'SELECTIVITY CONTROLS',
         ZONE_USE %in% c("ALL", "ST. CROIX MANAGEMENT AREA"))
mh_carib_sc <- mh_carib_sc %>% 
  mutate(END_DATE2 = case_when(FR_CITATION == "49 FR 50049" & COMMON_NAME_USE == "LOBSTER, CARIBBEAN SPINY" ~ as.Date("2021-10-12"),
                               FR_CITATION == "55 FR 46214" & COMMON_NAME_USE == "SNAPPER, YELLOWTAIL" & VALUE == 12 ~ as.Date("2021-10-12"),
                               FR_CITATION == "50 FR 34850" & COMMON_NAME_USE == "GROUPER, NASSAU" & VALUE == 17 ~ as.Date("1990-11-29"),
                               FR_CITATION == "81 FR 29166" & COMMON_NAME_USE == "CONCH, QUEEN" ~ as.Date("2021-10-13"),
                               FR_CITATION == "78 FR 45894" ~ as.Date("2022-10-13"), 
                               TRUE ~ END_DATE2)) %>%
  filter(!(FR_CITATION == "50 FR 34850" & START_YEAR >= "1991"))

mh_carib_sc <-mh_carib_sc %>%
  dplyr::ungroup()

# Confirm size limits
unique(mh_carib_sc$MANAGEMENT_TYPE_USE)

# Confirm only commercial
table(mh_carib_sc$SECTOR_USE)

source(here("ODM-MH-Analysis_ready", "Closures", "func_expand_status.R"))

# Min size limits
min_df_sc <- expand_status(mh_carib_sc, "MINIMUM SIZE LIMIT")

# Collapse to annual value
min_yr_df_sc <- min_df_sc %>%
  dplyr::mutate(YEAR = year(date_sequence)) %>%
  dplyr::select(COMMON_NAME_USE, ZONE_USE, SUBSECTOR_USE, YEAR,
         VALUE, VALUE_UNITS, VALUE_TYPE, VALUE_RATE) %>%
  distinct() 

# Explore Zone 
table(min_yr_df_sc$ZONE_USE)

# Explore Subsector
table(min_yr_df_sc$SUBSECTOR_USE)
# All for all data

# Explore year - only include complete years 
table(min_yr_df_sc$YEAR)
min_yr_df_sc<-min_yr_df_sc %>% filter(YEAR <=termyr)
table(min_yr_df_sc$YEAR)

# Format for plot
min_yr_df2_sc <- min_yr_df_sc %>%
  mutate(VALUE_UNITS = case_when(VALUE_UNITS == 'INCHES' ~ 'in',
                                 TRUE ~ VALUE_UNITS),
         VALUE_TYPE = case_when(VALUE_TYPE == 'FORK LENGTH' ~ 'FL',
                                TRUE ~ VALUE_TYPE),
         VALUE_TYPE = case_when(VALUE_TYPE == 'TOTAL LENGTH' ~ 'TL',
                                TRUE ~ VALUE_TYPE),
         VALUE_TYPE = case_when(VALUE_TYPE == "CARAPACE LENGTH" ~ "Carapace Length",
                                TRUE ~ VALUE_TYPE),
         VAL_LAB = paste(VALUE, VALUE_UNITS, VALUE_TYPE),
         LAB = paste(str_to_title(sub('(.*)\\,\\s+(.*)','\\2 \\1',COMMON_NAME_USE)),"-",  VAL_LAB),
         #         LAB = paste(COMMON_NAME_USE,"-",  VAL_LAB),
         #         LAB = paste(COMMON_NAME_USE, "(Zone ", ZONE_USE, ")", VAL_LAB),
         n = 1) %>%
  dplyr::select(LAB, YEAR, n) %>%
  arrange(YEAR) %>%
  pivot_wider(names_from = YEAR, values_from = n) %>%
  arrange(LAB) 

# Reorder by family (manually for now...)
dim(min_yr_df2_sc) #22 x 42
min_yr_df2_sc<-min_yr_df2_sc[c(1:17, 21:22, 18:20),]
dim(min_yr_df2_sc) #37 x 42

# Save outputs before we clean up labels
saveRDS(min_yr_df2_sc,paste0(Dirout,"commercial_pr_size_limits.rds"))

# Remove species name in consecutive rows
min_yr_df2_sc$LAB[which(min_yr_df2_sc$LAB=="Nassau Grouper - 13 in TL")]<-"- 13 in TL"
min_yr_df2_sc$LAB[which(min_yr_df2_sc$LAB=="Nassau Grouper - 14 in TL")]<-"- 14 in TL"
min_yr_df2_sc$LAB[which(min_yr_df2_sc$LAB=="Nassau Grouper - 15 in TL")]<-"- 15 in TL"
min_yr_df2_sc$LAB[which(min_yr_df2_sc$LAB=="Nassau Grouper - 16 in TL")]<-"- 16 in TL"
min_yr_df2_sc$LAB[which(min_yr_df2_sc$LAB=="Nassau Grouper - 17 in TL")]<-"- 17 in FL"
min_yr_df2_sc$LAB[which(min_yr_df2_sc$LAB=="Nassau Grouper - 18 in TL")]<-"- 18 in TL"
min_yr_df2_sc$LAB[which(min_yr_df2_sc$LAB=="Nassau Grouper - 19 in TL")]<-"- 19 in TL"
min_yr_df2_sc$LAB[which(min_yr_df2_sc$LAB=="Nassau Grouper - 20 in TL")]<-"- 20 in TL"
min_yr_df2_sc$LAB[which(min_yr_df2_sc$LAB=="Nassau Grouper - 21 in TL")]<-"- 21 in TL"
min_yr_df2_sc$LAB[which(min_yr_df2_sc$LAB=="Nassau Grouper - 22 in TL")]<-"- 22 in TL"
min_yr_df2_sc$LAB[which(min_yr_df2_sc$LAB=="Nassau Grouper - 23 in TL")]<-"- 23 in TL"
min_yr_df2_sc$LAB[which(min_yr_df2_sc$LAB=="Nassau Grouper - 24 in TL")]<-"- 24 in TL"
min_yr_df2_sc$LAB[which(min_yr_df2_sc$LAB=="Yellowtail Snapper - 9 in TL")]<-"- 9 in TL"
min_yr_df2_sc$LAB[which(min_yr_df2_sc$LAB=="Yellowtail Snapper - 10 in TL")]<-"- 10 in TL"
min_yr_df2_sc$LAB[which(min_yr_df2_sc$LAB=="Yellowtail Snapper - 11 in TL")]<-"- 11 in TL"
min_yr_df2_sc$LAB[which(min_yr_df2_sc$LAB=="Yellowtail Snapper - 12 in TL")]<-"- 12 in TL"

library(tidyverse)

# 1) Get just the year columns (handles if LAB is col 1)
yrs <- names(min_yr_df2_sc)
yrs <- yrs[grepl("^\\d{4}$", yrs)]   # keep only "1985", "1986", ...
yrs <- yrs[as.integer(yrs) >= 1985 & as.integer(yrs) <= termyr]

# 2) Optional: drop empty rows (all NA across years)
keep_rows <- rowSums(!is.na(min_yr_df2_sc[, yrs, drop = FALSE])) > 0

plot_df_sc <- min_yr_df2_sc %>%
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
p_sc <- ggplot(plot_df_sc, aes(x = YEAR, y = LAB, fill = present)) +
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
print(p_sc)
ggsave(filename = paste0(Dirout, "Commercial_size_limits_SC.jpeg"),
       plot = p_sc, width = 6.5, height = 4.6, dpi = 300)


