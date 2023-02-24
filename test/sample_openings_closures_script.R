#create date range for dates time series
min_date <- min(mh_expanded2$EFFECTIVE_DATE)
max_date <- max(mh_expanded2$EFFECTIVE_DATE)

#create date sequence
mgmt_dates_ts <- data.frame(DATE=seq(min_date,max_date,by="days"))

#isolate fishing season, openings and closures for a given species, region and gear 

#specify your inputs
#Species name
sp <- "SNAPPER, RED"

#region in "GULF OF MEXICO", "SOUTH ATLANTIC", "MID ATLANTIC", "CARIBBEAN"
reg <- "GULF OF MEXICO"

#Sector in "RECREATIONAL" "COMMERCIAL" 
sector <- "COMMERCIAL"

#Relevent areas to include - currently specifying list of general zones but may be necessary to refine for each analysis
#from all ZONE_USE values
zones <- c("ALL","ATLANTIC MIGRATORY GROUP SPANISH MACKEREL - NORTHERN ZONE","ATLANTIC MIGRATORY GROUP SPANISH MACKEREL - SOUTHERN ZONE",
           "ATLANTIC MIGRATORY GROUP SPANISH MACKEREL","GULF MIGRATORY GROUP KING MACKEREL - NORTHERN ZONE","GULF MIGRATORY GROUP KING MACKEREL - WESTERN ZONE",
           "GULF MIGRATORY GROUP KING MACKEREL - EASTERN ZONE - FLORIDA WEST COAST NORTHERN SUBZONE","GULF MIGRATORY GROUP KING MACKEREL - EASTERN ZONE - FLORIDA EAST COAST SUBZONE",
           "GULF MIGRATORY GROUP KING MACKEREL - EASTERN ZONE - FLORIDA WEST COAST SUBZONE","GULF MIGRATORY GROUP KING MACKEREL - EASTERN ZONE",
           "GULF MIGRATORY GROUP KING MACKEREL","GULF MIGRATORY GROUP KING MACKEREL - SOUTHERN ZONE","GULF MIGRATORY GROUP KING MACKEREL - EASTERN ZONE - FLORIDA WEST COAST SOUTHERN SUBZONE",
           "ATLANTIC MIGRATORY GROUP COBIA","ATLANTIC MIGRATORY GROUP KING MACKEREL","GULF MIGRATORY GROUP SPANISH MACKEREL","OFF SOUTH CAROLINA AND GEORGIA",
           "SOUTH OF 35°15.9' NORTH LATITUDE (CAPE HATTERAS LIGHT NORTH CAROLINA)","SOUTH OF 25°09' NORTH LATITUDE OFF THE WEST COAST OF FLORIDA",
           "OFF THE FLORIDA KEYS AND EAST COAST OF FLORIDA","OFF SOUTH CAROLINA","OFF GEORGIA")

#specify gear? maybe not may be better to have multiple open/closed columns (one for each gear)

#filter out mh data of interest
#measures relevant to closures and openings in MANAGEMENT_TYPE_USE %in% c("FISHING YEAR","FISHING SEASON","FMP ESTABLISHMENT","CLOSURE")
openings_closures <- filter(mh_expanded2,COMMON_NAME_USE==sp & REGION==reg & SECTOR_USE==sector &
                              MANAGEMENT_TYPE_USE %in% c("FISHING YEAR","FISHING SEASON","FMP ESTABLISHMENT","CLOSURE") &
                              ZONE_USE %in% zones) %>%
  arrange(EFFECTIVE_DATE) #%>%
  #select(COMMON_NAME_USE,FR_CITATION,MANAGEMENT_CATEGORY,MANAGEMENT_TYPE_USE,SECTOR_USE,SUBSECTOR_USE,ZONE_USE,START_DATE2,END_DATE2,FR_URL)

openings <- filter(openings_closures, MANAGEMENT_TYPE_USE %in% c("FISHING YEAR","FISHING SEASON"))
#Need to first create time series just showing fishing seasons ignoring closures and reopenings
ts_open_close <- filter(mgmt_dates_ts,DATE>(min(openings$START_DATE2)-1))
ts_open_close$STATUS <- NA

#ERIC's attempt 2/24/2023, I was able to find a function to fill NAs with previous values
#The problem, we need to reset the season (either January 1 or fishing season)
openings <- filter(openings_closures, MANAGEMENT_TYPE %in% c("REOPENING","FISHING YEAR","FISHING SEASON"))
#Need to first create time series just showing fishing seasons ignoring closures and reopenings
ts_open_close <- filter(mgmt_dates_ts,DATE>(min(openings$START_DATE2)-1))
ts_open_close$EFFECTIVE_DATE=ts_open_close$DATE
ts_open_close$MANAGEMENT_TYPE <- NA
closure= filter(openings_closures,MANAGEMENT_TYPE%in% c("CLOSURE"))
closure=subset(closure,select=c(MANAGEMENT_TYPE,EFFECTIVE_DATE))
closure %>%
  distinct(EFFECTIVE_DATE, .keep_all = TRUE)
ts2=merge(ts_open_close,closure,by=c('EFFECTIVE_DATE'),all=T)
ts3=ts2[,-3]
o=openings
o$MANAGEMENT_TYPE.y="OPEN"
o %>%
  distinct(EFFECTIVE_DATE, .keep_all = TRUE)
o=subset(o,select=c(MANAGEMENT_TYPE.y,EFFECTIVE_DATE))
o
ts4=merge(ts3,o,by=c('EFFECTIVE_DATE'),all=T)
ts4$mt=NA
ts4$mt[ts4$MANAGEMENT_TYPE.y.x=="CLOSURE"]="CLOSED"
ts4$mt[ts4$MANAGEMENT_TYPE.y.y=="OPEN"]="OPEN"

repeat.before = function(x) {   # repeats the last non NA value. Keeps leading NA
  ind = which(!is.na(x))      # get positions of nonmissing values
  if(is.na(x[1]))             # if it begins with a missing, add the 
    ind = c(1,ind)        # first position to the indices
  rep(x[ind], times = diff(   # repeat the values at these indices
    c(ind, length(x) + 1) )) # diffing the indices + length yields how often 
}                               # they need to be repeated

ts5=ts4
ts5$mt1=repeat.before(ts5$mt)

ts5$month<- strftime(ts5$EFFECTIVE_DATE,"%m")
ts5$yr=strftime(ts5$EFFECTIVE_DATE,"%Y")
library(doBy)
rs=summaryBy(mt1~yr,data=ts5[ts5$mt1=="CLOSED",],FUN=length)
rs1=summaryBy(mt1~yr,data=ts5[ts5$mt1=="OPEN",],FUN=length)
###END ERIC's 1st attempt