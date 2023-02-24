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

