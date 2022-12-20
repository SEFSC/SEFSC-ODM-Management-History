# Load species list ####

# Standardize and fill in common name for FMP species, species aggregate and species group tables
# Create work space containing the species list data frame to use in expansion (sp_info_use)

# Connect to the server ####
con <- dbConnect(dbDriver("Oracle"),
                 username = keyring::key_list("SECPR")[1,2],
                 password = keyring::key_get("SECPR", keyring::key_list("SECPR")[1,2]),
                 dbname = "SECPR")


# Ask the repo owner for the .env file ####
load_dot_env("mh_spp.env")
spp_grp_view <- Sys.getenv("FMP_SPECIES_GRP")
spp_agg_view <- Sys.getenv("FMP_SPECIES_AGG")
spp_itis_xref <- Sys.getenv("SEDAT_SPP_ITIS")

# Query original species tables in Oracle ####
Oracle_fmp_spp_grp <- dbGetQuery(con, paste0("select * from ", spp_grp_view, ""))
Oracle_fmp_spp_agg <- dbGetQuery(con, paste0("select * from ", spp_agg_view, ""))

# Read in species list ####
# Exclude HMS in all species tables 
# Join SEDAT species ITIS table to standardize the common name field
    # Example: 169593 says 'longsnout butterflyfish' in V_S_FMP_SPECIES_GRP_DET table and SEDAT has common name as 'BUTTERFLYFISH, LONGSNOUT

# Query FMP table
fmp_info = dbGetQuery(con, paste0("select distinct
                                    a.FMP_GROUP_ID,
                                    a.FMP_GROUP_NAME,
                                    a.SPECIES_ITIS,
                                    coalesce(b.itis_commonname, a.common_name) as common_name,
                                    coalesce(a.scientific_name, b.itis_scientificname) as scientific_name,
                                    MIN(a.ADDED_DT) OVER (PARTITION BY a.FMP_GROUP_ID, a.FMP_GROUP_NAME, a.SPECIES_ITIS, a.COMMON_NAME, a.SCIENTIFIC_NAME, a.ALIAS_NAME) AS ADDED_DT,
                                    case when (MAX(case when a.removed_dt is null then 1 else 0 end) OVER (PARTITION BY a.FMP_GROUP_ID, a.FMP_GROUP_NAME, a.SPECIES_ITIS, a.COMMON_NAME, a.SCIENTIFIC_NAME, a.ALIAS_NAME)) = 1 then
                                    NULL else
                                    MAX(a.REMOVED_DT) OVER (PARTITION BY a.FMP_GROUP_ID, a.FMP_GROUP_NAME, a.SPECIES_ITIS, a.COMMON_NAME, a.SCIENTIFIC_NAME, a.ALIAS_NAME) end AS REMOVED_DT
                        from ", spp_grp_view, " a
                        left join (select distinct itis_code, itis_commonname, itis_scientificname from ", spp_itis_xref, ") b
                        on a.species_itis = b.itis_code
                        where FMP_GROUP_NAME <> 'Atlantic Highly Migratory Species'
                        order by FMP_GROUP_NAME, COMMON_NAME"))


# Query species group table
group_info = dbGetQuery(con, paste("select distinct
                                          a.fmp_group_id,
                                          a.fmp_group_name,
                                          a.subgrp_name,
                                          a.species_itis,
                                          coalesce(b.itis_commonname, a.common_name) as common_name,
                                          coalesce(a.scientific_name, b.itis_scientificname) as scientific_name,
                                          a.added_dt,
                                          a.removed_dt
                                from ", spp_grp_view, " a
                                left join (select distinct itis_code, itis_commonname, itis_scientificname from ", spp_itis_xref, ") b
                                on a.species_itis = b.itis_code
                                where a.subgrp_name is not null and FMP_GROUP_NAME <> 'Atlantic Highly Migratory Species'"))

# Clean up duplicates ####
# Gray snapper is consistently in the snapper species group
# Golden tilefish is connsistently in the tilefishes species group
group_info  <- group_info %>%
  #filter(SUBGRP_NAME == 'Snappers—Lutjanidae', FMP_GROUP_NAME == 'Reef Fish Fishery of Puerto Rico and the U.S. Virgin Islands', COMMON_NAME == 'SNAPPER, GRAY')
  #filter(SUBGRP_NAME == 'Tilefishes—Malacanthidae', FMP_GROUP_NAME == 'Snapper-Grouper Fishery of the South Atlantic Region', COMMON_NAME == 'TILEFISH, GOLDEN') %>%
  group_by(FMP_GROUP_ID, FMP_GROUP_NAME, SUBGRP_NAME, SPECIES_ITIS, COMMON_NAME, SCIENTIFIC_NAME) %>%
  summarise(ADDED_DT = min(ADDED_DT),
            REMOVED_DT = max(REMOVED_DT)) 
  

# Query species aggregate table
agg_info = dbGetQuery(con, paste("select distinct 
                                    a.fmp_group_name,
                                    a.fmp_species_agg_name,
                                    a.species_itis,
                                    coalesce(b.itis_commonname, a.common_name) as common_name,
                                    coalesce(a.scientific_name, b.itis_scientificname) as scientific_name,
                                    a.added_dt,
                                    a.removed_dt
                          from ", spp_agg_view, " a
                          left join (select distinct itis_code, itis_commonname, itis_scientificname from ", spp_itis_xref, ") b
                          on a.species_itis = b.itis_code
                          where FMP_GROUP_NAME <> 'Atlantic Highly Migratory Species'
                          order by FMP_GROUP_NAME, FMP_SPECIES_AGG_NAME"))

# Reformat species lists for merge to MH
# Transposed data to eliminate the need for 3 fields and dealing with null values
# species_name_type indicates whether it is a group, aggregate, or common name
# Name is the actual group or aggregate name for expansion or when common name is ALL for expansion

# Reformat FMP Species data
fmp_info_use <- fmp_info %>%
  # Set species_name_type to ALL
  mutate(SPP_TYPE = "COMMON_NAME",
         SPP_NAME = 'ALL') %>%
  select(FMP_GROUP_NAME, SPP_TYPE, SPP_NAME, SPECIES_ITIS, COMMON_NAME, SCIENTIFIC_NAME, ADDED_DT, REMOVED_DT)

# Reformat species group data
grp_info_use <- group_info %>%
  # Set species_name_type to species group
  mutate(SPP_TYPE = "SPECIES_GROUP",
         SPP_NAME = SUBGRP_NAME) %>%
  select(FMP_GROUP_NAME, SPP_TYPE, SPP_NAME, SPECIES_ITIS, COMMON_NAME, SCIENTIFIC_NAME, ADDED_DT, REMOVED_DT)

# Reformat species aggregate data
agg_info_use <- agg_info %>%
  # Set species_name_type to species aggregate
  mutate(SPP_TYPE = "SPECIES_AGGREGATE",
         SPP_NAME = FMP_SPECIES_AGG_NAME) %>%
  select(FMP_GROUP_NAME, SPP_TYPE, SPP_NAME, SPECIES_ITIS, COMMON_NAME, SCIENTIFIC_NAME, ADDED_DT, REMOVED_DT) %>%
  # One record has common name null for 'Bag Limit: Parrotfishes' - error to fix in database
  filter(!is.na(COMMON_NAME))

# Combine species lists
sp_info_use = bind_rows(fmp_info_use, grp_info_use, agg_info_use) %>%
  rename(COMMON_NAME_USE = COMMON_NAME,
         SPECIES_ITIS_USE = SPECIES_ITIS,
         FMP = FMP_GROUP_NAME) %>%
  # Set field to upper to match MH dataset
  mutate(FMP = toupper(FMP),
         SPP_NAME = toupper(SPP_NAME),
         # Set common name for species ITIS codes missing common name
         COMMON_NAME_USE = case_when(SPECIES_ITIS_USE == '614546' ~ 'RAZORFISH, GREEN',
                                     SPECIES_ITIS_USE == '614513' ~ 'RAZORFISH, PEARLY',
                                     TRUE ~ COMMON_NAME_USE)) %>%
  # Reformat added and removed dates to remove time (this was not collected and does not appear in data base) 
  # In Oracle date 11/8/1984 changes to 1984-11-07 23:00:00
  mutate(ADDED_SP_DATE = as.Date(strftime(ADDED_DT, format = "%Y-%m-%d")),
         REMOVED_SP_DATE = as.Date(strftime(REMOVED_DT, format = "%Y-%m-%d"))) %>%
  select(-c(ADDED_DT, REMOVED_DT)) 

# Remove connection to Oracle when saving data to work space
rm(con, spp_grp_view, spp_agg_view, spp_itis_xref)

# Save work space
saveRDS(sp_info_use, here('data', 'interim', 'MH_clean_spp_tables.RDS'))

