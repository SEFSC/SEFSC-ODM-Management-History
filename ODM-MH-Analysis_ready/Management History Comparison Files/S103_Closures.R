librarian::shelf(here, tidyverse, lubridate, openxlsx, neatRanges, splitstackshape, dplyr)

# Set output directory for plots
Dirout<-paste0(getwd(),"/Closures/")
if(!file.exists(Dirout)) dir.create(Dirout)

# MH dataset 
mh <- readRDS(here("ODM-MH-Data_log", "data", "results", "MH_DL_2025Nov18.RDS"))

# Set end year for plots
termyr<-2025

#######################
# Closures #
#######################


# --- Your subset (as provided) ---
mh_carib_pr_lob <- mh %>%
  dplyr::filter(
    FMP %in% c(
      "REEF FISH FISHERY OF PUERTO RICO AND THE U.S. VIRGIN ISLANDS",
      "FMP FOR THE EEZ AROUND PUERTO RICO",
      "SPINY LOBSTER FISHERY OF PUERTO RICO AND THE U.S. VIRGIN ISLANDS",
      "QUEEN CONCH RESOURCES OF PUERTO RICO AND THE U.S. VIRGIN ISLANDS"
    ),
    NEVER_IMPLEMENTED == 0,
    SECTOR_USE == "COMMERCIAL",
    MANAGEMENT_CATEGORY == "TEMPORAL CONTROLS",
    COMMON_NAME_USE == "LOBSTER, CARIBBEAN SPINY",
    REGION == "CARIBBEAN",
    ZONE_USE == "PUERTO RICO MANAGEMENT AREA"
  )

unique(mh_carib_pr_lob$MANAGEMENT_TYPE_USE)
table(mh_carib_pr_lob$SECTOR_USE)

# ===========================
# Create closure visualization
# Method: expand to YEARS + tile heatmap (ggplot)
# ===========================

library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(ggplot2)

termyr <- 2025

# 1) Keep closure-type records (adjust if your closure type label differs)
lob_cl <- mh_carib_pr_lob %>%
  dplyr::filter(MANAGEMENT_TYPE_USE == "CLOSURE") %>%
  dplyr::mutate(
    start = as.Date(START_DATE2),
    end   = as.Date(dplyr::coalesce(END_DATE2, as.Date(paste0(termyr, "-12-31"))))
  ) %>%
  dplyr::filter(!is.na(start), !is.na(end), end >= start)

# If you get "no records", check what closure type is called:
# unique(mh_carib_pr_lob$MANAGEMENT_TYPE_USE)

# 2) Expand to YEAR (not days)
lob_yr <- lob_cl %>%
  dplyr::mutate(
    start_year = lubridate::year(start),
    end_year   = lubridate::year(end)
  ) %>%
  tidyr::uncount(end_year - start_year + 1, .id = "k") %>%
  dplyr::mutate(YEAR = start_year + (k - 1L)) %>%
  dplyr::filter(YEAR <= termyr)

# 3) Create row label (edit if you want it simpler)
lob_yr <- lob_yr %>%
  dplyr::mutate(
    # interpret closure status; adjust if your VALUE field uses different text
    status = dplyr::case_when(
      VALUE %in% c("CLOSE", "CLOSED", "CLOSURE") ~ "CLOSE",
      VALUE %in% c("OPEN") ~ "OPEN",
      TRUE ~ as.character(VALUE)
    ),
    LAB = paste0(
      "Caribbean Spiny Lobster - Commercial (PR)"
    )
  )

# 4) Collapse to one status per year (CLOSE wins if mixed)
lob_yr_status <- lob_yr %>%
  dplyr::group_by(LAB, YEAR) %>%
  dplyr::summarise(
    status = dplyr::if_else(any(status == "CLOSE"), "CLOSE", "OPEN"),
    .groups = "drop"
  )

# 5) Ensure all years appear on x-axis
all_years <- seq(min(lob_yr_status$YEAR, na.rm = TRUE), termyr)

lob_yr_status <- lob_yr_status %>%
  tidyr::complete(LAB, YEAR = all_years, fill = list(status = NA_character_)) %>%
  dplyr::mutate(LAB = factor(LAB, levels = rev(sort(unique(LAB)))))

# 6) Plot (tile heatmap)
p_lob_pr <- ggplot(lob_yr_status, aes(x = YEAR, y = LAB, fill = status)) +
  geom_tile(color = "lightgray", linewidth = 0.25) +
  scale_x_continuous(breaks = seq(min(all_years), termyr, by = 2), expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_fill_manual(
    values = c("CLOSE" = "lightblue", "OPEN" = "lightblue"),
    na.value = "white",
    guide = "none"
  ) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    plot.margin = margin(5, 5, 5, 5)
  )

print(p_lob_pr)

# 7) Save (uses your existing Dirout if defined; otherwise saves to wd)
out_file <- if (exists("Dirout")) file.path(Dirout, "Commercial_PR_SpinyLobster_Closures.jpeg") else "Commercial_PR_SpinyLobster_Closures.jpeg"
ggsave(out_file, plot = p_lob_pr, width = 7, height = 2.2, dpi = 300)


#####3 monthly value
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

# ---------------------------
# Monthly closure visualization
# ---------------------------

# 1) Keep closure-type records (adjust if your closure type label differs)
lob_cl <- mh_carib_pr_lob %>%
  dplyr::filter(MANAGEMENT_TYPE_USE == "CLOSURE") %>%
  dplyr::mutate(
    start = as.Date(START_DATE2),
    end   = as.Date(dplyr::coalesce(END_DATE2, as.Date("2025-12-31")))
  ) %>%
  dplyr::filter(!is.na(start), !is.na(end), end >= start)

# If you get "no records", check:
# unique(mh_carib_pr_lob$MANAGEMENT_TYPE_USE)

# 2) Expand to MONTHS (not days)
lob_mo <- lob_cl %>%
  dplyr::mutate(
    start_m = floor_date(start, "month"),
    end_m   = floor_date(end, "month")
  ) %>%
  tidyr::uncount(
    as.integer((year(end_m) - year(start_m)) * 12 + (month(end_m) - month(start_m)) + 1),
    .id = "k"
  ) %>%
  dplyr::mutate(MONTH = start_m %m+% months(k - 1L))

# 3) Status mapping (adjust if VALUE uses different text)
lob_mo <- lob_mo %>%
  dplyr::mutate(
    status = dplyr::case_when(
      VALUE %in% c("CLOSE", "CLOSED", "CLOSURE") ~ "CLOSE",
      VALUE %in% c("OPEN") ~ "OPEN",
      TRUE ~ as.character(VALUE)
    ),
    LAB = "Caribbean Spiny Lobster"
  )

# 4) Collapse to one status per month (CLOSE wins)
lob_mo_status <- lob_mo %>%
  dplyr::group_by(LAB, MONTH) %>%
  dplyr::summarise(
    status = dplyr::if_else(any(status == "CLOSE"), "CLOSE", "OPEN"),
    .groups = "drop"
  )

# 5) Make a complete month grid (so missing months plot as blank/white)
all_months <- seq(min(lob_mo_status$MONTH, na.rm = TRUE),
                  max(lob_mo_status$MONTH, na.rm = TRUE),
                  by = "1 month")

lob_mo_status <- lob_mo_status %>%
  tidyr::complete(LAB, MONTH = all_months, fill = list(status = NA_character_)) %>%
  dplyr::mutate(LAB = factor(LAB, levels = rev(sort(unique(LAB)))))

# 6) Plot
p_lob_pr_month <- ggplot(lob_mo_status, aes(x = MONTH, y = LAB, fill = status)) +
  geom_tile(color = "lightgray", linewidth = 0.25) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b %Y",
    expand = c(0, 0)
  ) +
  scale_y_discrete(expand = c(0, 0)) +
  scale_fill_manual(
    values = c("CLOSE" = "lightblue", "OPEN" = "lightblue"),
    na.value = "white",
    guide = "none"
  ) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 6, angle = 45, vjust = 1, hjust = 1),
    plot.margin = margin(t= 5, r = 40, b = 20, l = 40)
  )

print(p_lob_pr_month)

# 7) Save
out_file <- if (exists("Dirout")) file.path(Dirout, "Commercial_PR_SpinyLobster_Closures_Monthly.jpeg") else "Commercial_PR_SpinyLobster_Closures_Monthly.jpeg"
ggsave(out_file, plot = p_lob_pr_month, width = 9.5, height = 5, dpi = 300)
