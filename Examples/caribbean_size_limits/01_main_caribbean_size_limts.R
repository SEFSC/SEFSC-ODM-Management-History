# Management History Processing ####

# Load packages ####
#install.packages("librarian")
librarian::shelf(here, tidyverse, lubridate)

# Suppress summarize info ####
options(dplyr.summarize.inform = FALSE)

# Set working directory ####
# Working directory is relative to project root (SEFSC-MH-Processing.Rproj)
here::i_am('code/main_MH_prep.R')