# ==============================================================================
# **DATA WRANGLING**
# Author: Petrus Nuotio
# Date: 7.12.2023

# Preprocessing data for the "analysis" section
# of *6. Analysis of longitudinal data*

# ==============================================================================
# Packages
# Let's first load the required packages
pacman::p_load(tidyverse)

# ==============================================================================
# Read data
BPRS <- readr::read_delim(
  "https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/BPRS.txt",
  show_col_types = F
)
RATS <- read.table(
  "https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/rats.txt",
  header = TRUE, sep = '\t'
)

# ==============================================================================
# Explore, summaries
# The 'human' dataset originates from the United Nations Development Programme
list(BPRS, RATS) %>% map(\ (x) finalfit::finalfit_glimpse(x))

# Metadata:

# ==============================================================================
# Write wide datasets to files
BPRS %>% write_csv(paste0("data/bprs_rats/ready/BPRS_wide_", Sys.Date(), ".csv"))
RATS %>% write_csv(paste0("data/bprs_rats/ready/RATS_wide_", Sys.Date(), ".csv"))

# ==============================================================================
# Convert the categorical variables of both data sets to factors
BPRS <- BPRS %>% mutate(
  treatment = factor(treatment),
  subject = factor(subject)
)
RATS <- RATS %>% mutate(
  ID = factor(ID),
  Group = factor(Group)
)

# ==============================================================================
# Convert the data sets to long form
# Add a week variable to BPRS and a Time variable to RATS
BPRSL <- BPRS %>% pivot_longer(
  cols = week0:week8,
  names_to = "week",
  values_to = "value"
)
RATSL <- RATS %>% pivot_longer(
  cols = WD1:WD64,
  names_to = "Time",
  values_to = "value"
)

# ==============================================================================
# Glimpse at the long data
finalfit::finalfit_glimpse(BPRSL)
glimpse(RATSL)

# Write to file
BPRSL %>% write_csv(paste0("data/bprs_rats/ready/BPRS_long_", Sys.Date(), ".csv"))
RATSL %>% write_csv(paste0("data/bprs_rats/ready/RATS_long_", Sys.Date(), ".csv"))