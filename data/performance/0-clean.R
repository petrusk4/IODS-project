# ==============================================================================
# **DATA WRANGLING**
# Author: Petrus Nuotio
# Date: 8.11.2023

# Joining data for 3. Logistic regression
# Required by the "analysis" section
# Open data from: https://archive.ics.uci.edu/dataset/320/student+performance

# **List of sections**
# - 2.0 Packages
# - 2.1 Reading data from the web
# - 2.2 Scaling variables
# - 2.3 Combining variables
# - 2.4 Selecting columns
# - 2.5 Modifying column names
# - 2.6 Excluding observations
# ==============================================================================

### 2.0 Packages and functions
# Let's first load the required packages and define some custom functions.
# Packages
pacman::p_load(tidyverse)


# Read data
datasets <- list.files("data/performance/original", full.names = T)
datalist <- lapply(datasets, \ (x) read.table(x, sep = ";", header = TRUE))

# Variables to join by
joinby <- intersect(names(datalist[[1]]), names(datalist[[2]]))
joinby <- joinby[!joinby %in% c("failures", "paid", "absences", "G1", "G2", "G3")]

# Merge
data <- inner_join(
  datalist[[1]], datalist[[2]],
  by = joinby,
  suffix = c(".math", ".por")
)

glimpse(data)

# Compare to premade data
data_premade <- read.table(
  "https://raw.githubusercontent.com/KimmoVehkalahti/Helsinki-Open-Data-Science/master/datasets/alc.csv",
  sep = ",",
  header = TRUE
)
