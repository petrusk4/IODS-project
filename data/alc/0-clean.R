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
vars_free <- c("failures", "paid", "absences", "G1", "G2", "G3")
joinby <- intersect(names(datalist[[1]]), names(datalist[[2]]))
joinby <- joinby[!joinby %in% vars_free]

# Merge
data <- inner_join(
  datalist[[1]], datalist[[2]],
  by = joinby,
  suffix = c(".math", ".por")
)
glimpse(data)

alc <- data[, joinby]


# Resolve variables that were present in both datasets 
for (var in vars_free) {
  vars_both <- select(data, starts_with(var))
  var_1st <- vars_both %>% pull(1)
  
  if (is.numeric(var_1st)) {
    # Use rounded average
    alc[[var]] <- round(rowMeans(vars_both))
  }
  if (!is.numeric(var_1st)) {
    # Use the first column
    alc[[var]] <- var_1st
  }
}


# Glimpse at the new combined data
glimpse(alc)


# Compute new vars
alc <- alc %>% mutate(
  alc_use = (Dalc + Walc) / 2,
  high_use = alc_use > 2
)


# Compare to premade data
data_premade <- read.table(
  "https://raw.githubusercontent.com/KimmoVehkalahti/Helsinki-Open-Data-Science/master/datasets/alc.csv",
  sep = ",",
  header = TRUE
)
diffdf::diffdf(alc, data_premade)

# Write to file


