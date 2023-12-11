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
  values_to = "bprs"
) %>% mutate(
  week = gsub("week", "", week) %>% as.numeric(),
)

RATSL <- RATS %>% pivot_longer(
  cols = WD1:WD64,
  names_to = "WD",
  values_to = "Weight"
) %>% mutate(
  WD = gsub("WD", "", WD) %>% as.numeric()
)

# ==============================================================================
# Glimpse at the long data
finalfit::finalfit_glimpse(BPRSL)
finalfit::finalfit_glimpse(RATSL)

# ==============================================================================
# Visualize
# Loop instructions
datasets <- tribble(
  ~dataset, ~groupby, ~id, ~measurevar, ~timevar,
  "BPRSL", "treatment", "subject", "bprs", "week",
  "RATSL", "Group", "ID", "Weight", "WD"
)

# Loop
plist <- lapply(1:nrow(datasets), \ (i) {
  datasets$dataset[i] %>% get() %>%
    ggplot(aes(
      x = .data[[datasets$timevar[i]]],
      y = .data[[datasets$measurevar[i]]],
      color = .data[[datasets$groupby[i]]]
    )) +
    stat_summary(fun.data = mean_cl_normal, position = position_dodge(.5)) +
    stat_summary(
      aes(group = .data[[datasets$groupby[i]]]),
      fun = mean, geom = "line", position = position_dodge(.5)
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
    ggtitle(datasets$dataset[i])
})
ggpubr::ggarrange(plotlist = plist)

# ==============================================================================
# Write to file
BPRSL %>% write_csv(paste0("data/bprs_rats/ready/BPRS_long_", Sys.Date(), ".csv"))
RATSL %>% write_csv(paste0("data/bprs_rats/ready/RATS_long_", Sys.Date(), ".csv"))
