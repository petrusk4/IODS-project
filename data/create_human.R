# ==============================================================================
# **DATA WRANGLING**
# Author: Petrus Nuotio
# Date: 28.11.2023

# Preprocessing data for the "analysis" section
# of 5. Dimension reduction techniques

# ==============================================================================
# Packages
# Let's first load the required packages
pacman::p_load(tidyverse)

# ==============================================================================
# Read data
hd <- read_csv("https://raw.githubusercontent.com/KimmoVehkalahti/Helsinki-Open-Data-Science/master/datasets/human_development.csv")
gii <- read_csv("https://raw.githubusercontent.com/KimmoVehkalahti/Helsinki-Open-Data-Science/master/datasets/gender_inequality.csv", na = "..")

# Metadata:
# https://hdr.undp.org/data-center/human-development-index#/indicies/HDI
# https://hdr.undp.org/system/files/documents/technical-notes-calculating-human-development-indices.pdf

# ==============================================================================
# Explore, summaries
# The 'human' dataset originates from the United Nations Development Programme
list(hd, gii) %>% map(\ (x) finalfit::finalfit_glimpse(x))

# ==============================================================================
# New var names:
# https://github.com/KimmoVehkalahti/Helsinki-Open-Data-Science/blob/master/datasets/human_meta.txt

# Rename
hd_new <- hd %>% rename(
  "Country" = "Country",
  "HDI" = "Human Development Index (HDI)",
  "HDI.Rank" = "HDI Rank",
  "Life.Exp" = "Life Expectancy at Birth",
  "Edu.Exp" = "Expected Years of Education",
  "Edu.Mean" = "Mean Years of Education",
  "GNI" = "Gross National Income (GNI) per Capita",
  "GNI.HDI" = "GNI per Capita Rank Minus HDI Rank"
)
gii_new <- gii %>% rename(
  "Country" = "Country",
  "GII" = "Gender Inequality Index (GII)",
  "GII.Rank" = "GII Rank",
  "Mat.Mor" = "Maternal Mortality Ratio",
  "Ado.Birth" = "Adolescent Birth Rate",
  "Parli.F" = "Percent Representation in Parliament",
  "Edu2.F" = "Population with Secondary Education (Female)",
  "Edu2.M" = "Population with Secondary Education (Male)",
  "Labo.F" = "Labour Force Participation Rate (Female)",
  "Labo.M" = "Labour Force Participation Rate (Male)"
) %>% mutate(
  Edu2.FM = Edu2.F / Edu2.M,
  Labo.FM = Labo.F / Labo.M
)


# ==============================================================================
# Merge
data <- inner_join(
  hd_new, gii_new,
  by = "Country"
)

# Glimpse at the new combined data
glimpse(data)


# Write to file
write_csv(data, paste0("data/human/ready/human_", Sys.Date(), ".csv"))

# ==============================================================================
# Keep only needed variables
data <- data %>% dplyr::select(
  Country, Edu2.FM, Labo.FM, Edu.Exp, Life.Exp, GNI, Mat.Mor, Ado.Birth, Parli.F
)

# Remove all rows with missing values
data <- data %>% drop_na()

# Remove observations that relate to regions instead of countries
data <- data %>% filter(
  !Country %in% c(
    "Arab States",
    "East Asia and the Pacific",
    "Europe and Central Asia",
    "Latin America and the Caribbean",
    "South Asia",
    "Sub-Saharan Africa",
    "World"
  )
)

# Check end product
glimpse(data)


# Write to file
write_csv(data, paste0("data/human/ready/human_", Sys.Date(), ".csv"))
