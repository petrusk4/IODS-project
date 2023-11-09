# ==============================================================================
# **DATA WRANGLING**
# Author: Petrus Nuotio
# Sections 2.0--2.6 produce a dataset named `data/learning2014.csv`.
# For statistical analysis, **skip to section 2.7**.

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

# ==============================================================================
### 2.1 Reading data from the web
# Get data
lrn14 <- read.table(
  "http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-data.txt",
  sep = "\t",
  header = TRUE
)

# Check structure
# dim(lrn14)
str(lrn14)

# - Looks like the `lrn14` dataset has `r nrow(lrn14)` rows and `r ncol(lrn14)` columns.
# - Most columns appear to be intergers, whereas only `gender` is character.

# ==============================================================================
### 2.2 Scaling variables
# Returning variable `Attitude` to its original scale between 1--5 (likert scale).

# Scale the attitude variable to match other variables
lrn14 <- lrn14 %>%
  mutate(attitude = Attitude / 10)

# ==============================================================================
### 2.3 Combining variables
# Computing row means for each class of questions in a for-loop.

# Classify questions
deep <- c(
  "D03", "D11", "D19", "D27", "D07", "D14",
  "D22", "D30","D06",  "D15", "D23", "D31"
)
surf <- c(
  "SU02", "SU10", "SU18", "SU26", "SU05", "SU13",
  "SU21", "SU29", "SU08", "SU16", "SU24", "SU32"
)
stra <- c(
  "ST01", "ST09", "ST17", "ST25",
  "ST04", "ST12", "ST20", "ST28"
)

# Compute averages in a loop
for (Qclass in c("deep", "surf", "stra")) {
  lrn14[[Qclass]] <- rowMeans(lrn14[, get(Qclass)])
}

# ==============================================================================
### 2.4 Selecting columns
# Dropping individual question data and only keeping variables
# relevant for the analysis.

# Pick relevant vars
keep_columns <- c("gender", "Age", "attitude", "deep", "stra", "surf", "Points")

# Drop useless vars
learning2014 <- lrn14[, keep_columns]

# See structure
str(learning2014)

# ==============================================================================
### 2.5 Modifying column names
# Harmonizing all column names to include only lowercase letters.

# old_colnames <- colnames(learning2014)
learning2014 <- learning2014 %>%
  rename("age" = Age, "points" = Points)
# new_colnames <- colnames(learning2014)
# data.frame(old_colnames, new_colnames) %>% knitr::kable()

# ==============================================================================
### 2.6 Excluding observations
# Excluding students that did not attend the exam.

# Filter non-attending students
learning2014 <- learning2014 %>%
  filter(points > 0)

# ==============================================================================
### Write to file
# Saving the processed minimal dataframe as a `.csv` file.
readr::write_csv(
  learning2014,
  paste0("data/learning/learning2014_", Sys.Date(), ".csv")
)
