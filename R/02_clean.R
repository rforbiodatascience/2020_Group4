library(tidyverse)

# Load raw data
relative <- read_csv("data/_raw/relative.csv")

# Clean data
relative <- relative
  # Do something


# Write cleaned data
relative %>% 
  write_csv('data/relative_clean.csv')