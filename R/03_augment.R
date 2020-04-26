library(tidyverse)

# Load raw data
relative <- read_csv("data/relative_clean.csv")

# Augment data
relative <- relative %>% 
  mutate(genus = str_split(Snake, " ", simplify = TRUE)[, 1],
         species = str_split(Snake, " ", simplify = TRUE)[, 2]) %>% 
  select(Snake, genus, species, everything())
  # Do more stuff
  
  
# Write augmented data
relative %>% 
  write_csv('data/relative_aug.csv')