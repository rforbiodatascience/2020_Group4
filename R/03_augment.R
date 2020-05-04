library(tidyverse)

# Load raw data
relative <- read_csv("data/02_data_clean.csv")

# Augment data


## Group toxins
SVMP <- relative %>% 
  select(contains('SVMP (')) %>% 
  rowSums()

disintegrins <- relative %>% 
  select(contains('disintegrin'), -contains('DC')) %>% 
  rowSums()

lectins <- relative %>% 
  select(contains('lectin')) %>% 
  rowSums()

neurotoxins <- relative %>% 
  select(contains('NeuroToxin')) %>% 
  rowSums()

PLA2 <- relative %>% 
  select(contains('PLA2')) %>% 
  rowSums()

unknown <- relative %>% 
  select(contains('Unknown')) %>% 
  rowSums()


relative <- relative %>% 
  select(-contains('SVMP ('), 
         -contains('disintegrin,'),
         -contains('lectin'),
         -contains('NeuroToxin'),
         -contains('PLA2'),
         -contains('Unknown')
  ) %>% 
  mutate(
    SVMP = SVMP,
    disintegrin = disintegrins,
    lectins = lectins,
    neurotoxins = neurotoxins,
    PLA2 = PLA2,
    Unknown = unknown
  )

## Create new columns
relative <- relative %>% 
  mutate(genus = str_split(Snake, " ", simplify = TRUE)[, 1],
         species = str_split(Snake, " ", simplify = TRUE)[, 2]) %>% 
  select(Snake, genus, species, everything())
  # Do more stuff
  
  
# Write augmented data
relative %>% 
  write_csv('data/03_data_aug.csv')
