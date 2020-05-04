library(tidyverse)

# Load raw data
data_aug <- read_csv("data/02_data_clean.csv")

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


data_aug <- data_aug %>% 
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
data_aug <- data_aug %>% 
  mutate(genus = str_split(Snake, " ", simplify = TRUE)[, 1],
         species = str_split(Snake, " ", simplify = TRUE)[, 2]) %>% 
  select(Snake, genus, species, everything())
  # Do more stuff
  

# 
Snakedata <- read_csv('data/_raw/Snakedata.csv')
families <- Snakedata %>% 
  select('Family', 'Snake genus') %>% 
  rename(genus = 'Snake genus',
         family = Family) %>% 
  unique()

data_aug %>% 
  left_join(families, by = 'genus') %>% 
  count(family) %>%
  View()





# Write augmented data
data_aug %>% 
  write_csv('data/03_data_aug.csv')
