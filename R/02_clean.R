library(tidyverse)

# Load raw data
relative <- read_csv("data/_raw/relative.csv")

# Clean data
SVMP <- relative %>% 
  select(contains('SVMP (')) %>% 
  rowSums()

disintegrins <- relative %>% 
  select(contains('disintegrin,')) %>% 
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


relative <- relative %>% 
  select(-contains('SVMP ('), 
         -contains('disintegrin,'),
         -contains('lectin'),
         -contains('NeuroToxin'),
         -contains('PLA2')
         ) %>% 
  mutate(
    SVMP = SVMP,
    disintegrin = disintegrins,
    lectins = lectins,
    neurotoxins = neurotoxins,
    PLA2 = PLA2
    )

relative %>% 
  select(-c("Snake", "Reference", "Note", "Sum, %")) %>% 
  colSums() %>% 
  View()


# Write cleaned data
relative %>% 
  write_csv('data/relative_clean.csv')