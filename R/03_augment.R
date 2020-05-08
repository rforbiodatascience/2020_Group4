library(tidyverse)

# Load raw data
data_aug <- read_csv("data/02_data_clean.csv")


# Group toxins ------------------------------------------------------------
SVMP <- data_aug %>% 
  select(contains('SVMP (')) %>% 
  rowSums()

disintegrins <- data_aug %>% 
  select(ends_with('isintegrin')) %>% 
  rowSums()

lectins <- data_aug %>% 
  select(contains('lectin')) %>% 
  rowSums()

neurotoxins <- data_aug %>% 
  select(contains('NeuroToxin')) %>% 
  rowSums()

PLA2 <- data_aug %>% 
  select(contains('PLA2')) %>% 
  rowSums()

unknown <- data_aug %>% 
  select(contains('Unknown')) %>% 
  rowSums()


data_aug <- data_aug %>% 
  select(-contains('SVMP ('), 
         -ends_with('isintegrin'),
         -contains('lectin'),
         -contains('NeuroToxin'),
         -contains('PLA2'),
         -contains('Unknown')
  ) %>% 
  mutate(
    SVMP = SVMP,
    Disintegrin = disintegrins,
    Lectins = lectins,
    Neurotoxins = neurotoxins,
    PLA2 = PLA2,
    Unknown = unknown
  )

# Remove toxins with few occurances ---------------------------------------
summed_toxins <- data_aug %>% 
  select_if(is.numeric) %>% 
  summarise_all(is_not_zero) %>% 
  pivot_longer(everything(), values_to = 'toxin_occurance', names_to = 'toxin') %>%
  filter(toxin_occurance > 5)

data_aug <- data_aug %>% 
  select(c("Snake", "Reference", "Country", "Continent", summed_toxins$toxin))





# Separate snake names ----------------------------------------------------
data_aug <- data_aug %>% 
  mutate(Genus = str_split(Snake, " ", simplify = TRUE)[, 1],
         Species = str_split(Snake, " ", simplify = TRUE)[, 2]) %>% 
  select(Snake, Genus, Species, everything())
  


# Add families ------------------------------------------------------------
Snakedata <- read_csv('data/_raw/snake_families.csv')
families <- Snakedata %>% 
  rename(Genus = 'Snake genus') %>% 
  unique()

# Join families to data
data_aug <- data_aug %>% 
  left_join(families, by = "Genus")

# Sanity check
data_aug %>% 
  count(Family)



# Write augmented data
data_aug %>% 
  write_csv('data/03_data_aug.csv')
