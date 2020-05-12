library(tidyverse)

# Load clean data
data_clean <- read_csv("data/02_data_clean.csv")

# Add new data ------------------------------------------------------------
new_data <- read_csv("data/_raw/01_new_data.csv")

data_aug <- data_clean %>% 
  full_join(new_data) %>%
  mutate_each(list(~replace(., which(is.na(.)), 0)))


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

# Rename colnames to only contain abbreviations
colnames(data_aug) <- str_split(colnames(data_aug), pattern = " ", simplify = TRUE)[, 1]

# Remove toxins with few occurances ---------------------------------------
summed_toxins <- data_aug %>% 
  select_if(is.numeric) %>% 
  select(-Unknown) %>% 
  summarise_all(is_not_zero) %>% 
  pivot_longer(everything(), values_to = 'toxin_occurance', names_to = 'toxin') %>%
  filter(toxin_occurance > 5)

data_aug <- data_aug %>% 
  select(c("Snake", "Reference", "Country", summed_toxins$toxin)) %>% 
  mutate(Unknown = 100 - Reduce(`+`, select_if(., is.numeric)))



# Separate snake names ----------------------------------------------------
data_aug <- data_aug %>% 
  mutate(Genus = str_split(Snake, " ", simplify = TRUE)[, 1],
         Species = str_split(Snake, " ", simplify = TRUE)[, 2]) %>% 
  select(Snake, Genus, Species, everything())
  


# Add families ------------------------------------------------------------
snake_families <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vR1J2-JTgyqdK48fycrWrlC5bqWFHxVatiCLhvWuxnxTJYhuKoq-bMpEvxjL57LwePK819TJAHU-tkC/pub?gid=1798552264&single=true&output=csv",
                           col_types = "cc") %>% 
  rename(Genus = 'Snake genus') %>% 
  unique()

# Join families to data
data_aug <- data_aug %>% 
  left_join(snake_families, by = "Genus") %>% 
  select(1:Country, Family, everything())

# Sanity check
data_aug %>% 
  count(Family)



# Write augmented data ----------------------------------------------------
data_aug %>% 
  write_csv('data/03_data_aug.csv')