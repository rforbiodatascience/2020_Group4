library(tidyverse)
source('R/99_proj_func.R')

# Load clean data
data_clean <- read_csv("data/02_data_clean.csv")

# Add new data to main data ------------------------------------------------------------
new_data <- read_csv("data/_raw/01_new_data.csv")
new_meta <- read_csv('data/_raw/01_new_meta.csv')

# Make new data tidy
new_data <- new_data %>% 
  pivot_longer(-Toxin, names_to = "Snake", values_to = "value") %>%
  pivot_wider(names_from = Toxin, values_from = value) %>%
  left_join(new_meta, by = "Snake") %>% 
  mutate(`Unknown/Undetermined` = 100 - Reduce(`+`, select_if(., is.numeric)))

data_aug <- data_clean %>% 
  full_join(new_data) %>%
  replace(is.na(.), 0)

# # Rename colnames to only contain abbreviations
colnames(data_aug) <- str_split(colnames(data_aug), pattern = " \\(", simplify = TRUE)[, 1] %>%
   str_replace(pattern = "-toxin", replacement = "toxin")



# Group toxins ------------------------------------------------------------
SVMPs <- data_aug %>% 
  select(ends_with('SVMP'))

disintegrins <- data_aug %>% 
  select(ends_with('disintegrin'))

lectins <- data_aug %>% 
  select(`CTL`, Selectins, Gal)

FTx3 <- data_aug %>% 
  select(contains('NTx'), `3Ftx`, `Muscarinictoxin`, `Mojavetoxin`, `beta-BTx`)

PLA2s <- data_aug %>% 
  select(contains('PLA2'))

VAPs <- data_aug %>% 
  select(VAP, NP, BPP, BIP)

unknowns <- data_aug %>% 
  select(contains('Unknown'))

data_aug <- data_aug %>% 
  select(-all_of(colnames(SVMPs)), 
         -all_of(colnames(disintegrins)),
         -all_of(colnames(lectins)),
         -all_of(colnames(FTx3)),
         -all_of(colnames(PLA2s)),
         -all_of(colnames(VAPs)),
         -all_of(colnames(unknowns))
  ) %>% 
  mutate(
    SVMP = SVMPs %>% rowSums(),
    Disintegrin = disintegrins %>% rowSums(),
    Lectins = lectins %>% rowSums(),
    `3FTx` = FTx3 %>% rowSums(),
    PLA2 = PLA2s %>% rowSums(),
    VAP = VAPs %>% rowSums(),
    Unknown = unknowns %>% rowSums()
  )




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


# Separate snake names into genus and species ----------------------------------------------------
data_aug <- data_aug %>% 
  mutate(Genus = str_split(Snake, " ", simplify = TRUE)[, 1],
         Species = str_split(Snake, " ", simplify = TRUE)[, 2])


# Add snake families ------------------------------------------------------------
snake_families <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vR1J2-JTgyqdK48fycrWrlC5bqWFHxVatiCLhvWuxnxTJYhuKoq-bMpEvxjL57LwePK819TJAHU-tkC/pub?gid=1798552264&single=true&output=csv",
                           col_types = "cc") %>% 
  rename(Genus = 'Snake genus') %>% 
  unique()

# Join families to data
data_aug <- data_aug %>% 
  left_join(snake_families, by = "Genus") %>% 
  select(Snake, Genus, Species, Family, Country, Reference, everything())

# Sanity check
data_aug %>% 
  count(Family)


# Write augmented data ----------------------------------------------------
data_aug %>% 
  write_csv('data/03_data_aug.csv')