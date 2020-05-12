library(tidyverse)
source('R/99_proj_func.R')

# Load raw data
data_raw <- read_csv("data/_raw/01_data_load_relative.csv")

# Clean column containing regions -----------------------------------------
Brazilian_cities <- c("Juazeiro", "Ceara", "Paraiba", "Pernambuco", 
                      "ilha de Itaparica", "Adult and young in Brazil")
USA <- c("Colorado", "Arizona", "Idyllwild", "Loma Linda", "Phelan", 
         "Catalina","Texas", "Kentucky", "Missouri", "Florida", "Kansas", 
         "Colorado", "Ohio", "New Mexico", "Forida")
Unknown <- c("Origin unknown", "neonate", "adult")

# Conditions in data set: 
  # rownames in Snake column containing "*" indicates transcriptomic data, thus deleted.
  # rownames in Note column containing "pooled" indicates pooled venom of different snakes, thus deleted.
data_clean <- data_raw %>% 
  rename_if(is_double, rm_percent) %>% 
  rename(`SP (Serine Proteinase)` = `SP (Serine roteinase)`,
         `α-NTx (α-NeuroToxin)` = `?-NTx (?-NeuroToxin)`) %>% 
  filter(!(str_to_lower(Note) == "pooled"),
         str_detect(Snake, '\\*', negate = TRUE)) %>% 
  mutate(Country = case_when(
                            Note %in% USA ~ "USA",
                            Note %in% Brazilian_cities ~ "Brazil",
                            Note %in% Unknown ~ "Unknown",
                            str_detect(Note, "Caribbean") ~ "Costa Rica",
                            str_detect(Note, "Pacific") ~ "Costa Rica",
                            str_detect(str_to_lower(Note), "costa rica") ~ "Costa Rica",
                            str_detect(Note, "Venezuelan") ~ "Venezuela",
                            str_detect(Note, "Mexican") ~ "Mexico",
                            str_detect(Note, 'India') ~ "India",
                            Note == "Carribean" ~ "Costa Rica",
                            Note == "Columbia" ~ "Colombia",
                            Note == "Marocco" ~ "Morocco",
                            Note == "Tunesia" ~ "Tunisia",
                            Note == "New Guinea" ~ "Papua New Guinea",
                            Note == "Woodlark island" ~ "Papua New Guinea",
                            Note == "Saibai Island" ~ "Australia",
                            Note == "Java Island" ~ "Indonesia",
                            Note == "Chinese adult" ~ "China",
                            Note == "Burkina faso" ~ "Burkina Faso",
                            Note == "North Africa" ~ "Egypt",
                            TRUE ~ Note)) %>% 
  select(-c("Note", "Sum"))

# Write output clean file -------------------------------------------------
data_clean %>% 
  write_csv('data/02_data_clean.csv')
