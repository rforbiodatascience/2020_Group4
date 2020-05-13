# Clear workspace
rm(list = ls())

library(tidyverse)
source('R/99_proj_func.R')


# Load raw data
data_raw <- read_csv("data/_raw/01_data_load_relative.csv")

# Clean "Note" column containing countries -----------------------------------------
Brazilian_cities <- c("Juazeiro", "Ceara", "Paraiba", "Pernambuco", 
                      "ilha de Itaparica", "Adult and young in Brazil")
USA <- c("Colorado", "Arizona", "Idyllwild", "Loma Linda", "Phelan", 
         "Catalina","Texas", "Kentucky", "Missouri", "Florida", "Kansas", 
         "Colorado", "Ohio", "New Mexico", "Forida")
Unknown <- c("origin unknown", "neonate", "adult")

# Conditions in data set: 
  # rownames in "Snake" column containing "*" indicates transcriptomic data, thus deleted.
  # rownames in "Note" column containing "pooled" indicates pooled venom of different snakes, thus deleted.
data_clean <- data_raw %>% 
  rename_if(is_double, rm_percent) %>% 
  rename(`SP (Serine Proteinase)` = `SP (Serine roteinase)`) %>% 
  filter(!(str_to_lower(Note) == "pooled"),
         str_detect(string = Snake, pattern = '\\*', negate = TRUE)) %>% 
  mutate(Country = case_when(
                            detect_in_list(string = Note, list = USA) ~ "USA",
                            str_to_lower(Note) %in% Unknown ~ "Unknown",
                            Note %in% Brazilian_cities ~ "Brazil",
                            str_detect(string = Note, pattern = "Caribbean") ~ "Costa Rica",
                            str_detect(string = Note, pattern = "Pacific") ~ "Costa Rica",
                            str_detect(string = str_to_lower(Note), pattern = "costa rica") ~ "Costa Rica",
                            str_detect(string = Note, pattern = "Venezuelan") ~ "Venezuela",
                            str_detect(string = Note, pattern = "Mexican") ~ "Mexico",
                            str_detect(string = Note, pattern = 'India') ~ "India",
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
  select(-c(Note, Sum))


# Write output clean file -------------------------------------------------
data_clean %>% 
  write_csv('data/02_data_clean.csv')


# Clean new data ----------------------------------------------------------
data_new <- read_csv("data/_raw/01_data_new.csv")
meta_new <- read_csv('data/_raw/01_meta_new.csv')

# Make new data tidy
data_new <- data_new %>% 
  pivot_longer(cols = -Toxin, names_to = "Snake", values_to = "value") %>% 
  pivot_wider(names_from = Toxin, values_from = value) %>% 
  left_join(meta_new, by = "Snake") %>% 
  replace(list = is.na(.), values = 0)

# Write output clean new file
data_new %>% 
  write_csv('data/02_data_new_clean.csv')
